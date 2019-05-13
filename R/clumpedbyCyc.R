clumpedbyCyc <- function (rawdata, lambda = 0.528){
  raw_data_w_measurement_info <-
    rawdata %>%
    # nest to run the operations on the measurement info only once for each file
    nest(-file_id) %>%
    # find the relevant measurement info
    mutate(
      # pull out the measurement info from the nested data (the same for each raw data row so only need to look at 1)
      mi = map(data, ~.x$measurement_info[[1]]),
      # pick the relevant entries in the measurement info
      mi_select = map(mi,
                      ~data_frame(
                        Yield = pick_mi(.x, "left side"),
                        lp = pick_mi(.x, "l_p"),
                        rp = pick_mi(.x, "r_p"),
                        pc = pick_mi(.x, "PC"),
                        bgrd = pick_mi(.x, "Background")
                      )
      )
    ) %>%
    # unnest the relevant measurement info
    unnest(mi_select) %>%
    # extract combined values into multiple columns
    extract(lp, into = c("LeftPressure", "LeftBellows"),
            regex = "mBar l ([0-9.]+)   l_p ([0-9.]+)") %>%
    extract(rp, into = c("RightPressure", "RightBellows"),
            regex = "mBar r ([0-9.]+)   r_p ([0-9.]+)") %>%
    extract(pc, into = c("PC"),
            regex = "PC \\[([0-9.-]+)") %>%
    extract(bgrd, into = str_c("v", c("44", "45", "46", "47", "47.5", "48", "49"), ".background"),
            regex = str_c(rep("([0-9.-]+) mV,?", 7), collapse = "")) %>%
    # turn into numbers
    mutate_at(
      vars(Yield, LeftPressure, LeftBellows, RightPressure, RightBellows, PC, ends_with("background")),
      funs(parse_number)
    ) %>%
    # discard unnecessary columns
    select(-mi) %>%
    # unnest all data again
    unnest(data)

  isostandards <- did_files %>% iso_get_standards_info() %>% select(file_id, delta_name, delta_value) %>% mutate(delta_name = str_c("ref ", delta_name)) %>% spread(delta_name, delta_value)

  ref_pre <- filter(raw_data_w_measurement_info, type == "standard") %>%
    select(-type, -Analysis) %>%
    # prefix column names with pre
    { setNames(., str_c("pre_", names(.))) }

  ref_post <- filter(raw_data_w_measurement_info, type == "standard") %>%
    select(-type, -Analysis) %>%
    # prefix column names with post
    { setNames(., str_c("post_", names(.))) }


  # now combine the samples with their respective pre and post cycle standards (i.e. to bracket them)
  combined_data <-
    raw_data_w_measurement_info %>%
    filter(., type == "sample") %>%
    mutate(pre_ref = cycle - 1, post_ref = cycle) %>%
    left_join(ref_pre, by = c("file_id" = "pre_file_id", "pre_ref" = "pre_cycle")) %>%
    left_join(ref_post, by = c("file_id" = "post_file_id", "post_ref" = "post_cycle")) %>%
    mutate(
      Analysis = parse_number(str_c(Analysis, ".", cycle)),
      `var V 54.mV` = v54.mV,
      `var V 44.mV` = v44.mV,
      `var R sample 45CO2/44CO2` = (v45.mV/v44.mV),
      `var R standard 45CO2/44CO2` = (pre_v45.mV/pre_v44.mV + post_v45.mV/post_v44.mV)/2,
      `var d 45CO2/44CO2` = (`var R sample 45CO2/44CO2` / `var R standard 45CO2/44CO2` - 1) * 1000,
      `r45o44` = v45.mV / v44.mV,
      `r46o44` = v46.mV / v44.mV,
      `r47o44` = v47.mV / v44.mV,
      `r48o44` = v48.mV / v44.mV,
      `r49o44` = v49.mV / v44.mV,
      `pre_r45o44` = pre_v45.mV / pre_v44.mV,
      `pre_r46o44` = pre_v46.mV / pre_v44.mV,
      `pre_r47o44` = pre_v47.mV / pre_v44.mV,
      `pre_r48o44` = pre_v48.mV / pre_v44.mV,
      `pre_r49o44` = pre_v49.mV / pre_v44.mV,
      `post_r45o44` = post_v45.mV / post_v44.mV,
      `post_r46o44` = post_v46.mV / post_v44.mV,
      `post_r47o44` = post_v47.mV / post_v44.mV,
      `post_r48o44` = post_v48.mV / post_v44.mV,
      `post_r49o44` = post_v49.mV / post_v44.mV,
      `d45` = ((r45o44 /((pre_r45o44+post_r45o44)/2)-1)*1000),
      `d46` = ((r46o44 /((pre_r46o44+post_r46o44)/2)-1)*1000),
      `d47` = ((r47o44 /((pre_r47o44+post_r47o44)/2)-1)*1000),
      `d48` = ((r48o44/((pre_r48o44+post_r48o44)/2)-1)*1000),
      `d49` = ((r49o44/((pre_r49o44+post_r49o44)/2)-1)*1000),
      PB = v44.mV - (pre_v44.mV+post_v44.mV)/2
    )
  combined_data <- left_join(combined_data,isostandards, "file_id")
  combined_data <- correct_CO2_for_17O(combined_data,d45, d46, lambda = 0.528)
  combined_data <- mutate(combined_data, d13C = d13.raw+`ref d 13C/12C`)#11.103
  combined_data <- mutate(combined_data, d18O = d18.raw+`ref d 18O/16O`)#35.775

  R13VPDB <-	0.011237
  R18VSMOW <-	0.002005
  R17VSMOW <-	0.000380
  R47ZeroCO2 <-	4.65908E-05

  combined_data <- combined_data %>% mutate(
    refR13 = ((`ref d 13C/12C`/1000)+1)*R13VPDB,
    refR18 = ((`ref d 18O/16O`/1000)+1)*R18VSMOW,
    refR17 = ((refR18/R18VSMOW)^lambda)*R17VSMOW,
    ref12C = 1/(1+refR13),
    ref13C = 1-ref12C,
    ref16O = 1/(1+refR18+refR17),
    ref18O = ref16O*refR18,
    ref17O = ref16O*refR17,
    sampleR13 = ((d13C/1000)+1)*R13VPDB,
    sampleR18 = ((d18O/1000)+1)*R18VSMOW,
    sampleR17 = ((sampleR18/R18VSMOW)^lambda)*R17VSMOW,
    sample12C = 1/(1+sampleR13),
    sample13C = 1-sample12C,
    sample16O = 1/(1+sampleR18+sampleR17),
    sample18O = sample16O*sampleR18,
    sample17O = sample16O*sampleR17,
    #blue box AV-AX
    refmass12.16.16 = ref12C*ref16O*ref16O,
    refmass12.16.17 = ref12C*ref16O*ref17O*2,
    refmass13.16.16 = ref13C*ref16O*ref16O,
    refmass12.16.18 = ref12C*ref16O*ref18O*2,
    refmass12.17.17 = ref12C*ref17O*ref17O,
    refmass13.17.16 = ref13C*ref17O*ref16O*2,
    refmass12.17.18 = ref12C*ref17O*ref18O*2,
    refmass13.16.18 = ref13C*ref16O*ref18O*2,
    refmass13.17.17 = ref13C*ref17O*ref17O,
    refmass12.18.18 = ref12C*ref18O*ref18O,
    refmass13.17.18 = ref13C*ref17O*ref18O*2,
    refmass13.18.18 = ref13C*ref18O*ref18O,

    samplemass12.16.16 = sample12C*sample16O*sample16O,
    samplemass12.16.17 = sample12C*sample16O*sample17O*2,
    samplemass13.16.16 = sample13C*sample16O*sample16O,
    samplemass12.16.18 = sample12C*sample16O*sample18O*2,
    samplemass12.17.17 = sample12C*sample17O*sample17O,
    samplemass13.17.16 = sample13C*sample17O*sample16O*2,
    samplemass12.17.18 = sample12C*sample17O*sample18O*2,
    samplemass13.16.18 = sample13C*sample16O*sample18O*2,
    samplemass13.17.17 = sample13C*sample17O*sample17O,
    samplemass12.18.18 = sample12C*sample18O*sample18O,
    samplemass13.17.18 = sample13C*sample17O*sample18O*2,
    samplemass13.18.18 = sample13C*sample18O*sample18O,
    #blue box AZ-BB
    ref44 = refmass12.16.16,
    ref45 = refmass12.16.17 + refmass13.16.16,
    ref46 = refmass12.16.18 + refmass12.17.17 + refmass13.17.16,
    ref47 = refmass12.17.18 + refmass13.16.18 + refmass13.17.17,
    ref48 = refmass12.18.18 + refmass13.17.18,
    ref49 = refmass13.18.18,

    sample44 = samplemass12.16.16,
    sample45 = samplemass12.16.17 + samplemass13.16.16,
    sample46 = samplemass12.16.18 + samplemass12.17.17 + samplemass13.17.16,
    sample47 = samplemass12.17.18 + samplemass13.16.18 + samplemass13.17.17,
    sample48 = samplemass12.18.18 + samplemass13.17.18,
    sample49 = samplemass13.18.18,

    refR45 = ref45/ref44,
    refR46 = ref46/ref44,
    refR47 = ref47/ref44,
    refR48 = ref48/ref44,
    refR49 = ref49/ref44,

    sampleR45 = sample45/sample44,
    sampleR46 = sample46/sample44,
    sampleR47 = sample47/sample44,
    sampleR48 = sample48/sample44,
    sampleR49 = sample49/sample44,

    # # gray box
    R45 = ((d45/1000)+1)*refR45,
    R46 = ((d46/1000)+1)*refR46,
    R47 = ((d47/1000)+1)*refR47,
    R48 = ((d48/1000)+1)*refR48,
    R49 = ((d49/1000)+1)*refR49,

    # #Yellow box
    D45 = ((R45/sampleR45)-1)*1000,
    D46 = ((R46/sampleR46)-1)*1000,
    D47 = ((R47/sampleR47)-1)*1000,
    D48 = ((R48/sampleR48)-1)*1000,
    D49 = ((R49/sampleR49)-1)*1000,

    D47full = D47-D46-D45,
    D48full = D48-D46-D46,
    D49full = D49-D46-D46-D45,
    runinfo ="",
    Donotuse = FALSE) %>%
  group_by(file_id) %>% add_tally() %>%
    mutate(
      d45.stdev.Aq= sd(d45),
      d45.Aq = mean(d45),
      d46.stdev.Aq= sd(d46),
      d46.Aq = mean(d46),
      d47.stdev.Aq= sd(d47),
      d47.Aq = mean(d47),
      d48.stdev.Aq = sd(d48),
      d48.Aq = mean(d48),
      d49.stdev.Aq = sd(d49),
      d49.Aq = mean(d49),
      D47.stdev.Aq = sd(D47full),
      D47full.Aq= mean(D47full),
      D48.stdev.Aq = sd(D48full),
      D48full.Aq= mean(D48full),
      D49.stdev.Aq = sd(D49full),
      D49full.Aq= mean(D49full),
      d13C.stdev.Aq= sd(d13C),
      d13C.Aq =  mean(d13C),
      d18O.stdev.Aq= sd(d18O),
      d18O.VPDB.min.Aq = ((((mean(d18O)-30.86)/1.03086)+1000)/1.00821)-1000,
      d18O.Aq = mean(d18O),
      d18O.ref.Aq = `ref d 18O/16O`[1],
      d13C.ref.Aq = `ref d 13C/12C`[1],
      PB.Aq= mean(PB),
      LeftPressure.Aq= mean(LeftPressure),
      RightPressure.Aq = mean(LeftPressure),
      numberofcyc = n
    ) %>%
    ungroup %>%
    arrange(Analysis) %>%
    mutate(
      new_sample =  Preparation != c("", head(Preparation, -1))  | `Identifier 1` != c("", head(`Identifier 1`,-1)),
      batch = cumsum(new_sample)) %>%
    group_by(batch) %>% mutate(id = row_number()) %>% mutate(num.Aq = n()/numberofcyc)

}
