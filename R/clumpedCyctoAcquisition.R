#creates average acquistion values from cycle data

clumpedCyctoAcquisition<-  function(df) {
  df %>%
    group_by(file_id) %>%
    summarise(
      Analysis = floor(Analysis[1]),
      Identifier1 =`Identifier 1`[1],
      Identifier2 = `Identifier 2`[1],
      file_datetime =file_datetime[1],
      mass=as.numeric(`Identifier 2`[1]),
      Preparation = Preparation[1],
      runinfo = runinfo [1],
      Comment = Comment,
      Donotuse = Donotuse[1],
      Yield = Yield[1],
      Method = Method[1],
      num.cyc = n(),
      d45.stdev= sd(d45),
      d45 = mean(d45),
      d46.stdev = sd(d46),
      d46 = mean(d46),
      d47.stdev= sd(d47),
      d47 = mean(d47),
      d48.stdev = sd(d48),
      d48 = mean(d48),
      d49.stdev = sd(d49),
      d49 = mean(d49),
      D47.stdev = sd(D47full),
      D47full= mean(D47full),
      D48.stdev = sd(D48full),
      D48full= mean(D48full),
      D49.stdev = sd(D49full),
      D49full= mean(D49full),
      d13C.stdev= sd(d13C),
      d13C =  mean(d13C),
      d18O.stdev= sd(d18O),
      d18O.VPDB.min = ((((mean(d18O)-30.86)/1.03086)+1000)/1.00821)-1000,
      #splitting VSMOW to VPDB convertion then acid fractionation correction 
      d18O = mean(d18O),
      d18O.ref = `ref d 18O/16O`[1],
      d13C.ref = `ref d 13C/12C`[1],
      PB= mean(PB),
      LeftPressure= mean(LeftPressure),
      RightPressure = mean(LeftPressure)
    ) %>%
    mutate(
      new_sample =  Preparation != c("", head(Preparation, -1))  | `Identifier1` != c("", head(`Identifier1`,-1)), batch.Aq = cumsum(new_sample)
    ) %>%
    group_by(batch.Aq) %>% 
    mutate(id = row_number())
}
