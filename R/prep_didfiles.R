prep_didfiles<- function(did_files){
  rawdata <- did_files %>% iso_get_standards() %>% filter(ratio_name %in% c("R 13C/12C","R 18O/16O")) %>%
    select(file_id, delta_name, delta_value) %>% unique() %>% mutate(delta_name = str_c("ref ",delta_name)) %>%      spread(delta_name, delta_value)
  rawdata <- did_files %>% iso_get_raw_data(include_file_info = c(Analysis, file_datetime, Preparation, `Identifier 1`, `Identifier 2`, MS_integration_time.s, Method, measurement_info)) %>% right_join(rawdata,by="file_id")
  return(rawdata)
}
