#' Calculate clumped isotope temperatures using the composite calibration from Anderson et al., 2021
#' @ D47_90 indicates that the clumped values to use are those in 90degC space, ie not acid digestion corrected

convert_ARF.D47_to_ARF.Anderson.temp <- function(D47_90) {
  round(((0.0391*10^6)/(D47 - 0.154))^0.5 - 273.15, 1)
}