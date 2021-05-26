#' Anderson Clumped Isotope Calibration
#' 
#' Calculates clumped isotope temperatures using the composite calibration from Anderson et al., 2021
#'
#' @param D47_90 numeric vector of D47 values to use to calculate temperatures. This calibrations needs D47 values that are 90degC values, ie not acid digestion corrected
#'
#' @export

convert_ARF.D47_to_ARF.Anderson.temp <- function(D47_90) {
  round(((0.0391*10^6)/(D47 - 0.154))^0.5 - 273.15, 1)
}
