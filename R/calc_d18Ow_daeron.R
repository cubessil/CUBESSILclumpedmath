# This function calculates d18O water values using the water to calcite d18O fractionation factor published in Daeron et al., 2019
# The inputs are the temperatures (in celcius) and the d18O of calcite in the VPDB reference frame

# @param Temp is the clumped-isotope derived temperature in celcius
# @param d18Om is the calcite d18O values in the VPDB reference frame

calc_d18Ow_daeron <- function (d18Om, Temp) {
  A = 17.57
  B = 29.13
  T_c = Temp + 273.15
  d18O_vsmow = (1.03091 * d18Om + 30.91) #convert to VSMOW
  numerator = d18O_vsmow + 1000
  alpha  = exp((A * 1000 / T_c - B)/1000)
  d18Ow = round(((numerator/alpha) - 1000),1)
  return (d18Ow)
}







