# This function calculates undertainty of d18O water values, based on the water to calcite d18O fractionation factor published in Daeron et al., 2019
# The inputs are the temperatures and associated uncertainties (in celcius) and d18O of calcite and uncertainties in the VPDB reference frame

# @param Temp is the clumped-isotope derived temperature in celcius 
# @param Temp is the uncertainty on the clumped-isotope derived temperature
# @param d18Om is the calcite d18O values in the VPDB reference frame
# @param d18Om is uncertainty of the calcite d18O values in the VPDB reference frame

calc_d18Owse_daeron <- function (d18Om, d18Omse, Temp, Tempse) {
  A = 17.57
  B = 29.13
  T_c = Temp + 273.15
  d18O_vsmow = (1.03092 * d18Om + 30.92)
  
  first_part = (((A *((d18O_vsmow) + 1000) * exp(B/1000)) / (exp(A /T_c) * T_c^2) * Tempse)^2)  
  second_part = ((exp(B/1000) / exp(A/(T_c)) * d18Omse)^2)
  d18Owse = round(sqrt(first_part +  second_part),1)
  
  return(d18Owse)
}