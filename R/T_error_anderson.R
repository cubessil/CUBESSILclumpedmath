# This function creates temperature uncertainties from D47 and D47 uncertainties for the Anderson temperature calibration
# This equation was made following the matrix solution for error propagation in Huntington et al., 2009 and the data from Anderson et al., 2021
# The inputs are the temperatures (in celcius), the temp slope, intercept and uncertainties derived by inverting the equation as describe in Huntington et al., 2009
# The temperatures should be in Celcius, and the equations for D47 and D48 are in CDES90 reference frame, from Fiebig et al., 2021

# @param Temp is the temperature in celcius calculated using the Anderson et al., 2021 calibration
# @param D47 is the final ICDES correct D47 value
# @param D47se in the standard error of the D47 value

calc_T_se <- function (Temp, D47, D47se, Tslope = 2.5967012768762e-05, Tslope_err = 5.31291082070059e-07, Tint_err = 2.99538989431556e-07, Cab = 1.56047882297557E-13) 
{
  round(sqrt(abs((((Temp + 273.15)^6)/4) * (Tint_err^2 + D47^2 * Tslope_err^2 + 2 * D47 * Cab + Tslope^2 * D47se^2))), 2)
}