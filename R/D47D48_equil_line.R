# This function creates a dataframe of D47 and D48 values that correspond to a given range of temperatures
# These temperatures can be used to construct a line of dual equalibrium in a plot of D47 vs D48. 
# The function just needs the minimum and maximum temperatures from which to calculate D47 and D48, and will create list of temperatures in the defined range, in steps of 1°C. 
# The temperatures should be in Celcius, and the equations for D47 and D48 are in CDES90 reference frame, from Fiebig et al., 2021

# @param Tmin_C defines the minimum temperature for the range
# @param Tmax_C defines the maximum temperature for the range

D47D48equil_line <- function(Tmin_C, Tmax_C)
{
  D47_D48 <- data.frame(T_C = seq(from = Tmin_C, to = Tmax_C, by = 1))
  D47_D48 <- mutate(D47_D48, 
                    D47 = 1.038* (-5.897* 1/(T_C + 273.15) - 3.521* 10^3*1/(T_C + 273.15)^2 + 2.391*10^7*1/(T_C + 273.15)^3 - 3.541* 10^9 * 1/(T_C + 273.15)^4) + 0.1856,
                    D48 = 1.028* (6.002* 1/(T_C + 273.15) - 1.299* 10^4* 1/(T_C + 273.15)^2 + 8.996* 10^6* 1/(T_C + 273.15)^3 - 7.423* 10^8* 1/(T_C + 273.15)^4) + 0.1245)
}