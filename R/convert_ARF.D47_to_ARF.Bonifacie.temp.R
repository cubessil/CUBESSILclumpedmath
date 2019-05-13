convert_ARF.D47_to_ARF.Bonifacie.temp <- function(D47) {
  round(((0.0422*10^6)/(D47 - 0.1262))^0.5 - 273.15, 1)
}