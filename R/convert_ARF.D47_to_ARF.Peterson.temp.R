convert_ARF.D47_to_ARF.Peterson.temp <- function(D47) {
  round(((0.0383*10^6)/(D47 - 0.258))^0.5 - 273.15, 1)
}