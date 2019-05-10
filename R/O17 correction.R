
#' Correct CO2 data for O17
#'
#' This function takes d45 and d46 and corrects the values for O17 to derive raw d15 and d18 values.
#' The equations used are based on those derived by Jan Kaiser and Thomas Röckmann in
#' "Correction of mass spectrometric isotope ratio measurements for isobaric isotopologues of O2, CO, CO2, N2O and SO2" (Rapid Communications in Mass Spectrometry, 2008, 3997--4008).
#' It is important to note that this function does not currently take site preference into consideration
#'
#' @param data the data frame
#' @param d45 column (in permil)
#' @param d46 column (in permil)
#' @param ref_17R the 17O/16O reference ratio, VSMOW by default (value from #from Brand et al, 2010)
#' @param ref_18R the 18O/16O reference ratio, VSMOW by default (value from #from Brand et al, 2010)
#'
#' @param lambda the mass dependent scaling coefficient for the oxygen isotopes, default 0.528 (value from from Brand et al, 2010)
#' @param d_max the maximum +/- delta value to consider in the root finding [in permil], should not need to change this unless samples are heavily enriched
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @export
#' @return the data frame with corrected 17O
correct_CO2_for_17O <- function (data, d45, d46, ref_17R = 0.000393, ref_13R=0.011180, ref_18R = 0.00208835, lambda = 0.528, d_max = 1000) {
  
  if (missing(d45)) stop("please specify the column that holds the d45 data")
  if (missing(d46)) stop("please specify the columm that holds the d46 data")
  d45_quo <- enquo(d45)
  d46_quo <- enquo(d46)
  
  # fitting parameters
  b <- ref_17R^2 / 2*ref_18R
  c <- ref_17R/ref_13R       #17Rr/13Rr
  d <- b/c
  
  #' expects raw delta values (i.e. NOT in permil)
  d18_root_eq <- function(d18, d45, d46) {
    d17 <- md_scale_delta(d18, lambda, unit = 1)
    return( # note: comment out terms here to see the effect
      d18 - (d46 + d*((2+c)*d46-(2-2*c)*d17-(2+4*c)*d45*(1+d17)+3*c*d17^2)))
  }
  
  #' expects raw delta values (i.e. NOT in permil)
  calc_d18 <- function(d45, d46) {
    na_idx <- is.na(d45) | is.na(d46)
    out <- rep(NA_real_, length(d45))
    out[!na_idx] <- 
      mapply(function(.d45, .d46) {
        uniroot(function(x) d18_root_eq(x, .d45, .d46), lower = -d_max/1000, upper = d_max/1000, tol = 1e-9)$root
      }, d45[!na_idx], d46[!na_idx])
    return(out)
  }
  
  calc_d13 <- function(d18, d45) {
    d17 <- md_scale_delta(d18, lambda, unit = 1)
    return(d45 + 2*c * (d45 - d17))
  }
  
  df <- data %>%
    mutate(
      .d45 = !!d45_quo,
      .d46 = !!d46_quo,
      .d18 = 1000 * calc_d18 (.d45/1000, .d46/1000),
      p.17Ocor = paste0("scaling=", lambda, "; ref 17R=", ref_17R, ", 18R=", ref_18R, ", 13C="),
      d13.raw = 1000 * calc_d13 (.d18/1000, .d45/1000),
      d18.raw = .d18
    )
  
  
  df %>% select(-.d45, -.d46, -.d18) %>% return()
}

#---------- isotope convenience functions (not exported) -----------

# multiplication factor for permil
PERMIL = 1000;

# Calculate the 45R from N2O
calculate_45R <- function (`15R`, `17R`) {
  return (2 * `15R` + `17R`)
}

# Calculate the 46R from N2O
calculate_46R <- function (`15R`, `17R`, `18R`) {
  return (`15R`^2 + `18R` + 2 * `15R` * `17R`)
}

# Convert delta to ratio
delta_to_ratio <- function (delta, ref_ratio, unit = PERMIL) {
  return ((delta/PERMIL + 1) * ref_ratio)
}

# Convert ratio to delta
ratio_to_delta <- function (ratio, ref_ratio, unit = PERMIL) {
  return ( (ratio/ref_ratio - 1) * unit )
}

# Mass scale delta
md_scale_delta <- function(delta, lambda, unit = PERMIL) {
  ((delta/unit + 1) ^ lambda - 1)*unit
}


pick_mi <- function(mi, pattern) {
  x <- str_subset(mi, pattern)
  if (length(x) == 0) NA_character_ else x[1]
}