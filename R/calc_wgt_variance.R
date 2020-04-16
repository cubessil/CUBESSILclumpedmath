#' Calculate weighted variance for small samples sets; used for calculate variance of replicate analyses D47 samples

calc_wgt_variance <- function(x, wt, na.rm = TRUE)
{
  if (na.rm) {
    ind <- is.na(x) | is.na(wt)
    x <- x[!ind]
    wt <- wt[!ind]
  }
  wgt <- (1/wt)
  sum.wt <- sum(wgt)
  wd <- (sum(x^2*wgt)*sum.wt-(sum(wgt*x)^2))/(sum.wt^2-sum(wgt^2))
}
