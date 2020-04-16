#' Calculate weighted means for small samples sets; used for averaging replicate D47 analyses of samples together

calc_wgt_means <- function(x, wt, na.rm = TRUE)
{
  if (na.rm) {
    ind <- is.na(x) | is.na(wt)
    x <- x[!ind]
    wt <- wt[!ind]
  }
  wgt <- (1/wt)
  sum.wt <- sum(wgt)
  wm <- sum(x*wgt)/sum.wt
}
