# Function perfoms a york regression on a dataset with x,y and errors in both x and y. Equations and regressions statistics following York et al, 2004.
# Calculates the correlation coefficient of the errors (r) explicitly using the cor() function from the x errors and y errors.
# For clumped 47 or 48 data, this will likely result in r values close to 1.
# @param end.df.name is the name you want to give the output dataframe
# @param X is the column of x values for the regression
# @param x.err is the column of standard errors of X
# @param Y is the column of y values for the regression
# @param y.err is the column of standard errors of Y

york.regression <- function (X, x.err, Y, y.err)
{
  error.corr <- cor(x = x.err, y = y.err)
  wX <- 1/(x.err^2)
  wY <- 1/(y.err^2)
  alpha <- sqrt(wX * wY)
  initial.slope <- (coef(lm(Y ~ X))[[2]])
  initial.intercept <- (coef(lm(Y ~ X))[[1]])
  Wi <- (wX * wY/(wX + initial.slope^2 * wY - 2 * initial.slope * error.corr * alpha))
  Ui <- X - (sum(Wi * X)/sum(Wi))
  Vi <- Y - (sum(Wi * Y)/sum(Wi))
  beta.i <- Wi * (Ui/wY + initial.slope * Vi/wX - (initial.slope * Ui + Vi) * error.corr/alpha)
  york.slope <- sum(Wi * beta.i * Vi)/sum(Wi * beta.i * Ui)
  york.intercept <- sum(Wi * Y)/sum(Wi) - york.slope * sum(Wi * X)/sum(Wi)
  york.slope.temp <- numeric(100)
  york.int.temp <- numeric(100)
  for (i in 1:100) {
    Wi <- (wX * wY/(wX + york.slope^2 * wY - 2 * york.slope * error.corr * alpha))
    Ui <- X - (sum(Wi * X)/sum(Wi))
    Vi <- Y - (sum(Wi * Y)/sum(Wi))
    beta.i <- Wi * (Ui/wY + york.slope * Vi/wX - (york.slope * Ui + Vi) * error.corr/alpha)
    york.slope <- sum(Wi * beta.i * Vi)/sum(Wi * beta.i * Ui)
    york.intercept <- sum(Wi * Y)/sum(Wi) - york.slope * sum(Wi * X)/sum(Wi)
    york.slope.temp[i] <- york.slope
    york.int.temp[i] <- york.intercept
  }
  xi <- sum(Wi * X)/sum(Wi) + beta.i
  ui <- xi - sum(Wi * xi)/sum(Wi)
  xbar <- sum(Wi * X, na.rm = TRUE)/sum(Wi, na.rm = TRUE)
  york.slope.err <- sqrt(1/sum(Wi * ui^2))
  york.int.err <- sqrt(1/sum(Wi) + (sum(Wi * xi)/sum(Wi))^2 * york.slope.err^2)
  N <- length(X)
  MSWD <- sum(Wi * (Y - york.slope * X - york.intercept)^2)/(N -
                                                               2)
  Cab <- -xbar * york.slope.err^2
  results <- data.frame(york.slope =york.slope, york.slope.err = york.slope.err, york.intercept = york.intercept, york.int.err= york.int.err, MSWD = MSWD, N = N, Cor.ab = Cab, error.corr = error.corr)
  return(results) #this returns the results df
}
