# function that propogates errors for Y that includes the uncertainty on the linear regression used to calculate Y, along with uncertainties for measurements of values for X and Y. Note, these "x" and "y" data won't be the data used to regress the line, but will be the data that get converted to corrected values via the regression. For example, this will be used to calculate D47 uncertainties that include the uncertainty of the heated gas line used to correct clumped isotope data as well as the analytical uncertainties on d47 and uncorrected D47.
# @param df is the dataframe that contains all of the measurements to propogate uncertainty for and will be returned from the function with columns for the external errors
# @param x is the data that would plug into the x term in the regression, in the df dataframe
# @param x.sterr is the analytical uncertainty, as standard error of the y values to propogate with the line uncertainty
# @param y.sterr is the analytical uncertainty, as standard error of the y values to propogate with the line uncertainty
# @param slope.err is the error on the slope
# @param intercept.err is the error on the intercept
# @param correlation.line.parameters is the correlation coefficient of the slope and intercept. If the regression is a york regression, this is the Cor.ab output
calc_prop_errors_lines <- function(df, X, x.sterr, y.sterr, slope, slope.err, intercept.err, correlation.line.parameters)
{df$Y.prop.err <-
  sqrt(
    (intercept.err^2 + X * correlation.line.parameters) + 
      X * (correlation.line.parameters + X * slope.err^2) +
      slope^2 * x.sterr^2 + 
      y.sterr^2)
return(df)
}