#' Fitting Linear Model function
#'
#' This function is used to fit linear models. It can be
#' used to carry out regression, single stratum analysis
#' of variance and analysis of covariance.
#' @param function 	An object of class "formula": a symbolic description
#'   of the model to be fitted.
#' @param data An optional data frame, list or environment
#'   (or object coercible by as.data.frame to a data frame) containing
#'   the variables in the model.
#'
#' @return Numeric represrnting the coefficients and statistics of the fitting
#'
#' @examples
#' my_model <- my_lm(Sepal.Length~Sepal.Width, data = my_iris)
#'
#' @export
my_lm <- function(fnc, data){
  x <- model.matrix(fnc, data)
  modfr <- model.frame(fnc,data)
  y <- model.response(modfr)
  ctn <- ncol(x) + 1
  beta <- solve(t(x) %*% x, t(x)%*% y)
  df <- nrow(x) - ctn
  variance <- (1 / df) * sum((y - x %*% beta)^2)
  my_se = diag(sqrt(variance * (solve(t(x) %*% x))))
  my_tstat = beta / my_se
  library(kableExtra)
  result <- data.frame("Estimate" = beta,
                       "Std. Error" = my_se,
                       "t-value" = my_tstat,
                       "Pr(>|t|)" =  2 * pt(abs(my_tstat),
                                            df, lower.tail = FALSE))
  return (result)
}
