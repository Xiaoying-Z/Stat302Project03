#' Fitting Linear Model function
#'
#' This function raises input to a power
#' @param x Numeric input to be raised to the power of \code{power}
#' @param power Numeric input for the power that \code{x} will be raised to,
#'   default to \code{2}
#'
#' @return Numeric represrnting \code{x} raised to the power of \code{power}
#'
#' @examples
#' my_pow(4)
#' my_pow(4,power = 3)
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
