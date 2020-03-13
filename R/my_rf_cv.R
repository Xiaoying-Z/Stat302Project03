#' Random Forest function
#'
#' This function raises input to a power
#' @param x Numeric input to be raised to the power of \code{power}
#' @param power Numeric input for the power that \code{x} will be raised to,
#'   default to \code{2}
#'
#' @return Numeric represrnting \code{x} raised to the power of \code{power}
#'
#' @examples
#' my_rf_cv(5)
#'
#' @import class magrittr randomForest magrittr dplyr
#'
#' @export
my_rf_cv <- function(k){
  n <- nrow(iris)
  inds <- sample(rep(1:k, length = n))
  cv_err <- rep(NA, k)
  for (i in 1:k) {
    data_train <- iris[inds != i,]
    data_test <- iris[inds == i,]
    #building up models using randomForest
    length_output <- randomForest(Sepal.Length ~ Sepal.Width +
                                    Petal.Length + Petal.Width,
                                  data = data_train, ntree = 100)
    prediction <- predict(length_output, data_test[, -1])
    #record errors for each iteration
    cv_err[i] <- mean((prediction - data_test$Sepal.Length)^2)
  }
  return(mean(cv_err))
}
