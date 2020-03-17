#' Random Forest function
#'
#' This function implements Breiman's random forest algorithm for
#'   classification and regression.
#' @param k number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error
#'
#' @examples
#' my_rf_cv(2)
#'
#' @import class magrittr randomForest magrittr dplyr
#'
#' @export
my_rf_cv <- function(k){
  data(my_gapminder)
  n <- nrow(my_gapminder)
  inds <- sample(rep(1:k, length = n))
  cv_err <- rep(NA, k)
  for (i in 1:k) {
    data_train <- my_gapminder[inds != i,]
    data_test <- my_gapminder[inds == i,]
    #building up models using randomForest
    lifeExp_output <- randomForest(lifeExp ~ gdpPercap,
                                  data = data_train, ntree = 100)
    prediction <- predict(lifeExp_output, data_test[, 6])
    #record errors for each iteration
    cv_err[i] <- mean((prediction - data_test$lifeExp)^2)
  }
  return(mean(cv_err))
}
