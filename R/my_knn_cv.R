#' K-nearest Neighbors function
#'
#' This function uses k-nearest neighbour classification for test set from training set.
#' @param train the training data set
#' @param cl factor of true classifications of training set
#' @param k_nn number of neighbours considered
#' @param k_cv integer representing the number of folds
#' @keywords prediction
#'
#' @return a list with a vector of the predicted class and
#'   a numeric with the cross-validation misclassification rate
#'   a numeric with the training misclassification rate
#'
#' @examples
#' my_knn_cv(my_iris[, -5], my_iris$Species, 1, 5)
#'
#' @import class magrittr randomForest magrittr dplyr
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  n <- nrow(train)
  inds <- sample(rep(1:k_cv, length = n))
  cv_err <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    #splits the data and true species into training and testing sets
    data_train <- train[inds != i,]
    data_test <- train[inds == i,]
    cl_train <- cl[inds != i]
    cl_test <- cl[inds == i]
    class_output <- knn(train = data_train,
                        test = data_test,
                        cl = cl_train, k = k_nn)
    #records errors of the predictions
    cv_err[i] <- sum(class_output != cl_test)/length(cl_test)
  }
  output <- knn(train = train, test = train, cl = cl, k = k_nn)
  train_err <- sum(output != cl)/length(cl)
  my_output <- list(class_output, mean(cv_err), train_err)
  return(my_output)
}
