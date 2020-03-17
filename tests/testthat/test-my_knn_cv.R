data("my_gapminder")
testdata <- cbind(my_gapminder[, 4], my_gapminder[,6])
my_cl <- my_gapminder$continent
test_that("non-numeric input throws error", {
  expect_error(my_knn_cv(testdata, my_cl, 5, "a"))
  expect_error(my_knn_cv(testdata, my_cl, "g", 5))
  expect_error(my_knn_cv(testdata, my_cl, "d", "a"))
})

test_that("non-dataset input throws error", {
  expect_error(my_knn_cv("qqq", my_cl, 5, 5))
  expect_error(my_knn_cv(testdata, "qqq", 5, 5))
  expect_error(my_knn_cv("qqq", "qqq", 5, 5))
})

test_that("prediction correct", {
  expect_true(my_rf_cv(5) / var(my_gapminder$lifeExp) < 1)
  expect_true(my_rf_cv(10) / var(my_gapminder$lifeExp) < 1)
})

