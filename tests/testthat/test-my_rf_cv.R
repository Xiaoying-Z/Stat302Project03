test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("a"))
})

test_that("prediction correct", {
  expect_true(my_rf_cv(5) / var(my_gapminder$lifeExp) < 1)
  expect_true(my_rf_cv(10) / var(my_gapminder$lifeExp) < 1)
})
