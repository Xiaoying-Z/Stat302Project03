test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("a"))
})
