set.seed(333)
x <- rnorm(100)
my_result01 <- my_t.test(x, "two.sided", 0)
result01 <- t.test(x)

y <- rbinom(100, 1000, 0.3)
result02 <- t.test(y, alternative = "less", mu = 20)
my_result02 <- my_t.test(y, alternative = "less", mu = 20)

z <- rbinom(100, 1000, 2)
result03 <- t.test(y, alternative = "greater", mu = 4)
my_result03 <- my_t.test(y, alternative = "greater", mu = 4)

test_that("test output correct", {
  expect_true(abs(my_result01$test_stat - result01$statistic) < 1e-3)
  expect_true(abs(my_result01$`p-value` - result01$p.value) < 1e-3)
  expect_equal(my_result01$df, result01$parameter[[1]])

  expect_true(abs(my_result02$test_stat - result02$statistic) < 1e-3)
  expect_true(abs(my_result02$`p-value` - result02$p.value) < 1e-3)
  expect_equal(my_result02$df, result02$parameter[[1]])

  expect_true(abs(my_result03$test_stat - result03$statistic) < 1e-3)
  expect_true(abs(my_result03$`p-value` - result03$p.value) < 1e-3)
  expect_equal(my_result03$df, result03$parameter[[1]])
})

test_that("non-numeric input throws error", {
  expect_error(my_t.test("a string", alternative = "greater", mu = 4))
  expect_error(my_t.test(x, alternative = "greater", mu = "a string"))
})

test_that("invalid alternative throws error", {
  expect_error(my_t.test(x, alternative = 3, mu = 4))
  expect_error(my_t.test(x, alternative = "stat302", mu = 4))
})
