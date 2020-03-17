health = read.table("https://www.stat.washington.edu/marzban/390/winter20/brainhead_dat.txt",header=T)
result <- summary(lm(brain ~ head, data = health))
my_result <- my_lm(brain ~ head, data = health)

test_that("model output correct", {
  for (j in 1:4){
    for (i in 1:2){
      expect_true(abs(my_result[i, j] - result$coefficients[2*(j-1)+i]) < 1.5e-1)
    }
  }
})

test_that("invalid formula", {
  expect_error(my_lm(3, data = health))
})
