test_that("survival outcome works", {
  Dat <- survival::lung
  convars <- c("age","meal.cal")
  catvars <- c("sex")
  results <- surv_multiuni(Dat, "time", "status", catvars, convars,medianCI = TRUE,report_index = TRUE)

  expect_equal(nrow(results), 4)
})

test_that("survival outcome works", {
  Dat <- survival::lung
  results2 <- surv_multi(survival::Surv(time, status) ~ as.factor(sex) + age + meal.cal, data = Dat)
  expect_equal(nrow(results2), 4)
})
