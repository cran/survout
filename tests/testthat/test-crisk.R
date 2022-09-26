Dat <- MASS::Melanoma
Dat$time <- Dat$time/30.5

test_that("competing risk outcome works", {
  Dat$ulcer <- as.factor(Dat$ulcer)
  con_var <- c("age")
  ord_var <- c("ulcer")
  cat_var <- c("sex")
  results <- crisk_multiuni(Dat, "time", "status",cat_var, con_var, ord_var)
  expect_equal(nrow(results), 5)
})

test_that("competing risk outcome works", {

  con_var <- c("age","thickness")
  cat_var <- c("sex","ulcer")
  results2 <- crisk_multi(Dat, "time", "status", catvars = cat_var, convars =con_var)
  expect_equal(nrow(results2), 4)
})
