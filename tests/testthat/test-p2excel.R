Dat <- survival::lung
results <- surv_uni_cat(Dat, "time", "status", "sex", report_index = TRUE)

test_that("survival outcome works", {
  wb <- createWorkbook()
  wb <- p2excel_pre("survival_results",results,"Table 1. Overall Survival anlaysis",wb)
  expect_equal(length(wb), 1)
})

test_that("survival outcome works", {
  expect_error(p2excel(datastable = results), NA)
})
