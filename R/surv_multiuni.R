#'@title Modify the Output for Multiple Uni-variable Survival Analysis
#'
#'@description This function generates a table with the general survival analysis results, including the number of total patients, the number of sevents, the estimated median, the 1,2,5 year rate, the HR (95 percent confidence interval), the P value, the AIC, and the C index. This function just modifies the output table's format.
#'@param dat a dat.frame.
#'@param sevent the status indicator, which is generally 0 = alive, 1 = dead.
#'@param stime the duration of follow-up time in months.
#'@param catvars a vector of categorical variable names.
#'@param convars a vector of continuous variables names.
#'@param medianCI logical value indicating whether the 95 percent confidence interval of projected median survival should be reported.
#'@param y1 logical value indicating whether the 1-year survival rate should be reported.
#'@param y2 logical value indicating whether the 2-year survival rate should be reported.
#'@param y5 logical value indicating whether the 5-year survival rate should be reported.
#'@param report_index logical value indicating if to report the show AIC and C index.
#'@return A tibble of survival output
#'@examples
#'Dat <- survival::lung
#'convars <- c("age","meal.cal")
#'catvars <- c("sex")
#'surv_multiuni(Dat, "time", "status", catvars, convars, medianCI = TRUE)
#'@export
#'@name surv_multiuni
#'

surv_multiuni <- function (dat, stime, sevent, catvars = NULL, convars = NULL, y1 = TRUE, y2 = TRUE, y5 = TRUE, medianCI = FALSE, report_index = FALSE)
{
  rs.all <- tibble::tibble()

  if(!is.null(catvars)){
    for (i in 1:length(catvars)) {
     rs <- surv_uni_cat(dat, stime, sevent, catvars[i], y1 = y1, y2 = y2, y5 = y5, medianCI = medianCI, report_index = report_index)
     rs.all <- bind_rows(rs.all, rs)
     }
  }

  if(!is.null(convars)){
    for (i in 1:length(convars)) {
      rs <- surv_uni_con(dat, stime, sevent, convars[i],report_index = report_index)
      rs.all <- bind_rows(rs.all, rs)
    }

  }

  return(rs.all)
}
