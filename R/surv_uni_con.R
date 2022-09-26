#'@title Modify the Survival Output for a Continuous Variable.
#'
#'@description This function generates a table with the general survival analysis results, including the number of total patients, the number of events, the P value, the AIC, and the C index. This function just modifies the output table's format.
#'@param dat a data.frame.
#'@param sevent the status indicator, which is generally 0 = alive, 1 = dead.
#'@param stime the duration of follow-up time in months.
#'@param svar a variable name.
#'@param report_index logical value indicating if to report the show AIC and C index.
#'@return a tibble of survival results.
#'@import survival
#'@import dplyr
#'@examples
#'Dat <- survival::lung
#'surv_uni_con(Dat, "time", "status", "age",report_index = TRUE)
#'@export
#'@name surv_uni_con
#'
surv_uni_con <- function(dat, stime, sevent, svar, report_index = FALSE){

  D1 <- dat %>% select(stime = all_of(stime), sevent = all_of(sevent), svar = all_of(svar))

  # cox model
  fit1 <-  coxph(Surv(stime, sevent) ~ svar, data = D1)

  # assign name to result tibble
  l <- list("Variable Name" = svar)
  result <- as_tibble(l)

  ##HR (95\% Confidence Interval)
  result <-
    result %>%
    mutate(`HR (95%CI)` = c(paste(J.digit(summary(fit1)$conf.int[, 1], 2), '(' ,
                                  J.digit(summary(fit1)$conf.int[, 3], 2), ',' ,
                                  J.digit(summary(fit1)$conf.int[, 4], 2), ')' ))) %>%
    mutate(`P` = c(summary(fit1)$coefficients[,5] %>% JS.p))

  ## AIC , C index
  if(report_index == TRUE){
  blackrow = rep("",nrow(result)-1)
  result <-
    result %>%
    mutate(`C Index` =  c(J.digit(summary(fit1)$concordance[1],2),blackrow)) %>%
    mutate(`AIC` = c(J.digit(stats::AIC(fit1),2),blackrow))
  }

  return(result)
}
