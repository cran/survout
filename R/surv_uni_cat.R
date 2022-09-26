#'@title Modify the Survival Output for a Categorical Variable.
#'
#'@description This function generates a table with the general survival analysis results, including the number of total patients, the number of sevents, the estimated median, the 1,2,5 year rate, the HR (95 percent confidence interval), the P value, the AIC, and the C index. This function just modifies the output table's format.
#'@param dat a data.frame.
#'@param sevent the status indicator, which is generally 0 = alive, 1 = dead.
#'@param stime the duration of follow-up time in months.
#'@param svar a variable name.
#'@param month a number to get the month-rate of survival.
#'@param medianCI logical value indicating whether the 95 percent confidence interval of projected median survival should be reported.
#'@param y1 logical value indicating whether the 1-year survival rate should be reported.
#'@param y2 logical value indicating whether the 2-year survival rate should be reported.
#'@param y5 logical value indicating whether the 5-year survival rate should be reported.
#'@param report_index logical value indicating if to report the show AIC and C index.
#'@importFrom stats AIC
#'@return a tibble of survival output
#'@examples
#'Dat <- survival::lung
#'surv_uni_cat(Dat, "time", "status", "sex", report_index = TRUE)
#'@export
#'@name surv_uni_cat
#'
surv_uni_cat <- function(dat, stime, sevent, svar, month = 0, medianCI = TRUE, y1 = TRUE, y2 = TRUE, y5 = TRUE, report_index = FALSE){

  D <- dat %>% select(stime = all_of(stime), sevent = all_of(sevent), svar = all_of(svar))
  # D <- na.omit(D)

  # default log-rank test
  fit0 <- survdiff(Surv(stime, sevent) ~ svar, data = D)
  fit1 <- coxph(Surv(stime, sevent) ~ as.factor(svar), data = D)
  fit2 <- survfit(Surv(stime, sevent) ~ svar, data = D, conf.type="log-log")

  # N and sevent N
  result <- tibble(
    `Variable Name` = c(svar, rep(" ", length(fit0$n)-1)),
    `Variable Level` =  D$svar %>% table() %>% names(),
    `No Obs` = fit0$n,
    `No sevent` = fit0$obs)

  ## median survival
  result <- result %>%
    mutate(`Estimated Median` = summary(fit2 )$table[,c('median')] %>% format(.,digits = 3))

  if (medianCI == TRUE){
  result <- result %>% mutate(`Estimated Median (lower 0.95CI)` = summary(fit2 )$table[,c('0.95LCL')] %>% format(.,digits = 3))
  result <- result %>% mutate(`Estimated Median (upper 0.95CI)` = summary(fit2 )$table[,c('0.95UCL')] %>% format(.,digits = 3))
  }

  ##time range
  D %>% group_by(svar) %>% summarise(Value = max(stime,na.rm = TRUE)) -> tmp
  time_min <- tmp$Value %>% min()

  ## survival rate

  if(time_min > 11.999999 & y1 == TRUE){
    result <- result %>% mutate(`1-year rate` = paste(J.digit(summary(fit2, time = 12)$surv * 100 , 0), '%(' ,
                                             J.digit(summary(fit2, time = 12)$lower * 100, 0), '%, ',
                                             J.digit(summary(fit2, time = 12)$upper * 100, 0),  '%)'))
  }
  if(time_min > 23.999999 & y2 == TRUE){
    result <- result %>% mutate(`2-year rate` =  paste(J.digit(summary(fit2, time = 24)$surv * 100 , 0), '%(' ,
                                              J.digit(summary(fit2, time = 24)$lower * 100, 0), '%, ',
                                              J.digit(summary(fit2, time = 24)$upper * 100, 0),  '%)'))
  }
  if(time_min > 59.999999 & y5 == TRUE){
    result <- result %>% mutate(`5-year rate` = paste(J.digit(summary(fit2, time = 60)$surv * 100 , 0), '%(' ,
                                             J.digit(summary(fit2, time = 60)$lower * 100, 0), '%, ',
                                             J.digit(summary(fit2, time = 60)$upper * 100, 0),  '%)'))
  }
  if(month !=0){
    if(month > time_min){
      warning("The month your choose is beyond the time range, choose a smaller one")
    }
    else{
      result <- result %>% mutate(`N-year rate` = paste(J.digit(summary(fit2, time = month)$surv * 100 , 0), '%(' ,
                                               J.digit(summary(fit2, time = month)$lower * 100, 0), '%, ',
                                               J.digit(summary(fit2, time = month)$upper * 100, 0),  '%)'))

    }
  }

  ##HR (95\% Confidence Interval)
  result <- result %>%
    mutate(`HR (95%CI)` = c( "Ref",paste(J.digit(summary(fit1)$conf.int[, 1], 2), '(' ,
                                         J.digit(summary(fit1)$conf.int[, 3], 2), ',' ,
                                         J.digit(summary(fit1)$conf.int[, 4], 2), ')' ))) %>%
    mutate(`P` = c( "", summary(fit1)$coefficients[,5] %>% JS.p))

  ## AIC , C index (D index maybe)
  if(report_index == TRUE){
  blackrow <- rep("",nrow(result)-1)
  result <- result %>%
    mutate(`C Index` =  c(J.digit(summary(fit1)$concordance[1],2),blackrow)) %>%
    mutate(`AIC` = c(J.digit(AIC(fit1),2),blackrow))
  }
  return(result)
}
