#'@title Modify the Output for Uni-variable and Multi-variable Competing Risk Analysis (Ordinal Only)
#'
#'@description This function generates a table of competing risk analysis result with number of patients, number of event, number of competing event,
# 1-year, 2-year, 5-year, n- year incidence rate, hazard ratio, and p-value.
#
#'@param cevent the status indicator, which is generally 0 = alive, 1 = event, 2 = other event
#'@param csurv the duration of follow-up time in months.
#'@param cvars a vector, which has the variable's values (ordinal only)
#'@param gnames a text string, which is the name of the variable.
#'@param month a number to get the month-rate of competing risk.
#'@param y1 logical value indicating whether the 1-year competing risk rate should be reported.
#'@param y2 logical value indicating whether the 2-year competing risk rate should be reported.
#'@param y5 logical value indicating whether the 5-year competing risk rate should be reported.
#'@return a tibble of competing risk analysis output.
#'@examples
#'Dat <- MASS::Melanoma
#'Dat$time <- Dat$time/30.5
#'output <- crisk_ord(Dat$time, Dat$status, as.factor(Dat$year), "year")
#'
#'@export
#'@name crisk_ord
#'

crisk_ord <- function (csurv, cevent, cvars, gnames, month = 0, y1 = TRUE, y2 = TRUE, y5 = TRUE ){

  # model
  fit <- cmprsk::crr(csurv, cevent, cvars)
  fit2 <- cmprsk::cuminc(csurv, cevent, cvars)

  nlevel <- levels(cvars) %>% length

  # n and event
  result <- tibble(
    `Variable` =  c(gnames,rep("", nlevel - 1)),
    `Varaible Levels` = levels(cvars),
    `N` = table(cvars),
    `Number of event` = table(cvars, cevent)[,2],
    `Number of competing event` = table(cvars, cevent)[,3]
  )

  ## time range (the min follow up time of these levels)
  tmp.df <- cbind(csurv, cvars) %>% tibble::as_tibble()
  complete_df <- tmp.df[complete.cases(tmp.df),]
  complete_df %>% group_by(cvars) %>% summarise(Value = max(csurv,na.rm = TRUE)) -> .tmp
  time_min <- .tmp$Value %>% min()


  #incident rate:

  n <- result %>% nrow()

  if(time_min > 11.999999 & y1 == TRUE){
    cc <- cmprsk::timepoints(fit2, times = 12)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`1-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }


  if(time_min > 23.999999 & y2 == TRUE){
    cc <- cmprsk::timepoints(fit2, times = 24)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`2-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(time_min > 59.999999 & y5 == TRUE){
    cc <- cmprsk::timepoints(fit2, times = 60)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`5-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(month !=0){
    if(month > time_min){
      warning("The month your choose is beyond the time range, choose a smaller one")
    }
    else{
      cc <- cmprsk::timepoints(fit2, times = month)
      est <- cc$est[1:n] %>% round(2)
      car <- cc$var[1:n] %>% round(2)

      result <- result %>%
        mutate(`N-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )

    }}

  blackrow <- rep("",nrow(result)-1)
  ##HR (95\% Confidence Interval)
  S <- summary(fit)
  HR <- J.digit(S$coef[, c(2)], 2)
  LCL <- J.digit(S$conf.int[, c(3)], 2)
  UCL <- J.digit(S$conf.int[, c(4)], 2)
  HR95CI <- paste(HR, "(", LCL, ",", UCL, ")")
  result <-
    result %>%
    mutate(`HR (95% CI)` = c(HR95CI, blackrow)) %>%
    mutate(`P` = c(JS.p(S$coef[, c(5)]), blackrow))

  ## AIC, Cindex, (D index maybe)

  return(result)
}
