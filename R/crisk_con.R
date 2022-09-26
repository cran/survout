#'@title Modify the Output for Uni-variable and Multi-variable Competing Risk Analysis (Continuous and Ordinal Only)
#'
#'@description This function generates a table of competing risk analysis result with HR (95\% Confidence Interval),P value.
#'@param cevent the status indicator, which is generally 0 = alive, 1 = event, 2 = other event
#'@param csurv the duration of follow-up time in months.
#'@param cvars a matrix, which has the variables' values (continuous and ordinal only)
#'@param gnames a text vector, which are the names of the variables.
#'@return a dataframe containing HRs (with 95\% Confidence Intervals) and P values
#'@examples
#'Dat <- MASS::Melanoma
#'Dat$time <- Dat$time/30.5
#'X      <- cbind(Dat$age, Dat$thickness)
#'Gnames <- c('age', 'thickness')
#'output <- crisk_con(Dat$time, Dat$status, X, Gnames)
#'
#'@export
#'@name crisk_con
#'
crisk_con <- function (csurv, cevent, cvars, gnames){
  fit <- cmprsk::crr(csurv, cevent, cvars)
  S   <- summary(fit)
  HR  <- J.digit(S$coef[,c(2)], 2)
  LCL <- J.digit(S$conf.int[,c(3)], 2)
  UCL <- J.digit(S$conf.int[,c(4)], 2)
  HR95CI <- paste(HR,'(',LCL,',',UCL,')')
  p   <- JS.p(S$coef[,c(5)])

  result <- tibble(
    `Variable` = gnames,
    `HR (95% CI)` = HR95CI,
    `P` = p
  )
  return(result)
}

