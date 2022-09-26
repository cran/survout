#'@title Modify the Output for a Multi-variable Competing Risk Analysis .
#'
#'@description Create a table with the general multi-variable competing risk analysis results, including the HR (95 percent CI), P value.
#
#'@param dat a data.frame in which to interpret the variables.
#'@param cevent the status indicator, normally 0=alive, 1=dead.
#'@param csurv this is the follow up time.
#'@param catvars a vector of cat variable names.
#'@param convars a vector of con variable names.
#'@return a tibble of competing risk analysis output.
#
#'@examples
#'Dat <- MASS::Melanoma
#'Dat$time <- Dat$time/30.5
#'con_var <- c("age","thickness")
#'cat_var <- c("sex","ulcer")
#'multi_out <- crisk_multi(Dat, "time", "status", catvars = cat_var, convars =con_var)
#'
#'@export
#'@name crisk_multi
#'

crisk_multi <- function(dat, csurv, cevent,  convars = NULL, catvars = NULL){

  # make a  new dataset
  mvars <- c(convars,catvars)
  varD  <- dat[,c(csurv, cevent,mvars)]
  varD2 <- dat[,c(csurv, cevent,mvars)]

  # identify column type
  if (is.null(convars)){
    convars   <- colnames(varD)[sapply(varD, mode) == "numeric"]
    #con_names <- colnames(NewDat)[sapply(NewDat, class) == "numeric"]
  }

  if(is.null(catvars)){
    catvars   <- colnames(varD)[sapply(varD, mode) == "character"]
    #cat_names <- colnames(NewDat)[sapply(NewDat, class) == "character"]
  }

  # for categorical data, will condiser groups < 10 without event as NA
  n <- length(catvars)
  for(i in catvars){
    varD[,i]<- ingorfactor_c(varD[,i], dat[,cevent])
    # for chat variable   will chose the first level as referece
    # and make a matrix for modeling

    tmpv <- factor2ind(varD[,i])
    colnames(tmpv) <- gsub("varD[, i]", i, colnames(tmpv), fixed = TRUE)

    varD <- cbind(varD, tmpv)
    varD <- varD[,-which(names(varD) == i)]

  }
  blackrow <- rep("")
  fit <- cmprsk::crr(varD[,1], varD[,2], varD[,-c(1,2)])
  S <- summary(fit)
  HR <- J.digit(S$coef[, c(2)], 2)
  LCL <- J.digit(S$conf.int[, c(3)], 2)
  UCL <- J.digit(S$conf.int[, c(4)], 2)
  HR95CI <- paste(HR, "(", LCL, ",", UCL, ")")
  result <-
    tibble(
      `Variable` = colnames(varD[,-c(1,2)]),
      `HR (95% CI)` = HR95CI,
      `P` = JS.p(S$coef[, c(5)])
    )
  result
}




