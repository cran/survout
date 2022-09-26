#'@title Modify the Output for Multiple Uni-variable Competing Risk Analysis .
#'
#'@description This function generates a table of competing risk analysis result with number of patients, number of event, number of competing event,
# 1-year, 2-year, 5-year, n- year incidence rate, hazard ratio, and p-value.
#'@param dat a data.frame in which to interpret the variables.
#'@param cevent the status indicator, normally 0=alive, 1=dead.
#'@param csurv this is the follow up time.
#'@param catvars a vector of cat variable names.
#'@param convars a vector of con variable names.
#'@param ordvars a vector of ordinal variable names.
#'@param month a number to get the month-rate of competing risk.
#'@param y1 logical value indicating whether the 1-year competing risk rate should be reported.
#'@param y2 logical value indicating whether the 2-year competing risk rate should be reported.
#'@param y5 logical value indicating whether the 5-year competing risk rate should be reported.
#'@return a tibble of competing risk analysis output.
#'@examples
#'Dat <- MASS::Melanoma
#'Dat$time <- Dat$time/30.5
#'Dat$ulcer <- as.factor(Dat$ulcer)
#'con_var <- c("age")
#'ord_var <- c("ulcer")
#'cat_var <- c("sex")
#'uni_out <- crisk_multiuni(Dat, "time", "status",cat_var, con_var, ord_var)
#'@export
#'@name crisk_multiuni
#'
crisk_multiuni <- function (dat, csurv, cevent, catvars = NULL, convars = NULL, ordvars = NULL, y1 = TRUE, y2 = TRUE, y5 = TRUE, month = 0){
  # create output tibble
  rs.all <- tibble::tibble()

  # transform name to var
  csurv <- dat[[csurv]]
  cevent <-dat[[cevent]]

  if(!is.null(catvars)){
    for (i in 1:length(catvars)) {
      cvars <- as.matrix(dat[catvars[i]])
      rs <- crisk_cat(csurv, cevent, cvars, gnames = catvars[i],  y1 = y1, y2 = y2, y5 = y5, month = month)
      rs.all <- bind_rows(rs.all, rs)
    }
  }

  if(!is.null(ordvars)){
    for (i in 1:length(ordvars)) {
      cvars <- dat[[ordvars[i]]]
      rs <- crisk_ord(csurv, cevent, cvars, gnames = ordvars[i], y1 = y1, y2 = y2, y5 = y5, month = month)
      rs.all <- bind_rows(rs.all, rs)
    }

  }

  if(!is.null(convars)){
    for (i in 1:length(convars)) {
      cvars <- dat[[convars[i]]]
      rs <- crisk_con(csurv, cevent, cvars, convars[i])
      rs.all <- bind_rows(rs.all, rs)
    }

  }
  return(rs.all)
}




