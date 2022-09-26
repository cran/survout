#'@title Modify the Output for a Multi-variable Survival Analysis.
#'
#'@description Create a table with the general multi-variable survival analysis results, including the HR (95 percent CI), P value.
#'@param ... arguments will be passed to coxph
#'@return a dataframe containing coxph output that includes variable names, HRs (95% CIs), and P values.
#'@examples
#'Dat <- survival::lung
#'surv_multi(survival::Surv(time, status) ~ as.factor(sex) + age + meal.cal, data = Dat)
#'
#'@export
#'@name surv_multi
#'
#'

surv_multi<- function (...)
{
  # get input elements
  input <- c(...)
  input.data <- as.data.frame(input[2:length(input)])

  # get variables
  parse_formula_in <- eval(parse(text = "reshape2:::parse_formula"))

  input.var <- parse_formula_in(input[[1]])[2]
  input.var2 <- unlist(input.var)
  var_n <- length(input.var2)
  vars <- NULL
  for (i in 1:var_n ) {
    vars <- c(vars, c(as.character(input.var2[[i]])))
  }
  # find out which varaible is used as a factor
  isfa <- which(vars%in%"as.factor")
  isfa_2 <- isfa + 1
  var.fa  <- vars[isfa_2]
  var.all <- vars[! vars %in% "as.factor"]
  #survival analysis
  .fit <- coxph(...)
  .surv.cl <- summary(.fit)$conf.int
  .surv.p <- summary(.fit)$coefficients
  .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 2),
                             "(", J.digit(.surv.cl[, 3], 2), ",", J.digit(.surv.cl[,4], 2), ")"), .surv.p[, 5])
  .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))

  #modify the table by adding reference group
  .black <- c("Reference",rep(" ", length(.surv.total[1,])-1))
  ##find locations- can I use apply?
  if (length(isfa) != 0) {
    order.fa <- isfa
    for ( i in 1:length(isfa)){
      order.fa[i] <- .fit$assign[[isfa[i] - i + 1]][1]
    }


    ## insert
    .surv.total <- JS.insert(order.fa, .black, .surv.total)
  }

  ## 1. add variable names
  row.names<- rownames(.surv.total)
  ### modifly raw names
  .surv.total <- cbind( row.names(.surv.total), .surv.total)
  colnames(.surv.total) <- c( 'Variables', 'HR ( 95%CI )', 'P-value')
  return (.surv.total)



}
