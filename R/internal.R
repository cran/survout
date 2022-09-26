
J.digit <- function(x, k) format(round(x, k), nsmall=k)

JS.p <-function (value) {

  f <- function(p) {
    if (is.na(p))
      p <- NA
    else if (p < 0 || p > 1)
      stop(paste(p, "is not a valid probability"))
    else if (p > 0.99)
      p <- "> 0.99"
    else if (p > 0.1)
      p <- format(round(p, 2), nsmall = 2)
    else if (p > 0.001)
      p <- format(round(p, 3), nsmall = 2)
    else p <- "< 0.001"
    return(p)
  }
  p <- sapply(value, f)
  return(p)
}

JS.insert <- function(locs, inrows, df, rep = TRUE){
  #locs = insert locations
  #inrows = insert rows
  #df = dataframe

  #check if inrows and df are the same type
  #if (class(inrows) != class(df)){print("Insert rows should be the same type as data")}

  if ( rep == TRUE ) {
    for (i in 1:length(locs)){
      #the inrows can be only one column or  multiple columns
      #insert.n <- length(inrows[,1])
      insert.n <- 1
      loc <- locs[i] + insert.n * i - 1
      if (loc == 0) {
        df <- rbind(inrows, df)
      }
      else{
        df <- rbind(df[1:loc - 1, ], inrows, df[loc:nrow(df), ])
      }
    }
  }
  else
  {
    for (i in 1:length(locs)){
      loc <- locs[i] + i - 1
      .out <- rbind(df[0:loc - 1, ], inrows[i], df[loc:nrow(df), ])
    }
  }
  return (df)
}

ingorfactor_c <- function(x,event){
  # exclude the level of variable which has no risk event
  del_factor  <- names(x %>% table)[which(table(x, event)[,2] <1)]
  del_factor2 <- names(x %>% table)[table(x) < 10]
  x[((x %in% del_factor)&(x %in% del_factor2))]<- NA
  x
}

