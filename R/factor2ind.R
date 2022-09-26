#' Make An Integer Matrix Out of A Factor Variable.
#'
#'Create an indicator matrix of dimension length(x) x (nlevels(x)-1) with the column corresponding
#'to the baseline level removed (by default the first level is used as baseline).
#'@param x a variable.
#'@param baseline a string indicating the reference level.
#'@importFrom stats relevel
#'@return a matrix
#'@examples
#'x = gl(4, 2, labels = c( "A", "B", "C", "D"))
#'factor2ind(x)
#'factor2ind(x, "C")
#'@export
#'@name factor2ind


factor2ind <- function(x, baseline){

  xname <- deparse(substitute(x))
  n <- length(x)
  x <- as.factor(x)
  if(!missing(baseline)) x <- relevel(x, baseline)
  X <- matrix(0, n, length(levels(x)))
  X[(1:n) + n*(unclass(x)-1)] <- 1
  X[is.na(x),] <- NA
  dimnames(X) <- list(names(x), paste(xname, levels(x), sep = ":"))
  return(X[,-1,drop=FALSE])
}





