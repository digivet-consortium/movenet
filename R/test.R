#' A test function
#'
#' @param a a number
#' @param b another number
#'
#' @export
test <- function(a, b){
  return(Rcpp_test(a, b))
}
