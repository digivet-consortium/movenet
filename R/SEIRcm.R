##' Definition of the \sQuote{SEIRcm} model
##'
##' Class to handle the SEIRcm model.
##' @importClassesFrom SimInf SimInf_model
##' @export
setClass("SEIRcm", contains = c("SimInf_model"))

##' Create an SEIR model with contact matrix
##'
##' @param u0 FIXME
##' @param tspan FIXME
##' @param events FIXME
##' @param beta FIXME
##' @param epsilon FIXME
##' @param gamma FIXME
##' @export
SEIRcm <- function(u0,
                   tspan,
                   events  = NULL,
                   beta    = NULL,
                   epsilon = NULL,
                   gamma   = NULL) {
    compartments <- c("S", "E", "I", "R")
}
