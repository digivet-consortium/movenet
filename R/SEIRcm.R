##' Definition of the \sQuote{SEIRcm} model
##'
##' Class to handle the SEIRcm model.
##' @import SimInf
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
##' @param coupling FIXME
##' @export
SEIRcm <- function(u0,
                   tspan,
                   events  = NULL,
                   beta    = NULL,
                   epsilon = NULL,
                   gamma   = NULL) {
    compartments <- c("S", "E", "I", "R")

    ## Dependency graph.
    G <- matrix(c(
        1, 1, 1,
        1, 1, 1,
        1, 1, 1),
        nrow = 3,
        byrow = TRUE,
        dimnames = list(
            c("S -> beta*S*(I+coupling)/(S+E+I+R) -> E",
              "E -> epsilon*E -> I",
              "I -> gamma*I -> R"),
            NULL)

    ## State-change matrix
    S <- matrix(c(
        -1,  0,  0   ## S
         1, -1,  0   ## E
         0,  1, -1   ## I
         0,  0,  1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    model <- SimInf_model(G      = G,
                          S      = S,
                          tspan  = tspan,
                          u0     = u0)

    as(model, "SEIRcm")
}
