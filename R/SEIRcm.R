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
##' @param beta the transmission rate from susceptible to infected
##'     state.
##' @param epsilon the incubation rate from exposed to infected state.
##' @param gamma the recovery rate from infected to recovered state.
##' @param coupling FIXME
##' @export
SEIRcm <- function(u0      = NULL,
                   tspan   = NULL,
                   beta    = NULL,
                   epsilon = NULL,
                   gamma   = NULL) {
    compartments <- c("S", "E", "I", "R")

    ## Check u0
    if (!is.data.frame(u0))
        u0 <- as.data.frame(u0)
    if (!all(compartments %in% names(u0)))
        stop("Missing columns in u0.", call. = FALSE)
    u0 <- u0[, compartments, drop = FALSE]

    ## Check beta
    if (!is.numeric(beta) ||
        length(beta) != 1 ||
        !is.finite(beta) ||
        any(beta < 0)) {
        stop("'beta' must be a numeric value >= 0.", call. = FALSE)
    }

    ## Check epsilon
    if (!is.numeric(epsilon) ||
        length(epsilon) != 1 ||
        !is.finite(epsilon) ||
        any(epsilon < 0)) {
        stop("'epsilon' must be a numeric value >= 0.", call. = FALSE)
    }

    ## Check gamma
    if (!is.numeric(gamma) ||
        length(gamma) != 1 ||
        !is.finite(gamma) ||
        any(gamma < 0)) {
        stop("'gamma' must be a numeric value >= 0.", call. = FALSE)
    }

    gdata <- c(beta = beta, epsilon = epsilon, gamma = gamma)

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
            NULL))

    ## State-change matrix
    S <- matrix(c(
        -1,  0,  0,  ## S
         1, -1,  0,  ## E
         0,  1, -1,  ## I
         0,  0,  1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    model <- SimInf_model(G     = G,
                          S     = S,
                          tspan = tspan,
                          gdata = gdata,
                          u0    = u0)

    as(model, "SEIRcm")
}

##' Run the SEIRcm model
##' @export
##' @noRd
setMethod(
    "run",
    signature(model = "SEIRcm"),
    function(model, solver = c("ssm", "aem"), ...) {
        solver <- match.arg(solver)
        validObject(model)
        .Call(SEIRcm_run, model, solver)
    }
)
