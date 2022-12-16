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
##' @param coupling a measure of the strength of the interaction
##'     between nodes. Specifically, \code{coupling[i, j]} measures
##'     the relative strength of transmission to node \code{i} from
##'     node \code{j}.
##' @export
##' @examples
##' ## To illustrate the model, use the synthetic population of 1600
##' ## nodes in SimInf.
##' data("nodes", package = "SimInf")
##' plot(y ~ x, nodes, pch = 20, asp = 1)
##'
##' ## Assume a power-law kernel for the relative strength of the coupling
##' ## between populations where 'd' is the distance in meters between
##' ## populations and 'k' is a spatial kernel scaling parameter.
##' power_law_kernel <- function(d, k) {
##'     1 / (1 + (d/1000)^k)
##' }
##'
##' ## Calculate the coupling between nodes.
##' coupling <- power_law_kernel(as.matrix(dist(nodes)), 1.76)
##' diag(coupling) <- 0
##'
##' ## Assume each of the 1600 nodes consists of 100 susceptible
##' ## individuals. Then add one infected individual to the first node.
##' u0 <- data.frame(
##'     S = rep(100, nrow(nodes)),
##'     E = rep(0, nrow(nodes)),
##'     I = rep(0, nrow(nodes)),
##'     R = rep(0, nrow(nodes)))
##' u0$I[1] <- 1
##'
##' ## Create a SEIRvm model to simulate a trajectory over a year.
##' model <- SEIRcm(
##'     u0       = u0,
##'     tspan    = 1:365,
##'     beta     = 0.005,
##'     epsilon  = 0.25,
##'     gamma    = 0.077,
##'     coupling = coupling)
##'
##' ## Run the model and display a summary of the output in the generated
##' ## trajectory.
##' result <- run(model)
##' summary(result)
##'
##' ## Plot the number of individuals
##' plot(result, ~S+E+I+R)
##'
##' ## Plot the external force of infection
##' plot(result, ~lambda_i)
##'
##' ## Plot the individual level prevalence.
##' plot(result, I~S+E+I+R)
##'
##' ## Plot the node level prevalence.
##' plot(result, I~S+E+I+R, level = 2)
##'
##' ## Plot the within-node prevalence.
##' plot(result, I~S+E+I+R, level = 3)
SEIRcm <- function(u0       = NULL,
                   tspan    = NULL,
                   beta     = NULL,
                   epsilon  = NULL,
                   gamma    = NULL,
                   coupling = NULL) {
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

    ## Check coupling
    if (!is.numeric(coupling) ||
        !is.matrix(coupling) ||
        nrow(coupling) != ncol(coupling) ||
        nrow(coupling) != nrow(u0) ||
        any(!is.finite(coupling)) ||
        any(coupling < 0)) {
        stop("Invalid 'coupling' matrix.", call. = FALSE)
    }

    ## Ensure the diagonal is zero. Also, drop the dimnames to skip
    ## printing the matrix when printing the SEIRcm model object.
    diag(coupling) <- 0
    dimnames(coupling) <- NULL

    ## 'ldata' is a numeric matrix with local data specific to each
    ## node. The column ldata[, j] contains the local data vector for
    ## node j. The local data vector is passed as an argument to the
    ## post time step function where it will be used determine the
    ## strength of interaction between populations. To use the
    ## coupling matrix in ldata,it has to be transformed.
    ldata <- t(coupling)

    ## Add one row with the number of nodes in the model. This is used
    ## in the post time step function to iterate over all nodes.
    ldata <- rbind(rep(nrow(u0), nrow(u0)), ldata)

    ## FIXME: must calculate the initial strength of interaction
    ## between nodes from u0. For now, set lambda_i = 0.
    v0 <- data.frame(lambda_i = rep(0, nrow(u0)))

    ## Dependency graph.
    G <- matrix(c(
        1, 1, 1,
        1, 1, 1,
        1, 1, 1),
        nrow = 3,
        byrow = TRUE,
        dimnames = list(
            c("S -> beta*S*(I/(S+E+I+R)+lambda_i) -> E",
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
                          ldata = ldata,
                          u0    = u0,
                          v0    = v0)

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
