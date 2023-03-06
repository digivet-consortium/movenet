is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
    abs(x - round(x)) < tol
}

##' Definition of the \sQuote{SEIRcm} model
##'
##' Class to handle the SEIRcm model.
##' @import SimInf
##' @export
setClass("SEIRcm", contains = c("SimInf_model"))

##' Create an SEIR model with contact matrix
##'
##' Create an \code{SEIRcm} model to be used by the simulation
##' framework.
##'
##' The \code{SEIRcm} model contains four compartments per node;
##' susceptible (S), exposed (E) (nodes that have been infected but
##' are not yet infectious), infectious (I), and recovered
##' (R). Moreover, it also includes a contact matrix to capture
##' between-node spread. The \code{SEIRcm} model contains six state
##' transitions. The ternary operator \code{(a ? b : c)} is a
##' shorthand for an if-else-statement that evaluates to \code{b} if
##' the value of \code{a} is true, else to \code{c}.
##'
##' \deqn{@ \rightarrow (S > 0\,\&\&\,E == 0)\,?\,\lambda_i : 0
##' \rightarrow E}{@ -> (S > 0 && E == 0) ? lambda_i : 0 -> E}
##'
##' \deqn{@ \rightarrow (E > 0\,\&\&\,E <
##' \epsilon_{\text{shape}})\,?\,\epsilon_{\text{rate}} : 0
##' \rightarrow E}{@ -> (E > 0 && E < epsilon_shape) ? epsilon_rate :
##' 0 -> E}
##'
##' \deqn{@ \rightarrow (I == 0\,\&\&\,E ==
##' \epsilon_{\text{shape}})\,?\,\epsilon_{\text{rate}} : 0
##' \rightarrow E + I}{@ -> (I == 0 && E == epsilon_shape) ?
##' epsilon_rate : 0 -> E + I}
##'
##' \deqn{@ \rightarrow (I > 0\,\&\&\,I < \gamma_{\text{shape}})\,?\,
##' \gamma_{\text{rate}} : 0 \rightarrow I}{@ -> (I > 0 && I < gamma_shape) ?
##' gamma_rate : 0 -> I}
##'
##' \deqn{@ \rightarrow (R == 0\,\&\&\,I ==
##' \gamma_{\text{shape}})\,?\,\gamma_{\text{rate}} : 0 \rightarrow I
##' + R}{@ -> (R == 0 && I == gamma_shape) ?  gamma_rate : 0 -> I + R}
##'
##' Where the first transition depends on an external force of
##' infection denoted lambda_i, calculated in the post-time-step
##' function using the contact matrix. The second and third
##' transitions involves the stages of an Erlang process for E;
##' \eqn{E_1 \rightarrow E_2 \rightarrow \ldots \rightarrow
##' E_{\epsilon_{\text{shape}}}}{E_1 -> E_2 -> \ldots ->
##' E_epsilon_shape}. The first part of the Erlang process for \eqn{E}
##' (transition 2) simulates up to, but not including the last stage,
##' in a process to increment the counter for the \eqn{E} stages. The
##' last stage \eqn{E_{\epsilon_{\text{shape - 1}}} \rightarrow
##' E_{\epsilon_{\text{shape}}}}{E_(epsilon_shape - 1) ->
##' E_epsilon_shape} occurs when the \eqn{E} count equals the
##' \eqn{\epsilon_{\text{shape}}}{epsilon_shape} parameter and the
##' \eqn{I} count is zero. Similarly, the fourth and fifth transitions
##' involves the stages of an Erlang process for I; \eqn{I_1
##' \rightarrow I_2 \rightarrow \ldots \rightarrow
##' E_{\gamma_{\text{shape}}}}{I_1 -> I_2 -> \ldots ->
##' E_gamma_shape}. The first part of the Erlang process for \eqn{I}
##' (transition 4) simulates up to, but not including the last stage,
##' in a process to increment the counter for the \eqn{I} stages. The
##' last stage \eqn{I_{\gamma_{\text{shape - 1}}} \rightarrow
##' I_{\gamma_{\text{shape}}}}{I_(gamma_shape - 1) -> I_gamma_shape}
##' occurs when the \eqn{I} count equals the
##' \eqn{\gamma_{\text{shape}}}{gamma_shape} parameter and the \eqn{R}
##' count is zero.
##' @param infected logical vector with the infectious status in each
##'     node.
##' @param tspan A vector (length >= 1) of increasing time points
##'     where the state of each node is to be returned. Can be either
##'     an \code{integer} or a \code{Date} vector. A \code{Date}
##'     vector is coerced to a numeric vector as days, where
##'     \code{tspan[1]} becomes the day of the year of the first year
##'     of \code{tspan}. The dates are added as names to the numeric
##'     vector.
##' @param epsilon_rate the incubation rate from exposed to infected
##'     state.
##' @param epsilon_shape number of stages in the exposed (E)
##'     state. Each stage has an exponentially distributed duration
##'     given by epsilon_rate. Default is 1.
##' @param gamma_rate the recovery rate from infected to recovered
##'     state.
##' @param gamma_shape number of stages in the infectious (I)
##'     state. Each stage has an exponentially distributed duration
##'     given by gamma_rate. Default is 1.
##' @param contact_matrix an optional contact matrix to include
##'     between-node transmission of infection. The contact matrix
##'     specifies the strength of the interaction between
##'     nodes. Specifically, \code{contact_matrix[i, j]} measures the
##'     relative strength of transmission from node \code{i} to node
##'     \code{j}.
##' @importFrom methods as
##' @importFrom methods validObject
##' @export
##' @examples
##' ## Create an SEIRcm model with 1000 initially infected nodes.
##' ## Assume that the average infectious period is 10 days.
##' n <- 1000
##' m1 <- SEIRcm(
##'     infected = rep(TRUE, n),
##'     tspan = 1:100,
##'     epsilon_rate = 1/5,
##'     gamma_rate = 1/10)
##'
##' ## Run the model and extract the trajectory.
##' t1 <- trajectory(run(m1))
##'
##' ## Determine the duration of infection in each node. Add half a
##' ## day, since the transition happens sometime during the day when
##' ## the status becomes R.
##' d1 <- sapply(split(t1$I, t1$node), function(infected) {
##'     x <- which(infected > 0)
##'     ifelse(length(x), max(x), 0) + 0.5
##' })
##'
##' ## Again, create an SEIRcm model with 10000 initially infected
##' ## nodes. But this time, assume the infection happens in 10 steps,
##' ## where each step has a duration of 1 day on average.
##' m2 <- SEIRcm(
##'     infected = rep(TRUE, n),
##'     tspan = 1:100,
##'     epsilon_rate = 1/5,
##'     gamma_rate = 1,
##'     gamma_shape = 10)
##'
##' ## Run the model and extract the trajectory for the infected
##' ## state.
##' t2 <- trajectory(run(m2))
##'
##' ## Determine the duration of infection in each node. Add half a
##' ## day, since the transition happens sometime during the day when
##' ## the status becomes R.
##' d2 <- sapply(split(t2$I, t2$node), function(infected) {
##'     x <- which(infected > 0)
##'     ifelse(length(x), max(x), 0) + 0.5
##' })
##'
##' hist(d2, freq = FALSE, col = rgb(1, 0, 0, 1/4), xlim = c(0, 100),
##'      xlab = "Duration of infection in days", main = "")
##' hist(d1, freq = FALSE, col = rgb(0, 0, 1, 1/4), add = TRUE)
SEIRcm <- function(infected = NULL,
                   tspan = NULL,
                   epsilon_rate = NULL,
                   epsilon_shape = 1,
                   gamma_rate = NULL,
                   gamma_shape = 1,
                   contact_matrix = NULL) {
    compartments <- c("S", "E", "I", "R")

    ## Check infected.
    infected <- as.logical(infected)
    if (length(infected) < 1 || any(!is.finite(infected))) {
        stop("'infected' must be a logical vector.", call. = FALSE)
    }

    ## Create u0.
    u0 <- data.frame(
        S = as.integer(!infected),
        E = rep(0, length(infected)),
        I = as.integer(infected),
        R = rep(0, length(infected)))

    ## Check epsilon_rate
    if (!is.numeric(epsilon_rate) ||
        !all(is.finite(epsilon_rate)) ||
        any(epsilon_rate < 0)) {
        stop("'epsilon_rate' must be a numeric value >= 0.",
             call. = FALSE)
    }
    epsilon_rate <- rep(epsilon_rate, length.out = nrow(u0))

    ## Check epsilon_shape
    if (!is.numeric(epsilon_shape) ||
        !all(is.finite(epsilon_shape)) ||
        !all(is_wholenumber(epsilon_shape)) ||
        any(epsilon_shape < 1)) {
        stop("'epsilon_shape' must be an integer value >= 1.",
             call. = FALSE)
    }
    epsilon_shape <- rep(epsilon_shape, length.out = nrow(u0))

    ## Check gamma_rate
    if (!is.numeric(gamma_rate) ||
        !all(is.finite(gamma_rate)) ||
        any(gamma_rate < 0)) {
        stop("'gamma_rate' must be a numeric value >= 0.",
             call. = FALSE)
    }
    gamma_rate <- rep(gamma_rate, length.out = nrow(u0))

    ## Check gamma_shape
    if (!is.numeric(gamma_shape) ||
        !all(is.finite(gamma_shape)) ||
        !all(is_wholenumber(gamma_shape)) ||
        any(gamma_shape < 1)) {
        stop("'gamma_shape' must be an integer value >= 1.",
             call. = FALSE)
    }
    gamma_shape <- rep(gamma_shape, length.out = nrow(u0))

    ## Check contact_matrix
    if (is.null(contact_matrix)) {
        contact_matrix <- matrix(numeric(0), nrow = 0, ncol = nrow(u0))
        v0 <- data.frame(lambda_i = rep(0, nrow(u0)))
    } else {
        if (!is.numeric(contact_matrix) ||
            !is.matrix(contact_matrix) ||
            nrow(contact_matrix) != ncol(contact_matrix) ||
            nrow(contact_matrix) != nrow(u0) ||
            any(!is.finite(contact_matrix)) ||
            any(contact_matrix < 0)) {
            stop("Invalid 'contact_matrix'.", call. = FALSE)
        }

        ## Ensure the diagonal is zero.
        diag(contact_matrix) <- 0

        ## Calculate initial lambda_i.
        v0 <- data.frame(lambda_i = as.numeric(t(contact_matrix) %*% u0$I))
    }

    ## Determine the number of contacts.
    n_contacts <- rep(nrow(contact_matrix), nrow(u0))

    ## 'ldata' is a numeric matrix that contains local data specific
    ## for each node and column ldata[, j] contains the local data
    ## vector for node j. Also, drop the dimnames to skip printing the
    ## matrix when printing the SEIRcm model object.
    ldata <- rbind(
        epsilon_rate,
        epsilon_shape,
        gamma_rate,
        gamma_shape,
        n_contacts,
        contact_matrix)
    dimnames(ldata) <- NULL

    ## Dependency graph.
    G <- matrix(c(
        1, 1, 1, 0, 0,
        1, 1, 1, 0, 0,
        1, 1, 1, 1, 1,
        0, 0, 1, 1, 1,
        0, 0, 1, 1, 1),
        nrow = 5,
        byrow = TRUE,
        dimnames = list(
            c("@ -> (S > 0 && E == 0) ? lambda_i : 0 -> E",
              "@ -> (E > 0 && E < epsilon_shape) ? epsilon_rate : 0 -> E",
              "@ -> (I == 0 && E == epsilon_shape) ? epsilon_rate : 0 -> E + I",
              "@ -> (I > 0 && I < gamma_shape) ? gamma_rate : 0 -> I",
              "@ -> (R == 0 && I == gamma_shape) ? gamma_rate : 0 -> I + R"),
            NULL))

    ## State-change matrix.
    S <- matrix(c(
        0, 0, 0, 0, 0,  ## S
        1, 1, 1, 0, 0,  ## E
        0, 0, 1, 1, 1,  ## I
        0, 0, 0, 0, 1), ## R
        nrow = length(compartments),
        byrow = TRUE,
        dimnames = list(compartments, NULL))

    model <- SimInf_model(G     = G,
                          S     = S,
                          tspan = tspan,
                          ldata = ldata,
                          u0    = u0,
                          v0    = v0)

    methods::as(model, "SEIRcm")
}

##' Run the SEIRcm model
##' @export
##' @useDynLib movenet SEIRcm_run
##' @noRd
setMethod(
    "run",
    signature(model = "SEIRcm"),
    function(model, solver = c("ssm", "aem"), ...) {
        solver <- match.arg(solver)

        ## Pre-process u0. First, set all non-zero compartments to 1,
        ## and check that only one compartment is set.
        model@u0[model@u0 > 0L] <- 1L
        if (any(colSums(model@u0) > 1)) {
            stop("'u0' is invalid. Multiple compartments > 0.",
                 call. = FALSE)
        }

        ## Set I to gamma_shape + 1 when R > 0.
        i <- which(model@u0["R", ] > 0L)
        model@u0["I", i] <-
            as.integer(model@ldata[4, i] + 1)

        ## Set E to epsilon_shape + 1 when I > 0.
        i <- which(model@u0["I", ] > 0L)
        model@u0["E", i] <-
            as.integer(model@ldata[2, i] + 1)

        ## Let S = 1 in all nodes.
        model@u0["S", ] <- 1L

        methods::validObject(model)
        model <- .Call(SEIRcm_run, model, solver)

        ## Post-process U. Ensure only one compartment is set in every
        ## time-step.
        U <- as.integer(model@U)
        U[U > 0L] <- 1L
        for (i in seq_len(nrow(model@S) - 1L)) {
            j <- seq(from = i, to = length(U), by = nrow(model@S))
            U[j] <- U[j] - U[j + 1L]
        }
        dim(U) <- dim(model@U)
        model@U <- U

        model
    }
)
