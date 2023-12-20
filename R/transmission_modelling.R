#' Create an SEIR model with contact matrix and ASF-inspired parameters
#'
#' @description
#' Creates a SimInf `SEIRcm` ([`siminf4movenet::SEIRcm()`]) model with the
#' following fixed parameters relating to African swine fever:
#' - `epsilon_rate = 18/6.1`
#' - `epsilon_shape = 18`
#' - `gamma_rate = 1/8`
#' - `gamma_shape = 1`
#'
#' N.B. This function requires the siminf4movenet package to be installed.
#'
#' @details
#' ## Holding-level incubation rate (epsilon) related parameters
#' Parameters for Epsilon come from \href{https://doi.org/10.1111/tbed.12748}{Guinat et al. 2018},
#'  via \href{https://doi.org/10.2903/j.efsa.2020.5996}{EFSA 2020}.
#' They are within-farm transmission parameters for one of 9 outbreaks of the
#' Georgia 2007/1 strain in Russia. The rate and shape parameters are both close
#' to the median for the 9 outbreaks. In the EFSA report, parameters from this
#' outbreak are used as a low-R0 scenario.
#' These parameters cover a larger range with a higher average, compared to
#' several other studies - but this might fit with the idea that farm-level
#' latent periods would be higher than individual-pig latent periods due to v low
#' level transmissibility if considering just a single pig.
#'
#' ## Holding-level removal rate (gamma) related parameters
#' Average time-to-removal (infectious period) was derived as follows:
#' average time from first infection to suspicion report = 13 days (\href{https://doi.org/10.2903/j.efsa.2020.5996}{EFSA 2020}) +
#' assumed time from suspicion report to removal = 1 day -
#' average latent period = 6 days (as from \href{https://doi.org/10.1111/tbed.12748}{Guinat et al. 2018})
#'
#' @inheritParams siminf4movenet::SEIRcm
#'
#' @seealso
#' * [siminf4movenet::SEIRcm()] for the underlying SEIRcm model
#' * [`SimInf::SimInf`] for the underlying modelling package
#'
#' @family transmission modelling-related functions
#' @export
SEIRcm_ASF <- function(infected, tspan, contact_matrix){

  if(!requireNamespace("siminf4movenet")){
    #update when have released version
    #stop("The siminf4movenet package is not installed - run:\ninstall.packages('siminf4movenet', repos=....)")
    stop("The siminf4movenet package is not installed - run:\ninstall_github('digivet-consortium/siminf4movenet')")
  }

  epsilon_rate = 18/(6.1) #farm-level incubation rate (same as for individual?)
  epsilon_shape = 18
  # These parameters for Epsilon come from Guinat et al. 2018, via EFSA 2020.
  # They are within-farm transmission parameters for one of 9 outbreaks of the
  # Georgia 2007/1 strain in Russia. The rate and shape parameters are both close
  # to the median for the 9 outbreaks. In the EFSA report, parameters from this
  # outbreak are used as a low-R0 scenario.
  # These parameters cover a larger range with a higher average, compared to
  # several other studies - but this might fit with the idea that farm-level
  # latent periods would be higher than individual-pig latent periods due to v low
  # level transmissibility if considering just a single pig.

  gamma_rate = 1/8 #removal rate
  gamma_shape = 1
  # Average time-to-removal (infectious period) was derived as follows:
  # average time from first infection to suspicion report = 13 days (EFSA) +
  # assumed time from suspicion report to removal = 1 day -
  # average latent period = 6 days (as from Guinat et al. above)


  if(requireNamespace("siminf4movenet")){

    siminf4movenet::SEIRcm(infected = infected,
                           tspan = tspan,
                           epsilon_rate = epsilon_rate,
                           epsilon_shape = epsilon_shape,
                           gamma_rate = gamma_rate,
                           gamma_shape = gamma_shape,
                           contact_matrix = contact_matrix)
  }
}

#' Create a vector of holdings infected at t=0 to plug into SimInf models, by
#' randomly selecting a specified number of holdings
#'
#' @param n_nodes Number of nodes/holdings included in the model.
#' @param n_initially_infected Number of holdings infected at t=0.
#'
#' @returns A numeric vector, indicating which nodes are infected at t=0, to
#' plug into the [siminf4movenet::SEIRcm()] model or another SimInf model.
#'
#' @seealso
#' * [siminf4movenet::SEIRcm()] for the SEIRcm model developed for movenet.
#' * [`SimInf::SimInf`] for the underlying modelling package.
#' @family transmission modelling-related functions
#'
#' @export
create_random_infected_vector <- function(n_nodes, n_initially_infected){
  infected_node_vector <- rep(0, n_nodes) #n_nodes needs to correspond to ncol(contact_matrix)
  infected_nodes <- sample(n_nodes, n_initially_infected)
  infected_node_vector[infected_nodes] <- 1
  return(infected_node_vector)
}

#' Create a vector of holdings infected at t=0 to plug into SimInf models, using
#' holding identifiers
#'
#' @param infected_holdings A character vector with (original) identifiers of
#'   infected holdings.
#' @param key The `key` provided by `data2contactmatrix()`, to identify the node
#'   numbers of the infected holdings.
#'
#' @returns A numeric vector, indicating which nodes are infected at t=0, to
#' plug into the [siminf4movenet::SEIRcm()] model or another SimInf model.
#'
#' @seealso
#' * [siminf4movenet::SEIRcm()] for the SEIRcm model developed for movenet.
#' * [`SimInf::SimInf`] for the underlying modelling package.
#' @family transmission modelling-related functions
#'
#' @export
create_specified_infected_vector <- function(infected_holdings, key){
  infected_node_vector <- rep(0, length(key))
  infected_nodes <- as.numeric(key[which(names(key) %in% infected_holdings)])
  infected_node_vector[infected_nodes] <- 1
  return(infected_node_vector)
}

#' Get the cumulative infected (I+R components) from SimInf model trajectories
#'
#'
#' @param trajectories SimInf model trajectories.
#' @param n_simulations Number of simulations that the model was run for.
#' @param n_nodes Number of nodes/holdings included in the model.
#'
#' @seealso
#' * [siminf4movenet::SEIRcm()] for the SEIRcm model developed for movenet.
#' * [`SimInf::SimInf`] for the underlying modelling package.
#' @family transmission modelling-related functions
#'
#' @importFrom dplyr row_number summarize
#' @export
get_cumulative_infected <- function(trajectories, n_simulations, n_nodes){

    sapply(1:n_simulations, function(x){
      cumul_df <- data.frame(cumul_infected = trajectories[["I",x]] + trajectories[["R",x]])
      cumul_df <- cumul_df %>%
        mutate(group = ceiling(row_number() / n_nodes))
      result <- cumul_df %>%
        group_by(group) %>%
        summarize(sum = sum(cumul_infected))
      return(result$sum)
    })

}

