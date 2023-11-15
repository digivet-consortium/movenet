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

#' @export
create_random_infected_vector <- function(n_nodes, n_initially_infected){
  infected_node_vector <- rep(0, n_nodes) #n_nodes needs to correspond to ncol(contact_matrix)
  infected_nodes <- sample(n_nodes, n_initially_infected)
  infected_node_vector[infected_nodes] <- 1
  return(infected_node_vector)
}

#' @export
create_specified_infected_vector <- function(infected_holdings, key){
  infected_node_vector <- rep(0, length(key))
  infected_nodes <- as.numeric(key[which(names(key) %in% infected_holdings)])
  infected_node_vector[infected_nodes] <- 1
  return(infected_node_vector)
}

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

