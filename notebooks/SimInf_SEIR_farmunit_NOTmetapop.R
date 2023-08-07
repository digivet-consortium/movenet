##############
### Set-up ###
##############


library(dplyr) #for arrange and transmute
if(!requireNamespace("siminf4movenet")){
  #update when have released version
  #stop("The siminf4movenet package is not installed - run:\ninstall.packages('siminf4movenet', repos=....)")
  stop("The siminf4movenet package is not installed - run:\ninstall_github('digivet-consortium/siminf4movenet')")
}


load_all()

movement_datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <- "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"

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

days = 365 #number of days to run the simulation. Default is 365 days.
stride = 1 #the increment (integer) between days that are recorded in the model
           #U and V matrices. Default is 1 day, i.e., every day is recorded.

#####################
### Reformat data ###
#####################

load_config(movement_configfile)
movement_data <- movement_datafile |> reformat_data("movement")

load_config(holding_configfile)
holding_data <- holding_datafile |> reformat_data("holding")

#############################
### Create contact matrix ###
#############################

load("inst/extdata/local_spread_probabilities_ASF_Halasa_et_al_2016.Rdata")
# For now see notebook "data2contactmatrix.R"
contact_matrix <-
  data2contactmatrix(movement_data, holding_data, incl_nonactive_holdings = TRUE,
                     weight_unit_transmission_probability = 1,
                     whole_months = TRUE,
                     local_spread_transmission_probabilities =
                       local_spread_probabilities_ASF_Halasa_et_al_2016,
                     additional_transmission_prob_matrices = NULL)


####################
### Set up model ###
####################

n_nodes <- ncol(contact_matrix) #number of holdings -- don't call this n to avoid confusion w/in replicate

tspan <- seq(from = 1L, to = as.integer(days), by = as.integer(stride))

create_infected_vector <- function(n_nodes, n_infected){
  infected_node_vector <- rep(0, n_nodes)
  infected_nodes <- sample(n_nodes, n_infected)
  infected_node_vector[infected_nodes] <- 1
}

if(requireNamespace("siminf4movenet")){

  model <- SEIRcm(infected = create_infected_vector(n_nodes, 1), #newly samples starting infected holding for each
                  tspan = tspan,
                  epsilon_rate = epsilon_rate,
                  epsilon_shape = epsilon_shape,
                  gamma_rate = gamma_rate,
                  gamma_shape = gamma_shape,
                  contact_matrix = contact_matrix)


  #################
  ### Run model ###
  #################

  #If running 100 model n simulations, is it best to save run(model) or trajectories?
  models <- replicate(n = 100, run(model)) #do these simulations need the same or a newly sampled starting infected node?
  trajectories <- replicate(n = 100, trajectory(run(model)))

  #Summary stats of total outbreak size, for 100 simulations
  summary(replicate(n = 100, {
    IR_at_end <- tail(trajectory(run(model))[c("I","R")], n_nodes)
    sum(IR_at_end)
  }))



}





