library(dplyr) #for arrange and transmute
library(SimInf)
load_all()

movement_datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <- "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"

epsilon_rate = #farm-level incubation rate (same as for individual?)
epsilon_shape = 1
gamma_rate =  #mortality rate (how does this work for farm level?)
gamma_shape = 1

days = 365 #number of days to run the simulation. Default is 365 days.
stride = 1 #the increment (integer) between days that are recorded in the model
           #U and V matrices. Default is 1 day, i.e., every day is recorded.

#####################
### Reformat data ###
#####################

load_config(movement_configfile)

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

load_config(holding_configfile)

anonymisation_h <-
  holding_datafile |>
  reformat_data("holding") |>
  anonymise("", key=anonymisation_m$key)

nodes <-
  anonymisation_h$data |>
  arrange(as.numeric(.data[[movenetenv$options$holdingdata_cols$id]])) |>
  transmute(id = .data[[movenetenv$options$holdingdata_cols$id]])


#############################
### Create contact matrix ###
#############################

# For now see notebook "data2contactmatrix.R"


####################
### Set up model ###
####################

n <- length(nodes$id) #number of farms

tspan <- seq(from = 1L, to = as.integer(days), by = as.integer(stride))

#Make a single random node infectious
infected <- rep(0,n)
infected_node <- sample(n,1)
infected[n] <- 1

model <- SEIRcm(infected = infected,
                tspan = tspan,
                epsilon_rate = epsilon_rate,
                epsilon_shape = epsilon_shape,
                gamma_rate = gamma_rate,
                gamma_shape = gamma_shape,
                contact_matrix = contact_matrix)


#################
### Run model ###
#################

#Summary stats of total outbreak size, for 100 simulations
summary(replicate(n = 100, {
  IR_at_end <- tail(trajectory(run(model))[c("I","R")], n)
  sum(IR_at_end)
}))

