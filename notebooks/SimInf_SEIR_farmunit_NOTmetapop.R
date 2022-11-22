library(dplyr) #for arrange and transmute
library(SimInf)
load_all()

movement_datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <- "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"

beta = 0 #within-farm transmission; 0 for farm-unit model
epsilon = #farm-level incubation rate (same as for individual?)
gamma =  #mortality rate (how does this work for farm level?)

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

u0 <- data.frame(S = rep(1,n), #all nodes have pop size 1 and start susceptible
                 E = rep(0,n),
                 I = rep(0,n),
                 R = rep(0,n))

#Make a single random node infectious
infected_node <- sample(n,1)
u0[infected_node, 1] <- 0 #change S to 0 for infected node
u0[infected_node, 3] <- 1 #change I to 1 for infected node

model <- SEIRcm(u0 = u0,
                tspan = tspan,
                beta = beta, #within-farm f.o.i.; 0 for farm-unit model
                epsilon = epsilon,
                gamma = gamma,
                contact_matrix = contact_matrix)


#################
### Run model ###
#################

#Summary stats of total outbreak size, for 100 simulations
summary(replicate(n = 100, {
  IR_at_end <- tail(trajectory(run(model))[c("I","R")], n)
  sum(IR_at_end)
}))

