library(dplyr) #for arrange and transmute
library(SimInf)
load_all()

movement_datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <- "tests/testthat/test_input_files/test_holdingdata_generic.csv" #needs herd_size
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml" #needs herd_size

time_unit <- "week" #every how much time / how many days to report results for
beta = 0.45 #transmission rate
#Guinat et al 2016: 0.3 between-pen, 0.6 within-pen;
#used in Halasa 2016a as low & high tm levels
epsilon = 0.1 #incubation rate (1/incubation period)
#Zhang et al 2021: 0.1 (0.05-0.25);
#based on Barongo et al 2016, Nielsen et al 2017
gamma = 0.25 #mortality rate
#Zhang et al 2021: 0.25 (0.07-0.5)
#based on Guinat et al 2016

#####################
### Reformat data ###
#####################

load_config(movement_configfile)

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("") #change holding identifiers to numbers only

events <-
  anonymisation_m$data |>
  transmute(
    event = "extTrans", #"extTrans" between nodes (or "intTrans","enter","exit")
    time = .data[[movenetenv$options$movedata_cols$date]], #date
    node = as.integer(.data[[movenetenv$options$movedata_cols$from]]), #from
    dest = as.integer(.data[[movenetenv$options$movedata_cols$to]]), #to
    n = .data[[movenetenv$options$movedata_cols$weight]], #weight (number pigs)
    proportion = 0, #alternative to n, if want to move proportion (set n as 0)
    select = 2, #col in model select matrix (which compartments to sample from)
    shift = 0) #col in model shift matrix (if want to shift compartment)

load_config(holding_configfile)

anonymisation_h <-
  holding_datafile |>
  reformat_data("holding") |>
  anonymise("", key=anonymisation_m$key)

nodes <-
  anonymisation_h$data |>
  arrange(as.numeric(.data[[movenetenv$options$holdingdata_cols$id]])) |>
  transmute(id = .data[[movenetenv$options$holdingdata_cols$id]],
            size = .data[[movenetenv$options$holdingdata_cols$herd_size]])

####################
### Set up model ###
####################

n <- length(nodes$id) # number of nodes = number of farms in dataset
u0 <- data.frame(S = nodes$size, # number of susceptibles = herd size
                 E = rep(0, n),
                 I = rep(0, n),
                 R = rep(0, n))

#Add single infectious individual to a single random node
infected_node<-sample(n,1)
u0[infected_node,1] <- u0[infected_node,1]-1 #take 1 individual out of S
u0[infected_node,3] <- 1 #add 1 individual to I

tspan <- seq(from = min(events$time),
             to = max(events$time),
             by = time_unit)

model <- SEIR(u0 = u0,
              tspan = tspan,
              events = events, #moves between nodes
              beta = beta, #transmission rate
              epsilon = epsilon, #incubation rate (1/incubation period)
              gamma = gamma) #mortality rate

#Change the select_matrix of the model so that the 2nd column indicates
#selection from S, E and I (but not R) compartments
select_matrix(model) <- matrix(c(1, 0, 0, 0, 1, 1, 1, 0), nrow = 4)


#################
### Run model ###
#################

#Summary stats of total outbreak size, for 100 simulations
summary(replicate(n = 10, {
  IR_at_end <- tail(trajectory(run(model))[c("I","R")], n)
  sum(IR_at_end)
}))

