library(dplyr) #for arrange and transmute
library(SimInf)
load_all()

movement_datafile <- "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <- "tests/testthat/test_input_files/test_holdingdata_generic.csv" #needs herd_size
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml" #needs herd_size

epsilon = #farm-level incubation rate (same as for individual?)
gamma =  #mortality rate (how does this work for farm level?)


#####################
### Reformat data ###
#####################

load_config(movement_configfile)

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

events <-
  anonymisation_m$data |>
  transmute(event="extTrans", #for movements between nodes
            time=.data[[movenetenv$options$movedata_cols$date]],
            node=as.integer(.data[[movenetenv$options$movedata_cols$from]]),
            dest=as.integer(.data[[movenetenv$options$movedata_cols$to]]),
            n=.data[[movenetenv$options$movedata_cols$weight]],
            proportion=0, #alternative to n (set n as 0)
            select=2, #col in model select matrix (which comp to sample from)
            shift=0) #col in model shift matrix (if want to shift compartment)

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

n <- length(nodes$id)

transitions <- NULL
compartments <- NULL
for (i in 1:n){
  beta_i <- paste0("beta_",i,"_")
  j <- 1:n
  S_i <- paste0("S_",i)
  E_i <- paste0("E_",i)
  I_i <- paste0("I_",i)
  R_i <- paste0("R_",i)
  foi <- paste0(beta_i,j,"*I_",j, collapse = " + ")
  S2E <- paste0(S_i," -> ",S_i,"*(",foi,") -> ",E_i) #does this need an N somewhere?
  E2I <- paste0(E_i," -> epsilon*",E_i," -> ",I_i)
  I2R <- paste0(I_i," -> gamma*",I_i," -> ",R_i)
  transitions <- append(transitions,c(S2E,E2I,I2R)) #not efficient
  compartments <- append(compartments,c(S_i,E_i,I_i,R_i)) #not efficient
}

#gdata <- c(
#           epsilon = epsilon,
#           gamma = gamma)
# Add whole collection of beta's

#define tspan

model <- mparse(transitions = transitions,
                compartments = compartments,
                gdata = gdata,
                u0 = data.frame(rep(c(1,0,0,0),n),
                                colnames=compartments),
                tspan = tspan)
