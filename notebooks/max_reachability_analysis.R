##############
### Set-up ###
##############

library(networkDynamic) #networkDynamic
library(tsna) #tReach, [tPath]
library(dplyr) #select [can be avoided]
library(tidyr) #pivot_longer
library(ggplot2)
library(lubridate) #wday

load_all()

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
load_config(movement_configfile)

#jitter_set = c(0:10) #more sensible values
jitter_set = c(0, 4, 15, 30, 46, 91, 183) #corresponding to half rounding units
round_set = c("day", "week", "month", "bimonth", "quarter", "halfyear", "year")
#Do we want any jitter + rounding? does not seem particularly sensible, but
#depends on relative values

#How much jitter & which rounding, for fig 1 on monthly max reachabilities?
#Make sure these are included in jitter_set and round_set above
jitter_mmr <- c(4, 15)
round_mmr <- c("week", "month")

#######################################
### Reformat data & create networks ###
#######################################

#reformat movement data, anonymise to have node ids as numbers (required!)
anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

true_data <- anonymisation_m$data

#create dynamic networks
movedata2networkDynamic <- function(data){
  nd_data <-
    data |>
    select(onset = 3, terminus = 3, tail = 1, head = 2) |>
    lapply(as.integer) |>  #networkDynamic doesnt seem to like tibbles, need to
    data.frame()           #convert to df - hence using lapply cf purrr::modify

  networkDynamic(edge.spells = nd_data)
}

true_network <- movedata2networkDynamic(true_data)

jitter_networks <-
  lapply(jitter_set, function(x){
    coarsen_date(true_data, jitter = x, rounding_unit = FALSE) |>
    movedata2networkDynamic()
  })
names(jitter_networks)<-paste0("jitter (",jitter_set," days)")

week_start <- wday(min(true_data[[movenetenv$options$movedata_cols$date]]))
rounding_networks <-
  lapply(round_set, function(x){
    coarsen_date(true_data, jitter = FALSE, rounding_unit = x,
                 week_start = week_start) |>
      movedata2networkDynamic()
  })
names(rounding_networks)<-paste0(round_set,"ly")

##########################################################
### Fig 1 prep: Extract monthly maximum reachabilities ###
##########################################################

#Extract months covered in dataset
extract_months <- function(data){
  start_dates <- seq(floor_date(min(data),"month"),
                     floor_date(max(data),"month"),
                     by = "month")
  end_dates <- as.integer(start_dates + months(1))
  start_dates <- as.integer(start_dates)
  Map(c,start_dates,end_dates)
}

months_in_data <-
  extract_months(true_data[[movenetenv$options$movedata_cols$date]])

#Extract monthly max reachabilities
max_reachability_for_period <- function(network, start, end){
  network |>
    network.extract(onset = start, terminus = end,
                    rule = "any",
                    trim.spells = TRUE) |> #essential: eliminates edge activity
                                           #spells outside desired time frame
    tReach(graph.step.time = 1) |> #identifies reachable sets
    max()
}

selected_networks <- c(true=list(true_network),
                       jitter_networks[paste0("jitter (",jitter_mmr," days)")],
                       rounding_networks[paste0(round_mmr,"ly")])

monthly_max_reachabilities <- tibble(.rows = length(months_in_data))

for (netw_name in names(selected_networks)){
  monthly_max_reachabilities[netw_name] <-
    sapply(months_in_data,
           function(x){
             max_reachability_for_period(selected_networks[[netw_name]],
                                         x[[1]], x[[2]])
          })
}

#Reformat to long tibble for plotting
monthly_max_reachabilities <-
  monthly_max_reachabilities |>
  pivot_longer(everything(),
               names_to = "network",
               values_to = "max_reachability")

#Set network as a factor with specific order (to avoid default alphabetic order)
monthly_max_reachabilities$network <-
  factor(monthly_max_reachabilities$network,
         levels = names(selected_networks))

########################################################
### Fig 1: boxplot of monthly maximum reachabilities ###
########################################################

p <-
  ggplot(data = monthly_max_reachabilities,
         aes(x = network, y = max_reachability)) +
  xlab("Movement network") +
  ylab ("Monthly maximum reachability") +
  geom_boxplot()

plot(p)


##########################################################
### Fig 2 prep: Extract overall maximum reachabilities ###
##########################################################

jitter_measures <- tibble(jitter = jitter_set,
                          max_reachability = "")
jitter_measures[, "max_reachability"] <-
  sapply(jitter_networks, function(x){max(tReach(x, graph.step.time = 1))})

round_measures <- tibble(round = c(1,7,30.4,60.8,91.3,182.5,365),
                         max_reachability = "")
round_measures[, "max_reachability"] <-
  sapply(rounding_networks, function(x){max(tReach(x, graph.step.time = 1))})


##########################################################################
### Fig 2: Max reachabilities for a diverse range of jitter & rounding ###
##########################################################################

q <-
  ggplot(data = jitter_measures,
         aes(x = jitter, y = max_reachability)) +
  xlab("Jitter (days)") +
  ylab ("Maximum reachability") +
  geom_point()

plot(q)

r <-
  ggplot(data = round_measures,
         aes(x = round, y = max_reachability)) +
  xlab("Rounding unit equivalent (days)") +
  ylab ("Maximum reachability") +
  geom_point()

plot(r)
