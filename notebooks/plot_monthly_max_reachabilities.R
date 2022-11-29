##############
### Set-up ###
##############

library(networkDynamic) #networkDynamic
library(tsna) #tReach, [tPath]
library(ndtv) #[transmissionTimeline]
library(dplyr) #select [can be avoided]
library(tidyr) #pivot_longer
library(ggplot2)

load_all()

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
load_config(movement_configfile)

jitter = 5 #how many days to jitter the date by?


#######################################
### Reformat data & create networks ###
#######################################

#reformat movement data to have node ids and dates as numbers
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

jittered_network <-
  true_data |>
    coarsen_date(jitter = jitter, rounding_unit = FALSE) |>
    movedata2networkDynamic()

weekly_network <-
  true_data |>
    coarsen_date(jitter = FALSE, rounding_unit = "week") |>
    movedata2networkDynamic()

monthly_network <-
  true_data |>
  coarsen_date(jitter = FALSE, rounding_unit = "month") |>
  movedata2networkDynamic()


##############################################
### Extract monthly maximum reachabilities ###
##############################################

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
  subnetwork <-
    network.extract(network,
                    onset = start, terminus = end,
                    rule = "any",
                    trim.spells = TRUE) #essential: eliminates edge activity
                                        #spells outside the desired time frame!

  reachability_sets <- tReach(subnetwork, graph.step.time = 1)

  max(reachability_sets)

  #path <-
  #  tPath(subnetwork,
  #        v = which(reachability_sets == max(reachability_sets)),
  #        graph.step.time = 1)

  #plot(path)
  #transmissionTimeline(path, jitter = TRUE)
}

monthly_max_reachabilities <- tibble(true = '',
                                     jittered = '',
                                     weekly = '',
                                     monthly = '',
                                     .rows = length(months_in_data))

for (dataset in c("true","jittered","weekly","monthly")){
  network <- eval(parse(text = paste0(dataset,"_network")))
  monthly_max_reachabilities[dataset] <-
    sapply(months_in_data,
           function(x){max_reachability_for_period(network, x[[1]], x[[2]])}
    )
}


#############################
### Reformat for plotting ###
#############################

monthly_max_reachabilities <-
  monthly_max_reachabilities |>
  pivot_longer(everything(),
               names_to = "network",
               values_to = "max_reachability")

#Set network as a factor with specific order (to avoid default alphabetic order)
monthly_max_reachabilities$network <-
  factor(monthly_max_reachabilities$network,
         levels = c("true","jittered","weekly","monthly"))


#####################
### Plot: boxplot ###
#####################

p <-
  ggplot(data = monthly_max_reachabilities,
         aes(x = network, y = max_reachability)) +
  xlab("Movement network") +
  ylab ("Monthly maximum reachability") +
  geom_boxplot()

plot(p)

