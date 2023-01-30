##############
### Set-up ###
##############
library(lubridate) #wday
library(tibble) #tibble
library(pbapply)
library(movenet)

#load_all()

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
load_config(movement_configfile)

# Danish data, available to Matt only:
# movement_datafile <- "/Users/matthewdenwood/Documents/Research/Projects/DigiVet/CS2/DK_pig_movements/svine_flytninger_2018_2020.csv"
movement_datafile <- "/Users/matthewdenwood/Documents/Research/Projects/DigiVet/CS2/DK_pig_movements/svine_flytninger_2020.csv"
movement_configfile <- "Denmark_processed"
load_config(movement_configfile)

date_col <- movenet:::movenetenv$options$movedata_cols$date

#jitter_set <- c(0:10) #more sensible values
jitter_set <- c(0, 4, 15, 30, 46, 91, 183) #corresponding to half rounding units
n_sim <- 5 #How many simulations to do for each jitter value?
round_set <- c("day", "week", "month", "bimonth", "quarter", "halfyear", "year")
#Do we want any jitter + rounding? does not seem particularly sensible, but
#depends on relative values

#How much jitter & which rounding, for fig 1 on monthly max reachabilities?
#Make sure these are included in jitter_set and round_set above
jitter_mmr <- c(4, 15)
round_mmr <- c("week", "month")

#Parallelisation
n_threads <- ifelse(movement_configfile == "Denmark_processed", 10, 4)

#######################################
### Reformat data & create networks ###
#######################################

#reformat movement data, anonymise to have node ids as numbers (required!)
anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

true_data <- anonymisation_m$data

true_network <- movedata2networkDynamic(true_data)

jitter_networks <-
  pblapply(rep(jitter_set, n_sim), function(x){
    coarsen_date(true_data, jitter = x, rounding_unit = FALSE) |>
      movedata2networkDynamic()
  })
names(jitter_networks) <- paste0("jitter (",rep(jitter_set, n_sim)," days)")

# create_rounding_networks <- function(round_set, n_threads, ...){
#   cl <- makeCluster(n_threads)
#   on.exit(stopCluster(cl))
#   clusterExport(cl, c("coarsen_date", "movedata2networkDynamic",
#                       "has_element", "floor_date", "select", "networkDynamic",
#                       "movenetenv"))
#   clusterExport(cl, ...names(), envir = environment())
#   clusterEvalQ(cl, {
#     library(checkmate)
#     library(dplyr)
#     #print(has_element(names(movenetenv$options), "movedata_cols"))
#   })
#   rounding_networks <-
#     parLapply(cl, round_set,
#               function(x){coarsen_date(rounding_unit = x, ...) |>
#                   movedata2networkDynamic()})
#   return(rounding_networks)
# }
#movenetenv is passed on to cluster (confirmed!), but somehow it raises error
#as if config file not loaded

## Comment from Matt:  on a socket type cluster the R sessions loaded are "fresh",
## which is why the config file is not loaded. If you do the following it should
## hopefully work (untested):
if(FALSE){
  clusterExport(cl, "movement_datafile")
  clusterEvalQ(cl, {
    library("movenet")
    load_config(movement_configfile)
  })
}
## This isn't necessary on Fork clusters, but these are not available on Windows

# create_rounding_networks(round_set, n_threads, data = true_data,
#                          jitter = FALSE, week_start = week_start)

week_start <- wday(min(true_data[[date_col]]))
rounding_networks <-
  pblapply(round_set, function(x){
    coarsen_date(true_data, jitter = FALSE, rounding_unit = x,
                 week_start = week_start) |>
      movedata2networkDynamic()
  })
names(rounding_networks)<-paste0(round_set,"ly")

##########################################################
### Fig 1 prep: Extract monthly maximum reachabilities ###
##########################################################

months_in_data <-
  extract_months(true_data[[date_col]])

#Extract monthly max reachabilities
selected_networks <- c(true=list(true_network),
                       jitter_networks[names(jitter_networks) %in%
                                       paste0("jitter (",jitter_mmr," days)")],
                       rounding_networks[paste0(round_mmr,"ly")])

monthly_networks <- extract_monthly_networks(selected_networks, n_threads,
                                             months_in_data)

monthly_max_reachabilities <- tibble(.rows = length(months_in_data))
for (netw_ind in seq_along(monthly_networks)){
  cat("Running network ", netw_ind, " of ", length(monthly_networks), "...\n", sep="")
  network <- monthly_networks[[netw_ind]]
  monthly_max_reachabilities[as.character(netw_ind)] <-
    parallel_max_reachabilities(network, n_threads)
}
colnames(monthly_max_reachabilities) <- names(selected_networks)

########################################################
### Fig 1: boxplot of monthly maximum reachabilities ###
########################################################

violinplot_monthly_measures(monthly_max_reachabilities, "maximum reachability")


##########################################################
### Fig 2 prep: Extract overall maximum reachabilities ###
##########################################################
jitter_measures <- tibble(jitter = rep(jitter_set,n_sim),
                          max_reachability = "")
jitter_measures[, "max_reachability"] <-
  parallel_max_reachabilities(jitter_networks, n_threads)

round_measures <- tibble(round = c(1,7,30.4,60.8,91.3,182.5,365),
                         max_reachability = "")
round_measures[, "max_reachability"] <-
  parallel_max_reachabilities(rounding_networks, n_threads)

##########################################################################
### Fig 2: Max reachabilities for a diverse range of jitter & rounding ###
##########################################################################
plot_measure_over_anonymisation_gradient(jitter_measures, "Max reachability", "jitter")
plot_measure_over_anonymisation_gradient(round_measures, "Max reachability", "rounding")
