##############
### Set-up ###
##############
library(lubridate) #wday
library(tidyverse) #tibble,str_split,str_c
library(pbapply)
library(tsna)  #tPath
library(networkDynamic)  #get.vertex.id
library(movenet)

#load_all()

#movement_datafile <-
#  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
#movement_configfile <- "ScotEID"
#load_config(movement_configfile)
verbose <- FALSE

movement_configfile <- "Denmark_processed"
load_config(movement_configfile)

## Specific code for running outputs on Matt's machine:
if(movement_configfile == "Denmark_processed"){
  if(length(find("year_range"))==0L){
    year_range <- "2020_2020"
  }
  dates <- as.Date(paste0(str_split(year_range, "_")[[1]], c("-01-01","-12-31")))
  stopifnot(length(dates)==2L)

  # Danish data, available to Matt only:
  readRDS("/Users/matthewdenwood/Documents/Research/Projects/DigiVet/CS2/DK_pig_movements/pig_movements_anon_2016_2021.rds") |>
    filter(DATO_FLYTNING >= dates[1], DATO_FLYTNING <= dates[2]) |>
    mutate(ID=1:n()) |>
    select(ID, DATO_FLYTNING, CHRNR_AFSENDER=ACHR_FROM, CHRNR_MODTAGER=ACHR_TO, ANTAL_FLYT_DYR) ->
  tdata

  stopifnot(nrow(tdata) > 0L)
  movement_datafile <- tempfile()
  write_csv(tdata, file=movement_datafile)
  Sys.sleep(2L) # Some time to finish writing the file
  stopifnot(file.exists(movement_datafile))

  pboptions(type="txt")
  cat("Running", year_range, "...\n")
  verbose <- TRUE
}

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

if(verbose) cat("Beginning analysis at", as.character(Sys.time()), "\n")

#reformat movement data
true_data <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("") |>
  getElement(1)

true_network <- movedata2networkDynamic(true_data)

if(verbose) cat("Start jitter_networks at", as.character(Sys.time()), "\n")

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
#if(FALSE){
#  clusterExport(cl, "movement_datafile")
#  clusterEvalQ(cl, {
#    library("movenet")
#    load_config(movement_configfile)
#  })
#}
## This isn't necessary on Fork clusters, but these are not available on Windows

if(verbose) cat("Start rounding networks at", as.character(Sys.time()), "\n")

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

if(verbose) cat("Start fig 1 prep at", as.character(Sys.time()), "\n")

months_in_data <-
  extract_periods(true_data[[date_col]], "month")

#Extract monthly max reachabilities
selected_networks <- c(true=list(true_network),
                       jitter_networks[names(jitter_networks) %in%
                                       paste0("jitter (",jitter_mmr," days)")],
                       rounding_networks[paste0(round_mmr,"ly")])

monthly_networks <- extract_periodic_subnetworks(selected_networks, n_threads,
                                                 months_in_data)

monthly_max_reachabilities <- tibble(.rows = length(months_in_data))
monthly_max_reaching_nodes <- tibble(.rows = length(months_in_data))
max_reach_paths_month1 <- list()
for (netw_ind in seq_along(monthly_networks)){
  cat("Running network ", netw_ind, " of ", length(monthly_networks), " at ", as.character(Sys.time()), "...\n", sep="")
  network <- monthly_networks[[netw_ind]]
  max_reachabilities_with_ids <-
    parallel_max_reachabilities_with_id(network, n_threads)
  monthly_max_reachabilities[as.character(netw_ind)] <-
    sapply(max_reachabilities_with_ids,"[[",1)
  monthly_max_reaching_nodes[[netw_ind]] <-
    sapply(max_reachabilities_with_ids,"[[",2)
  max_reach_paths_month1[[netw_ind]] <-
    tPath(network[[1]],
          v = get.vertex.id(network[[1]],
                            max_reachabilities_with_ids[[1]][[2]][1]),
          graph.step.time = 1)
}
colnames(monthly_max_reachabilities) <- names(selected_networks)
colnames(monthly_max_reaching_nodes) <- names(selected_networks)
names(max_reach_paths_month1) <- names(selected_networks)

# monthly_max_temp_degree <- tibble(.rows = length(months_in_data))
# monthly_mean_temp_degree <- tibble(.rows = length(months_in_data))
# monthly_median_temp_degree <- tibble(.rows = length(months_in_data))
# for (netw_ind in seq_along(monthly_networks)){
#   cat("Running network ", netw_ind, " of ", length(monthly_networks), "...\n", sep="")
#   network <- monthly_networks[[netw_ind]]
#   monthly_max_temp_degree[as.character(netw_ind)] <-
#     parallel_max_reachabilities(network, n_threads)
# }
# colnames(monthly_max_reachabilities) <- names(selected_networks)
# max(colSums(deg))
# mean(colSums)

########################################################
### Fig 1: boxplot of monthly maximum reachabilities ###
########################################################

# violinplot_monthly_measures(monthly_max_reachabilities, "maximum reachability")
#
#
##########################################################
### Fig 2 prep: Extract overall maximum reachabilities ###
##########################################################

if(verbose) cat("Start fig 2 prep (jitter measures) at", as.character(Sys.time()), "\n")

jitter_measures <- tibble(jitter = rep(jitter_set,n_sim),
                          max_reachability = "")
jitter_measures[, "max_reachability"] <-
  parallel_max_reachabilities(jitter_networks, n_threads)

if(verbose) cat("Start fig 2 prep (round measures) at", as.character(Sys.time()), "\n")

round_measures <- tibble(round = c(1,7,30.4,60.8,91.3,182.5,365),
                         max_reachability = "")
round_measures[, "max_reachability"] <-
  parallel_max_reachabilities(rounding_networks, n_threads)

# ##########################################################################
# ### Fig 2: Max reachabilities for a diverse range of jitter & rounding ###
# ##########################################################################
# plot_measure_over_anonymisation_gradient(jitter_measures, "Max reachability", "jitter")
# plot_measure_over_anonymisation_gradient(round_measures, "Max reachability", "rounding")

## Specific code for saving outputs on Matt's machine:
if(movement_configfile == "Denmark_processed"){
  save(
    monthly_max_reachabilities,
    monthly_max_reaching_nodes,
    max_reach_paths_month1,
    jitter_measures,
    round_measures,
    file=str_c("mra_outputs_dk_", year_range, ".rda")
  )

}

if(verbose) cat("Finished at", as.character(Sys.time()), "\n")
