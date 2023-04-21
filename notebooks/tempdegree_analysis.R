##############
### Set-up ###
##############
library(lubridate) #wday
library(tidyverse) #tibble,str_split,str_c
library(pbapply)
library(tsna)  #tPath
library(networkDynamic)  #get.vertex.id
library(parallel)
library(movenet)
library(stringr)


# movement_datafile <-
#   "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
# movement_configfile <- "ScotEID"
# load_config(movement_configfile)
verbose <- TRUE

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

#How much jitter & which rounding, for fig 1 on monthly max/med temporal degree?
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
  anonymise("") |> #anonymise Danish data
  getElement(1)

true_network <- movedata2networkDynamic(true_data)

if(verbose) cat("Start jitter_networks at", as.character(Sys.time()), "\n")

create_jitter_networks <- function(jitter_set, n_sim, n_threads, ...){
  #To generalise into single general jitter/rounding function for package, need
  #to mind the option to apply both jitter AND rounding. How is this best
  #parallelised? Or otherwise need to limit to selecting jitter OR rounding.
  #Also: mind current dependence on "true_data" in global environment!
  #Also: clusterExport from different environments (e.g. "data"/arguments from
  #calling env, others from global env)

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("true_data", "movement_configfile",
                      "has_element", "floor_date", "select", "networkDynamic"))
  clusterEvalQ(cl, {
    library(movenet)
    library(checkmate)
    library(dplyr)
    load_config(movement_configfile)
  })
  jitter_networks <-
    pblapply(rep(jitter_set, n_sim),
             function(x){coarsen_date(true_data, jitter = x,
                                      rounding_unit = FALSE, ...) |>
                         movedata2networkDynamic()},
             cl = cl)
  return(jitter_networks)
}

jitter_networks <- create_jitter_networks(jitter_set, n_sim, n_threads)
names(jitter_networks) <- paste0("jitter (",rep(jitter_set, n_sim)," days)")


if(verbose) cat("Start rounding networks at", as.character(Sys.time()), "\n")

create_rounding_networks <- function(round_set, n_threads, ...){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("true_data", "movement_configfile", "week_start",
                      "has_element", "floor_date", "select", "networkDynamic"))
  clusterEvalQ(cl, {
    library(movenet)
    library(checkmate)
    library(dplyr)
    load_config(movement_configfile)
  })
  rounding_networks <-
    pblapply(round_set,
             function(x){coarsen_date(true_data, jitter = FALSE,
                                      rounding_unit = x,
                                      week_start = week_start, ...) |>
                         movedata2networkDynamic()},
             cl = cl)
  return(rounding_networks)
}

week_start <- wday(min(true_data[[date_col]]))
rounding_networks <- create_rounding_networks(round_set, n_threads)
names(rounding_networks) <- paste0(round_set,"ly")

##############################################################
### Fig 1 prep: Extract monthly temp. degree summary stats ###
##############################################################

if(verbose) cat("Start fig 1 prep at", as.character(Sys.time()), "\n")

months_in_data <-
  extract_periods(true_data[[date_col]], "month")

#Extract monthly networks
selected_networks <- c(true=list(true_network),
                       jitter_networks[names(jitter_networks) %in%
                                       paste0("jitter (",jitter_mmr," days)")],
                       rounding_networks[paste0(round_mmr,"ly")])

monthly_networks <- extract_periodic_subnetworks(selected_networks, n_threads,
                                                 months_in_data)


#Extract monthly temp. degree summary stats
cat("Running temporal degree analysis in parallel at ", as.character(Sys.time()), "\n")

monthly_summ_temp_degree_with_id <-
  unlist(monthly_networks, recursive = FALSE) |>
  parallel_summarise_temporal_node_properties(n_threads,
                                              "temporal degree",
                                              list(median = median, max = max),
                                              identify_nodes = TRUE)

monthly_max_temp_degree <-
  sapply(seq_along(monthly_summ_temp_degree_with_id),
         function(x){monthly_summ_temp_degree_with_id[[x]][[1]][["max"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_med_temp_degree <-
  sapply(seq_along(monthly_summ_temp_degree_with_id),
         function(x){monthly_summ_temp_degree_with_id[[x]][[1]][["median"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_max_temp_degree_nodes <-
  sapply(seq_along(monthly_summ_temp_degree_with_id),
         function(x){monthly_summ_temp_degree_with_id[[x]][[2]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(value = names(selected_networks))

#Extract monthly temp. indegree summary stats
cat("Running temporal indegree analysis in parallel at ", as.character(Sys.time()), "\n")

monthly_summ_temp_indegree_with_id <-
  unlist(monthly_networks, recursive = FALSE) |>
  parallel_summarise_temporal_node_properties(n_threads,
                                              "temporal indegree",
                                              list(median = median, max = max),
                                              identify_nodes = TRUE)

monthly_max_temp_indegree <-
  sapply(seq_along(monthly_summ_temp_indegree_with_id),
         function(x){monthly_summ_temp_indegree_with_id[[x]][[1]][["max"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_med_temp_indegree <-
  sapply(seq_along(monthly_summ_temp_indegree_with_id),
         function(x){monthly_summ_temp_indegree_with_id[[x]][[1]][["median"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_max_temp_indegree_nodes <-
  sapply(seq_along(monthly_summ_temp_indegree_with_id),
         function(x){monthly_summ_temp_indegree_with_id[[x]][[2]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(value = names(selected_networks))

#Extract monthly temp. outdegree summary stats
cat("Running temporal outdegree analysis in parallel at ", as.character(Sys.time()), "\n")

monthly_summ_temp_outdegree_with_id <-
  unlist(monthly_networks, recursive = FALSE) |>
  parallel_summarise_temporal_node_properties(n_threads,
                                              "temporal outdegree",
                                              list(median = median, max = max),
                                              identify_nodes = TRUE)

monthly_max_temp_outdegree <-
  sapply(seq_along(monthly_summ_temp_outdegree_with_id),
         function(x){monthly_summ_temp_outdegree_with_id[[x]][[1]][["max"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_med_temp_outdegree <-
  sapply(seq_along(monthly_summ_temp_outdegree_with_id),
         function(x){monthly_summ_temp_outdegree_with_id[[x]][[1]][["median"]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(names(selected_networks))

monthly_max_temp_outdegree_nodes <-
  sapply(seq_along(monthly_summ_temp_outdegree_with_id),
         function(x){monthly_summ_temp_outdegree_with_id[[x]][[2]]}) |>
  matrix(ncol = length(monthly_networks), nrow = length(months_in_data)) |>
  as_tibble() |>
  `colnames<-`(value = names(selected_networks))



# #Get some example paths for plotting
# #N.B this only includes edges going out from the identified vertex, and misses
# #the edges coming into the vertex -> plotting shows outdegree
# fwd_paths_month1 <-
#    lapply(seq_along(monthly_networks),
#           function(x){
#             n <- 1+(x-1)*length(months_in_data)
#             tPath(monthly_networks[[x]][[1]],
#                   v = get.vertex.id(monthly_networks[[x]][[1]],
#                                     monthly_summ_temp_degree_with_id[[n]][["node_pid_with_max_value"]][1]),
#                   graph.step.time = 1)})
# names(fwd_paths_month1) <- names(selected_networks)
# #The below determines incoming edges for same nodes in same networks -> plotting
# #shows indegree
# bkwd_paths_month1 <-
#   lapply(seq_along(monthly_networks),
#          function(x){
#            n <- 1+(x-1)*length(months_in_data)
#            tPath(monthly_networks[[x]][[1]],
#                  direction = "bkwd", type = "latest.depart",
#                  v = get.vertex.id(monthly_networks[[x]][[1]],
#                                    monthly_summ_temp_degree_with_id[[n]][["node_pid_with_max_value"]][1]),
#                  graph.step.time = 1)})
# names(bkwd_paths_month1) <- names(selected_networks)


# #Calculate percentage correctly identified nodes w max degree
# pct_correct_node_id <-
#   monthly_max_temp_degree_nodes |>
#   #Is the monthly maximal-degree node correctly id'ed in modified networks?
#   #(If the true network has >1 max-degree node for a particular month, the
#   #result is TRUE if *at least one of these nodes* is correctly identified.)
#   apply(1,
#         function(x){sapply(x, function(y){any(y %in% x[[1]])})}) |>
#   #calculate percentage
#   rowSums() %>%
#   {.*100/length(months_in_data)} |>
#   as.list() %>%
#   as_tibble(.name_repair="minimal")


############################################################
### Fig 1: boxplot of monthly max/median temporal degree ###
############################################################

# violinplot_monthly_measures(monthly_max_temp_degree, "maximum temporal degree")
# violinplot_monthly_measures(monthly_med_temp_degree, "median temporal degree")

##########################################################################
### Fig 1b: % correct identification of (at least one) max-degree node ###
##########################################################################

# p <-
#   violinplot_monthly_measures(
#     pct_correct_node_id,
#     "nodes with max temp degree: consistency with true network (%)") +
#   ylim(0,100)
# plot(p)

##############################################################
### Fig 2 prep: Extract overall max/median temporal degree ###
##############################################################

if(verbose) cat("Start fig 2 prep (jitter measures) at", as.character(Sys.time()), "\n")

jitter_measures <- tibble(jitter = rep(jitter_set,n_sim),
                          max_temp_degree = "",
                          med_temp_degree = "",
                          max_indegree = "",
                          med_indegree = "",
                          max_outdegree = "",
                          med_outdegree = "")

jitter_summ_degree <-
  parallel_summarise_temporal_node_properties(jitter_networks, n_threads,
                                              "temporal degree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
jitter_measures[, "max_temp_degree"] <-
  sapply(seq_along(jitter_summ_degree),
         function(x){jitter_summ_degree[[x]][[1]][["max"]]})
jitter_measures[, "med_temp_degree"] <-
  sapply(seq_along(jitter_summ_degree),
         function(x){jitter_summ_degree[[x]][[1]][["median"]]})

jitter_summ_indegree <-
  parallel_summarise_temporal_node_properties(jitter_networks, n_threads,
                                              "temporal indegree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
jitter_measures[, "max_indegree"] <-
  sapply(seq_along(jitter_summ_indegree),
         function(x){jitter_summ_indegree[[x]][[1]][["max"]]})
jitter_measures[, "med_indegree"] <-
  sapply(seq_along(jitter_summ_indegree),
         function(x){jitter_summ_indegree[[x]][[1]][["median"]]})

jitter_summ_outdegree <-
  parallel_summarise_temporal_node_properties(jitter_networks, n_threads,
                                              "temporal outdegree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
jitter_measures[, "max_outdegree"] <-
  sapply(seq_along(jitter_summ_outdegree),
         function(x){jitter_summ_outdegree[[x]][[1]][["max"]]})
jitter_measures[, "med_outdegree"] <-
  sapply(seq_along(jitter_summ_outdegree),
         function(x){jitter_summ_outdegree[[x]][[1]][["median"]]})


if(verbose) cat("Start fig 2 prep (round measures) at", as.character(Sys.time()), "\n")

round_measures <- tibble(round = c(1,7,30.4,60.8,91.3,182.5,365),
                         max_temp_degree = "",
                         med_temp_degree = "",
                         max_indegree = "",
                         med_indegree = "",
                         max_outdegree = "",
                         med_outdegree = "")
round_summ_degree <-
  parallel_summarise_temporal_node_properties(rounding_networks, n_threads,
                                              "temporal degree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
round_measures[, "max_temp_degree"]<-
  sapply(seq_along(round_summ_degree),
         function(x){round_summ_degree[[x]][[1]][["max"]]})
round_measures[, "med_temp_degree"] <-
  sapply(seq_along(round_summ_degree),
         function(x){round_summ_degree[[x]][[1]][["median"]]})

round_summ_indegree <-
  parallel_summarise_temporal_node_properties(rounding_networks, n_threads,
                                              "temporal indegree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
round_measures[, "max_indegree"]<-
  sapply(seq_along(round_summ_indegree),
         function(x){round_summ_indegree[[x]][[1]][["max"]]})
round_measures[, "med_indegree"] <-
  sapply(seq_along(round_summ_indegree),
         function(x){round_summ_indegree[[x]][[1]][["median"]]})

round_summ_outdegree <-
  parallel_summarise_temporal_node_properties(rounding_networks, n_threads,
                                              "temporal outdegree",
                                              list(median = median, max = max),
                                              identify_nodes = FALSE)
round_measures[, "max_outdegree"]<-
  sapply(seq_along(round_summ_outdegree),
         function(x){round_summ_outdegree[[x]][[1]][["max"]]})
round_measures[, "med_outdegree"] <-
  sapply(seq_along(round_summ_outdegree),
         function(x){round_summ_outdegree[[x]][[1]][["median"]]})

##############################################################################
### Fig 2: Max/median temp degree for a diverse range of jitter & rounding ###
##############################################################################

#plot_measure_over_anonymisation_gradient(jitter_measures, "Max temporal degree", "jitter")
#plot_measure_over_anonymisation_gradient(jitter_measures[,c(1,3)], "Median temporal degree", "jitter")
#plot_measure_over_anonymisation_gradient(round_measures, "Max temporal degree", "rounding")
#plot_measure_over_anonymisation_gradient(round_measures[,c(1,3)], "Median temporal degree", "rounding")

## Specific code for saving outputs on Matt's machine:
if(movement_configfile == "Denmark_processed"){
  save(
    monthly_max_temp_degree,
    monthly_med_temp_degree,
    monthly_max_temp_degree_nodes,
    monthly_max_temp_indegree,
    monthly_med_temp_indegree,
    monthly_max_temp_indegree_nodes,
    monthly_max_temp_outdegree,
    monthly_med_temp_outdegree,
    monthly_max_temp_outdegree_nodes,
    jitter_measures,
    round_measures,
    file=str_c("mra_outputs_tempdegree_dk_", year_range, ".rda")
  )

} else if(movement_configfile == "ScotEID"){
  save(
    monthly_max_temp_degree,
    monthly_med_temp_degree,
    monthly_max_temp_degree_nodes,
    monthly_max_temp_indegree,
    monthly_med_temp_indegree,
    monthly_max_temp_indegree_nodes,
    monthly_max_temp_outdegree,
    monthly_med_temp_outdegree,
    monthly_max_temp_outdegree_nodes,
    jitter_measures,
    round_measures,
    file="test_tempdegree.rda"
  )
}

if(verbose) cat("Finished at", as.character(Sys.time()), "\n")

