library(networkDynamic)
library(tsna)
library(ndtv)

load_all()

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"

load_config(movement_configfile)

#reformat movement data to have node ids and dates as numbers
anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

data_as_ints <-
  anonymisation_m$data[,c(3,3,1,2)] |> #date (onset), date (terminus), from, to
  lapply(as.integer) |>
  data.frame()

#create dynamic network from timed edges
temporal_network <- networkDynamic(edge.spells = data_as_ints)

#forward path metrics
#(see https://statnet.org/Workshops/ndtv_workshop.html#forward-path-metrics)

#reachable set for each node, if each move takes a full day
reachable_sets <-
  tReach(temporal_network, direction = "fwd", graph.step.time = 1)

max_reachable_sets <- max(reachable_sets)

#plot earliest forward path for node with min reachable set
path <-
  temporal_network |>
  tPath(v = which(reachable_sets == min(reachable_sets)),
        graph.step.time = 1)

plot(path)

#plot transmission timeline for node with min reachable set
transmissionTimeline(path, jitter = TRUE,
                     main="Earliest forward path, for the node with the smallest
                     reachable set")
