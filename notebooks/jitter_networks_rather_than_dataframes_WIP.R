movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"

#reformat movement data
true_data <-
  movement_datafile |>
  reformat_data("movement")

true_network <- movedata2networkDynamic(true_data)

test_data <- head(true_data)
test_data[7,] <- test_data[6,]
test_data[7,3] <- test_data[[7,3]]+1
test_data[7,4] <- 100
test_data[7,5] <- 15891

test_network <- movedata2networkDynamic(test_data)


#Checked that two movements that go out on consecutive days, are shown as two
#lines. Hence terminus should remain the same as onset, and can just extract and
#change onsets
#N.B.: don't use as.data.frame(network), as this doesn't show movements on the last day of the observation period!!


#Change assert on data

#Extract onsets
onsets <- get.edge.activity(test_network, as.spellList = TRUE)[[1]]
termini <- get.edge.activity(test_network, as.spellList = TRUE)[[2]]
#Change onsets to dates
onset_dates <- as.Date(onsets) #base: N.B. this requires explicit setting of origin!
onset_dates <- as_date(onsets) #lubridate: N.B. default origin = lubridate::origin = Unix epoch of "1970-01-01"
#Deactivate edges (deactivate.edges)
#Activate edges at new times
  #Have to do this separately for multiple spells of the same edge
#Change times of each attribute activity (set.edge.attribute)
  #Need to be careful that repeated spells for the same edge, are given in correct order.
  #If jittering results in two movements being reversed in order, the list of *values* also needs to be changed in order.
