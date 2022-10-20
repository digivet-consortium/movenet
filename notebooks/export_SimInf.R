###############################################################################
### Example movement input format for SimInf
#from: https://rdrr.io/cran/SimInf/f/vignettes/scheduled-events.Rmd
#more info: https://rdrr.io/cran/SimInf/man/SimInf_events.html
###############################################################################

events <- data.frame(
  event      = rep("extTrans", 6),  ## Event "extTrans" is a movement between nodes. Can also be an int (3L for "extTrans")
  time       = c(1, 1, 2, 2, 3, 3), ## The time that the event happens. Can be int or Date. -> date
  node       = c(3, 3, 1, 4, 3, 4), ## In which node does the event occur. Integer. -> from  [anonymise with prefix = '', turn into int]
  dest       = c(4, 2, 3, 3, 2, 2), ## Which node is the destination node  Integer. -> to  [anonymise with prefix = '', turn into int]
  n          = c(9, 2, 8, 3, 5, 4), ## How many individuals are moved  -> if weight is int [what if it isnt?]
  proportion = c(0, 0, 0, 0, 0, 0), ## This is not used when n > 0
  select     = c(4, 4, 4, 4, 4, 4), ## Use the 4th column in the model select matrix
  shift      = c(0, 0, 0, 0, 0, 0)) ## Not used in this example

###############################################################################
### Node coords in SimInf
###############################################################################

#SimInf spatial models use distance matrices

data("nodes", package = "SimInf") #Example nodes dataset - just 2 columns, x and y, with projected coordinates
distance_matrix(nodes$x, nodes$y, cutoff = 2500, min_dist = NULL) #This is how SimInf calculates distance matrices between nodes


###############################################################################
### Questions
###############################################################################

# assume all movements are extTrans
# Do node numbers need to be in order of movement dates (can first move be from node 432 to 5, or does it need to involve node 1?)?
# what if weight isn't int?
# what about a network of probabilities?

# Can one build a model with both direct tm + indirect tm (SEIR-Sp)?
# Does this require explicit definition of an environmental compartment and various
# rates via mparse?
# how to add spatial coupling & distance matrix if not via args?

#Use existing DTU-DADS-ASF model (complicated, does not use SimInf) or make own?
# What structure for modelling within a farm?
# - susceptible-latent-subclinical-clinical-removed (SLSCR) as in DTU-DADS-ASF
#   (https://www.sciencedirect.com/science/article/pii/S0378113516302097)
# - SEID (https://www.sciencedirect.com/science/article/pii/S0022519321002174)

# Add natural births / deaths?
# Add different holding types? (complicated)


###############################################################################
### Transform Movenet movement data to SimInf event format
###############################################################################

library(dplyr) #for arrange and transmute
load_all()
movement_data <- reformat_data("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv",
                      "movement")
anonymised_data <-
  movement_data |>
    anonymise("")

events <-
  anonymised_data$data |>
    transmute(event="extTrans", #for movements between nodes
              time=.data[[movenetenv$options$movedata_cols$date]],
              node=as.integer(.data[[movenetenv$options$movedata_cols$from]]),
              dest=as.integer(.data[[movenetenv$options$movedata_cols$to]]),
              n=.data[[movenetenv$options$movedata_cols$weight]],
              proportion=0, #alternative to n (set n as 0)
              select=3, #col in model select matrix (which comp to sample from)
              shift=0) #col in model shift matrix (if want to shift compartment)

###############################################################################
### Transform Movenet holding data to SimInf nodes format
###############################################################################

load_config("tests/testthat/test_input_files/fakeScotEID_holding.yml")
holding_data <- reformat_data("tests/testthat/test_input_files/test_holdingdata_generic.csv",
                               "holding")

anonymised_holding_data <-
  holding_data |>
  anonymise("", key=anonymised_data$key)

nodes <-
  anonymised_holding_data$data |>
    arrange(as.numeric(movenetenv$options$holdingdata_cols$id)) |>
    transmute(id = .data[[movenetenv$options$holdingdata_cols$id]],
              x = .data[[movenetenv$options$holdingdata_cols$coord_x]],
              y = .data[[movenetenv$options$holdingdata_cols$coord_y]],
              size = .data[[movenetenv$options$holdingdata_cols$herd_size]])



