###############################################################################
### Example input format for SimInf
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
  select     = c(4, 4, 4, 4, 4, 4), ## Use the 4th column in
                                    ## the model select matrix
  shift      = c(0, 0, 0, 0, 0, 0)) ## Not used in this example

###############################################################################
### Questions
###############################################################################

# assume all movements are extTrans
# Do node numbers need to be in order of movement dates (can first move be from node 432 to 5, or does it need to involve node 1?)?
# what if weight isn't int?
# What is select? Should this always be 4 or set as user-defined argument?
# What is shift? Should this always be 0 or set as user-defined argument?

#Is there a mechanism of adding node data, incl. coordinates, to SimInf?
#Or only a distance matrix?

###############################################################################
### Transform Movenet movement data to SimInf event format
###############################################################################
library(dplyr) #for arrange and transmute
load_all()
data <- reformat_data("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv",
                      "movement")
events <-
  data |>
    arrange(departure_date) |>  # Sort movements by date. May be redundant
    anonymise("") |>            # Change holding identifier to number only
    transmute(event="extTrans",
           time=departure_date,
           node=as.integer(.data[[movenetenv$options$movedata_cols$from]]),
           dest=as.integer(.data[[movenetenv$options$movedata_cols$to]]),
           n=.data[[movenetenv$options$movedata_cols$weight]],
           proportion=0,
           select=4,
           shift=0)

#Need to update anonymise() to also output key for original_id:anonymised_id transformation