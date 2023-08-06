#test everything with non-consecutive holding id's!!

load_all()
library(tidyverse) #
library(sf) #st_as_sf(), st_distance()
library(units) #drop_units()
library(lubridate) #floor_date() ceiling_date

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <-
  "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"
contactpars_outfile <- "tests/testthat/test_input_files/contact_matrix.rds"

load_config(movement_configfile)
from <- movenetenv$options$movedata_cols$from
to <- movenetenv$options$movedata_cols$to
weight <- movenetenv$options$movedata_cols$weight
date <- movenetenv$options$movedata_cols$date

movement_data <-
  movement_datafile %>%
  reformat_data("movement")

load_config(holding_configfile)
id <- movenetenv$options$holdingdata_cols$id
crs <- movenetenv$options$holdingdata_fileopts$coord_EPSG_code
coord_x <- movenetenv$options$holdingdata_cols$coord_x
coord_y <- movenetenv$options$holdingdata_cols$coord_y

holding_data <-
  holding_datafile %>%
  reformat_data("holding")

#replace holding_ids with int-chars
outputs <- holdingids2consecints(movement_data, holding_data, TRUE)
movement_data_intchar <- outputs$movement_data
holding_data_intchar <- outputs$holding_data
datakey <- outputs$key

n_nodes <- length(holding_data[[id]])
node_idnumbers <- sort(as.numeric(holding_data[[id]])) #turns int-chars into numerics

movement_spread_matrix <- create_movement_spread_matrix(movement_data_intchar, n_nodes, TRUE)

#matrix containing daily probabilities of becoming infected through local spread
#using distance look-up-table from DTU-DADS-ASF Table S4 (which in turn is based
#on Boklund et al. (2009), adjusted according to Nigsch et al. (2013) - i.e.
#halved relative to Boklund's estimates for CSF)  -->
#from 0 to 0.1 km: 0.1,
#from 0.1 to 0.5 km: 0.006
#from 0.5 to 1 km: 0.002
#from 1 to 2km: 0.000015
local_spread_tiers <- tibble(lower_boundary = c(0,100,500,1000,2000),
                             upper_boundary = c(100,500,1000,2000,Inf),
                             probability = c(0.1, 0.006, 0.002, 0.000015, 0))

local_spread_matrix <- create_local_spread_matrix(holding_data_intchar, local_spread_tiers)

#combine both matrices into contact_matrix
#SORT THESE MATRICES FIRST, otherwise they might get combined wrong
#probably should check dimensions too
#movement_spread_matrix[order(as.numeric(rownames(movement_spread_matrix))),
#                       order(as.numeric(colnames(movement_spread_matrix)))]
contact_matrix <- movement_spread_matrix + local_spread_matrix

#saveRDS(contact_matrix,
#        contactpars_outfile)


create_movement_spread_matrix <- function(movement_data_intchar, n_nodes,
                                          whole_months = TRUE,
                                          transmission_probability = 1){

  #creates movement matrix with numeric identifiers. Require movement_data with
  #numeric identifiers

  #empty matrix w all possible combinations of holdings in the holding datafile
  #using numeric identifiers
  movement_spread_matrix <-
    matrix(0, n_nodes, n_nodes, dimnames = list(1:n_nodes, 1:n_nodes))

  average_movement_data <-
    average_daily_weights(movement_data_intchar, whole_months = TRUE)

  movement_spread_matrix[cbind(as.numeric(average_movement_data[[1]]), #from
                         as.numeric(average_movement_data[[2]]))] <- #to
    average_movement_data[[3]] #average_daily_weight

  movement_spread_matrix <- movement_spread_matrix * transmission_probability

  return(movement_spread_matrix)
}

average_daily_weights <- function(movement_data, whole_months = TRUE){

  #doesn't assume or require from/to to be in integer/"integer"  format, i.e.
  #accepts any movenet format movement data

  #definition of specific columns based on movenetenv - if using, need to ensure
  #correctly loaded movement config
  # from <- movenetenv$options$movedata_cols$from
  # to <- movenetenv$options$movedata_cols$to
  # date <- movenetenv$options$movedata_cols$date
  # weight <- movenetenv$options$movedata_cols$weight

  #set timeframe of movement data
  if(isTRUE(whole_months)){ #assume movement_data covers whole months
    #if using movenetenv
    # n_days <- as.numeric(ceiling_date(max(movement_data[[date]]),"month") -
    #                        floor_date(min(movement_data[[date]]),"month"))
    #if using column indices
    n_days <- as.numeric(ceiling_date(max(movement_data[[3]]),"month") -
                           floor_date(min(movement_data[[3]]),"month"))
  } else if(isFALSE(whole_months)) { #count days from first to last movement
    #if using movenetenv
    # n_days <- as.numeric(max(movement_data[[date]]) -
    #                        min(movement_data[[date]])) + 1
    #if using column indices
    n_days <- as.numeric(max(movement_data[[3]]) -
                           min(movement_data[[3]])) + 1
  }

  #average number of pigs transported per day between each farm
  #(NB this NAMES the new column average_daily_weight)
  #(NB this drops other columns - output is just from, to, average_daily_weight)
  movement_data %>%
    #if using movenetenv
    #group_by(.data[[from]], .data[[to]]) %>%
    #summarise(average_daily_weight = sum(.data[[weight]])/n_days)
    #if using column indices
    group_by(.[,1], .[,2]) %>%
    summarise(
      average_daily_weight = sum(eval(sym(names(movement_data)[[4]])))/n_days)

    #output is summarised movement tibble with still int-char identifiers
}


create_local_spread_matrix <- function(holding_data_intchar, local_spread_tiers){

  local_spread_matrix <-
    holding_data_intchar %>%
    create_distance_matrix %>%
    drop_units()

  apply(local_spread_tiers, 1, function(tier){
    local_spread_matrix[local_spread_matrix >= tier['lower_boundary'] &
                          local_spread_matrix < tier['upper_boundary']] <<-
      tier['probability']
    #NB this also changes distance 0 to probability of the first tier.
    #How does this work for farm1-to-farm1? Do I need to specifically change 0 to something, first?
  })

  return(local_spread_matrix)
}

create_distance_matrix <- function(holding_data_intchar){

  #does require integer-ish ids

  #check for presence & define coords column(s), either based on movenetenv or
  #based on data type
  id <- movenetenv$options$holdingdata_cols$id
  crs <- movenetenv$options$holdingdata_fileopts$coord_EPSG_code #or use this at reformat data stage
  coord_x <- movenetenv$options$holdingdata_cols$coord_x #or use this at reformat data stage
  coord_y <- movenetenv$options$holdingdata_cols$coord_y #or use this at reformat data stage

  #distance matrix between farms
  distance_matrix <-
    holding_data_intchar %>%
    arrange(as.numeric(holding_data_intchar[[id]])) %>%
    #If not reformatted to sf coordinates yet - else remove the below step
    st_as_sf(coords = c(coord_x, coord_y), #Do this step as part of reformat_data?
             crs = crs) %>%
    #Check whether st_distance works on tibble with a coordinates column with
    #class sf, rather than a tibble that is itself of class sf
    st_distance() #has matrix format output

  return(distance_matrix)
}








