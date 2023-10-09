#test everything with non-consecutive holding id's!!

load_all()
library(tidyverse) #dplyr arrange
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

n_nodes <- length(holding_data_intchar[[id]])
node_idnumbers <- sort(as.numeric(holding_data_intchar[[id]])) #turns int-chars into numerics

movement_spread_matrix <- create_movement_spread_matrix(movement_data_intchar,
                                                        n_nodes, TRUE,
                                                        transmission_probability)

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
local_spread_probabilities_ASF_Halasa_et_al_2016 <-local_spread_tiers
save(local_spread_probabilities_ASF_Halasa_et_al_2016,
     file="inst/extdata/local_spread_probabilities_ASF_Halasa_et_al_2016.Rdata")

local_spread_matrix <- create_local_spread_matrix(holding_data_intchar, local_spread_tiers)

#combine both matrices into contact_matrix
#SORT THESE MATRICES FIRST, otherwise they might get combined wrong
#probably should check dimensions too
#movement_spread_matrix[order(as.numeric(rownames(movement_spread_matrix))),
#                       order(as.numeric(colnames(movement_spread_matrix)))]
contact_matrix <- movement_spread_matrix + local_spread_matrix

#saveRDS(contact_matrix,
#        contactpars_outfile)










