#test everything with non-consecutive holding id's!!

load_all()
library(tidyverse) #
library(sf) #st_as_sf(), st_distance()
library(units) #drop_units()

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

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

load_config(holding_configfile)
id <- movenetenv$options$holdingdata_cols$id
crs <- movenetenv$options$holdingdata_fileopts$coord_EPSG_code
coord_x <- movenetenv$options$holdingdata_cols$coord_x
coord_y <- movenetenv$options$holdingdata_cols$coord_y

anonymisation_h <-
  holding_datafile |>
  reformat_data("holding") |>
  anonymise("", key = anonymisation_m$key)


#empty df w all possible combinations of holdings in the holding datafile
holding_df <-
  expand.grid(anonymisation_h$data[[id]], anonymisation_h$data[[id]]) |>
  `colnames<-`(c(from, to))

#matrix containing average number of pigs transported per day
transport_matrix <-
  anonymisation_m$data %>%
    group_by(.data[[from]], .data[[to]]) %>%
    summarise(prob = sum(.data[[weight]])/365) %>%
    ungroup() %>%
    right_join(holding_df, by=c(from, to)) %>%
    replace_na(list(prob=0)) %>%
    arrange(as.numeric(.data[[to]])) %>%
    pivot_wider(names_from = all_of(to),
                values_from = prob) %>%
    arrange(as.numeric(.data[[from]])) %>%
    column_to_rownames(from) %>%
    as.matrix()

#distance matrix between farms
distance_matrix <-
  anonymisation_h$data |>
  st_as_sf(coords = c(coord_x, coord_y),
           crs = crs) |>
  st_distance()

#matrix containing daily probabilities of becoming infected through local spread
#using distance look-up-table from DTU-DADS-ASF Table S4 (which in turn is based
#on Boklund et al. (2009), adjusted according to Nigsch et al. (2013) - i.e.
#halved relative to Boklund's estimates for CSF)  -->
#from 0 to 0.1 km: 0.1,
#from 0.1 to 0.5 km: 0.006
#from 0.5 to 1 km: 0.002
#from 1 to 2km: 0.000015
local_spread <-
  distance_matrix |>
  drop_units()

local_spread[local_spread > 0 & local_spread < 100] <- 0.1
local_spread[local_spread >= 100 & local_spread < 500] <- 0.006
local_spread[local_spread >= 500 & local_spread < 1000] <- 0.002
local_spread[local_spread >= 1000 & local_spread < 2000] <- 0.000015
local_spread[local_spread >= 2000] <- 0

#combine both matrices into contact_matrix
contact_matrix <- transport_matrix + local_spread

#transpose?

#saveRDS(contact_matrix,
#        contactpars_outfile)
