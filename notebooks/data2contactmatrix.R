load_all()
library(tidyverse)
library(sf)

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
movement_configfile <- "ScotEID"
holding_datafile <-
  "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"
contactpars_outfile <- "tests/testthat/test_input_files/contact_matrix.rds"

load_config(movement_configfile)

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

load_config(holding_configfile)

anonymisation_h <-
  holding_datafile |>
  reformat_data("holding") |>
  anonymise("", key=anonymisation_m$key)


#matrix containing average number of pigs transported per day
transport_matrix <-
  anonymisation_m$data %>%
    group_by(departure_cph, dest_cph) %>%
    summarise(prob=sum(qty_pigs)/365) %>%
    ungroup() %>%
    arrange(as.numeric(dest_cph)) %>%
    pivot_wider(names_from = dest_cph,
                values_from = prob,
                values_fill = 0) %>%
    arrange(as.numeric(departure_cph)) %>%
    select(-departure_cph) %>%
    as.matrix(rownames.force = TRUE)

#distance matrix between farms
distance_matrix <-
  anonymisation_h$data %>%
  st_as_sf(coords = c("easting", "northing"),
           crs = movenetenv$options$holdingdata_fileopts$coord_EPSG_code) %>%
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
  distance_matrix %>%
  units::drop_units()

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
