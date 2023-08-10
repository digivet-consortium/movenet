holding_datafile <-
  "tests/testthat/test_input_files/test_holdingdata_generic.csv"
holding_configfile <- "tests/testthat/test_input_files/fakeScotEID_holding.yml"

load_config(holding_configfile)
holding_data <- reformat_data(holding_datafile, "holding")

#nuts_code <- movenet:::movenetenv$options$holdingdata_fileopts$country_code
NUTS_code <- "DK032"
crs <- movenet:::movenetenv$options$holdingdata_fileopts$coord_EPSG_code

library(hexscape)
library("tidyverse")
library("sf")

#make hexscape storage folder on hard drive
set_storage_folder("C:/Users/cboga/Documents/hexscape storage")

#Manual step - Download NUTS data and stick it in storage folder
#see instructions on https://www.costmodds.org/rsc/hexscape/

#Load the multipolygon map data for the selected NUTS area
NUTS_map <- load_map(NUTS_code)

#Manual step - Download Corine landcover data and stick it in storage folder
#see instructions on https://www.costmodds.org/rsc/hexscape/
#requires registration

#Extracting corine land cover data for the selected NUTS area, with multipolygon geometry for each type
NUTS_corine_map <- load_corine(NUTS_code)

#Extracting and combining info for all agricultural areas for the selected NUTS area
NUTS_farmland_map <-
  NUTS_corine_map %>%
  filter(CLC_Label1 == "Agricultural areas") %>% #extracts agricultural data for different types of agricultural area
  summarise(Area = sum(Area), geometry = st_union(geometry)) #extracts total area + multipolygon data for all types of agricultural area combined


### CODE BELOW JUST FOR TESTING PURPOSES ###

#replacing "fake ScotEID coordinates" with randomly sampled coordinates from NUTS area (e.g. DK032),
#to test things out with coordinates that actually correspond to a mapped area

#sample 500 random points in NUTS farmland
N<-500
fake_farms <-
  tibble(Index = 1:N, point=st_sample(NUTS_farmland_map, N)) %>%
  st_as_sf(sf_column_name="point")

#replace fake coordinates with randomly sampled points above
holding_data$coordinates<-fake_farms$point

### END OF CODE JUST FOR TESTING PURPOSES ###


#Converting the whole holding_data tibble to an sf object (rather than just the sf column)
holding_data <- st_as_sf(holding_data, sf_column_name = "coordinates")

#Creating random points
holding_data_w_random_points <-
  randomise_voronoi(NUTS_farmland_map, holding_data, randomise_size = 5L, verbose = 1L)

# Don't need the plotting code below, but it's useful to check #
#Plot to see original & randomised points, with changes marked by black lines
holding_data_w_random_points |>
  mutate(Change = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, coordinates, RandomPoint, SIMPLIFY=FALSE), crs = st_crs(coordinates))) ->
  holding_data_w_random_points
ggplot()+
  geom_sf(data = NUTS_map) +
  geom_sf(data = NUTS_farmland_map, fill = "dark green", alpha = 0.5) +
  geom_sf(aes(geometry=coordinates), holding_data_w_random_points, col="red") +
  geom_sf(aes(geometry=RandomPoint), holding_data_w_random_points, col="green") +
  geom_sf(aes(geometry=Change), holding_data_w_random_points) +
  theme_void()


#Drop original coordinates
holding_data_w_random_points %>%
  mutate(coordinates = RandomPoint, .keep = "unused")

#need to transform coordinates back to original projection?
#st_transform(holding_data, crs = crs)



