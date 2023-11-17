##############
### Set-up ###
##############

library(movenet)
library(network)
library(sna)
library(magrittr)
library(hexscape)
library(pbapply)
library(units)
library(RColorBrewer)
library(parallel)
library(sf)
library(tidyverse)

if(.Platform$OS.type=="unix"){

  (goldfinger::gy_load(dk_pigfile))
  st_crs(farms) <- 25832
  holding_data <- farms |> mutate(Animals = BSTRK_1501+BSTRK_1502+BSTRK_1504) |> filter(str_detect(Type, "K\\u00f8d"), !is.na(Animals), Animals>=20L) |> select(CHR_ID, coordinates=geometry) |> st_transform(st_crs(3035))

  load_corine("DK") |>
    filter(CLC_Label1 == "Agricultural areas") |>
    summarise(Area = sum(Area), geometry = st_union(geometry)) ->
    map

}else{
  #data and config files  <- PLEASE CHANGE TO SUITABLE DK FILES
  holding_configfile <- "fakeScotEID_holding"
  holding_datafile <- "inst/extdata/fake_Scottish_holding_data.csv"
  load_config(holding_configfile)
  holding_data <- reformat_data(holding_datafile, "holding")

  map = NUTS_farmland_map   # <- PLEASE CHANGE TO SUITABLE DK MAP
}

#randomise_voronoi
randomise_size_range = c(5L,10L,15L,20L)
from_type = "point"
to_type = "centroid"
mask_landscape = FALSE

#range of distance thresholds over which to loop  <- Needs playing around with to identify useful range for DK
distance_thresholds_in_meters <- seq(0,100000,1000)

#n_threads
n_threads = 4

#####################
### Reformat data ###
#####################

#load_config(holding_configfile)
#holding_data <- reformat_data(holding_datafile, "holding")

#############################
### Anonymise coordinates ###
#############################

anonymised_data <-
  pblapply(randomise_size_range,
         function(x){
             randomise_voronoi(map = map,
                               points = st_as_sf(holding_data, sf_column_name = "coordinates"),
                               randomise_size = x,
                               from_type = from_type, to_type = to_type,
                               mask_landscape = mask_landscape, verbose=0L)
         })
names(anonymised_data) <- randomise_size_range

#################################################
### Create distance matrices for all datasets ###
#################################################

distance_matrices <-
  pblapply(c("true" = list(holding_data), anonymised_data),
         function(x) { movenet:::create_distance_matrix(x) %>% units::drop_units()})

#Have separated this out from calculate_max_comp_size_for_distance_threshold below,
#as this is a slow step that would otherwise be repeated unnecessarily


####################################################################
### Calculate maximum component size for each distance threshold ###
####################################################################

#function to calc max comp size
calculate_max_comp_size_for_distance_threshold <-
  function(distance_matrix, distance_threshold){

    #create distance-based transmission probability matrix for holding_data
    local_spread_matrix <-
      movenet:::replace_distances_with_probabilities(
        distance_matrix,
        local_spread_probability_tiers =
          data.frame(lower_boundary = c(0, distance_threshold),
                     upper_boundary = c(distance_threshold, Inf),
                     probability = c(1,0)),
        accept_missing_coordinates = FALSE)

    #create network and calculate max component size
    max_component_size <-
      network(local_spread_matrix, directed = FALSE) %>%
      component.dist(connected = "weak") %>%
      extract2("csize") %>%
      max

    return(max_component_size)
  }


if(.Platform$OS.type=="unix"){
  cl <- makeForkCluster(10L)
}else{
  #loop over distance thresholds
  cl <- makeCluster(n_threads)
  clusterExport(cl, c("distance_matrices",
    "calculate_max_comp_size_for_distance_threshold",
    "distance_thresholds_in_meters"))
  clusterEvalQ(cl, {
    library(movenet)
    library(magrittr)
    library(sna)
    library(network)
    library(pbapply)
  })
}

max_comp_sizes <-
  lapply(distance_matrices,
         function(m){pbsapply(distance_thresholds_in_meters,
                              function(n){calculate_max_comp_size_for_distance_threshold(m, n)},
                              cl = cl)})
stopCluster(cl)


###############################
### Processing for plotting ###
###############################

n_holdings <- nrow(holding_data)

max_comp_sizes <-
  max_comp_sizes %>%
  as_tibble() %>%
  mutate(across(everything(), ~ .x*100/n_holdings))

max_comp_sizes["threshold"] <- distance_thresholds_in_meters


################
### Plotting ###
################

colour_palette <- brewer.pal(length(randomise_size_range)+1, "Set3")
names(colour_palette) <- c("True data", paste("Randomise_size =",randomise_size_range))

anon_lines <-
  lapply(seq(2,length(max_comp_sizes)-1),
         function(idx) {
           geom_line(data = max_comp_sizes,
                     aes(x = threshold, y = get(names(max_comp_sizes)[idx]),
                         colour = eval(names(colour_palette)[idx])))})

ggplot(data = max_comp_sizes) +
  xlab("Distance-based transmission threshold (m)") +
  ylab("Holdings included in largest component (%)") +
  labs(colour = "Legend") +
  scale_colour_manual(breaks = names(colour_palette), values = colour_palette) +
  theme_bw() +
  geom_line(aes(x = threshold, y = true, col = "True data")) +
  anon_lines

