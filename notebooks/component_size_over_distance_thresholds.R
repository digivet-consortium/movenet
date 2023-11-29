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
randomise_size_range = c(5L,10L,15L,20L,50L,100L)
from_type = "point"
to_type = "centroid"
mask_landscape = FALSE

#range of distance thresholds over which to loop  <- Needs playing around with to identify useful range for DK
distance_thresholds_in_meters <- seq(0,25000,250)

#n_threads
n_threads = 4

#####################
### Reformat data ###
#####################

#load_config(holding_configfile)
#holding_data <- reformat_data(holding_datafile, "holding")



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



#############################
### Anonymise coordinates ###
#############################

if(.Platform$OS.type=="unix"){
  ## Note: memory requirements are quite high, even with a fork cluster
  cl <- makeForkCluster(6L)
  clusterSetRNGStream(cl)
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


expand_grid(RandomiseSize = randomise_size_range, Iteration = 1:10, FromType = from_type, ToType = to_type, MaskLandscape = mask_landscape) |>
  mutate(Row = 1:n()) |>
  group_split(Row) |>
  pblapply(function(x){
    hh <- randomise_voronoi(map = map,
      points = st_as_sf(holding_data, sf_column_name = "coordinates"),
      randomise_size = x$RandomiseSize,
      from_type = x$FromType, to_type = x$ToType,
      mask_landscape = x$MaskLandscape, verbose=0L) |>
      mutate(coordinates = RandomPoint)
    x |> mutate(Holdings = list(hh))
  }, cl=cl) |>
  c(list(tibble(RandomiseSize = 0L, Iteration = 0L, FromType = NA_character_, ToType = NA_character_, MaskLandscape = NA, Holdings = list(holding_data)))) ->
  anonymised_data

#################################################
### Create distance matrices for all datasets ###
#################################################

anonymised_data |>
  pblapply(function(x){
    dm <- movenet:::create_distance_matrix(x$Holdings[[1L]]) %>% units::drop_units()
    x |> select(-Holdings) |> mutate(DistanceMatrix = list(dm))
  }, cl=cl) |>
  bind_rows() ->
distance_matrices

#Have separated this out from calculate_max_comp_size_for_distance_threshold below,
#as this is a slow step that would otherwise be repeated unnecessarily



distance_matrices |>
  expand_grid(Threshold = distance_thresholds_in_meters) |>
  mutate(Row = 1:n()) |>
  slice_sample(prop=1) |>  ## Just to randomise the order so that the ETA is more reasonable
  group_split(Row) |>
  pblapply(function(x){
    x |> select(-DistanceMatrix) |> mutate(Size = calculate_max_comp_size_for_distance_threshold(x$DistanceMatrix[[1L]], x$Threshold))
  }, cl=cl) |>
  bind_rows() |>
  arrange(RandomiseSize, Iteration, Threshold) |>
  mutate(Proportion = Size/nrow(holding_data) * 100) ->
  max_comp_sizes

stopCluster(cl)

RandomiseSize_labels<-as.list(c("True data",paste("Resampling area size:",c(5,20,50), "Voronoi cells")))
names(RandomiseSize_labels)<-c(0L,5L,20L,50L)
RandomiseSize_labeller <- function(variable, value){return(RandomiseSize_labels[as.character(value)])}

max_comp_sizes|>
  filter(RandomiseSize %in% c(0L,5L,20L,50L)) |>
  mutate(Threshold = Threshold/1000) |>
  ggplot(aes(x=Threshold, y=Proportion, col=factor(RandomiseSize))) +
  geom_line(aes(group = interaction(RandomiseSize,Iteration))) +
  #geom_point() +
  facet_wrap(~RandomiseSize, labeller = RandomiseSize_labeller) +
  theme_bw(base_size = 15) +
  ylab("Holdings included in largest component (%)") +
  xlab("Distance threshold (km)") +
  guides(col = "none")
  #labs(colour = "Resampling area size") +
  #scale_colour_discrete(labels=c("True data",paste(c(5,20,50), "Voronoi cells"))) +
  #theme(legend.position = c(0.8, 0.2))
ggsave("output2.pdf")
saveRDS(max_comp_sizes, "max_comp_sizes.rds")

file.copy("output.pdf", "~/Dropbox/SimRes/output.pdf", overwrite = TRUE)
file.copy("max_comp_sizes.rds", "~/Dropbox/SimRes/max_comp_sizes.rds", overwrite = TRUE)

stop("DONE")


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
  geom_line(aes(x = Threshold, y = Proportion, col = colour_palette[])) +
  anon_lines

