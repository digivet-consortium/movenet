library(movenet)
library(network)
library(sna)
library(magrittr)
library(hexscape)

### OPTIONS ###

#holding files
holding_configfile <- "fakeScotEID_holding"
holding_datafile <- "inst/extdata/fake_Scottish_holding_data.csv"

#randomise_voronoi
map = NUTS_farmland_map
randomise_size = 5L
from_type = "point"
to_type = "centroid"
mask_landscape = FALSE

#range of distance thresholds over which to loop
distance_thresholds_in_meters <- seq(0,100000,500)

#n_threads
n_threads = 4

### "SCRIPT" (not quite) ###

load_config(holding_configfile)
holding_data <- reformat_data(holding_datafile, "holding")

#"anonymise" coordinates
anonymised_holdings <-
  randomise_voronoi(map = map,
                    points = st_as_sf(holding_data, sf_column_name = "coordinates"),
                    randomise_size = randomise_size,
                    from_type = from_type,
                    to_type = to_type,
                    mask_landscape = mask_landscape)

#function to calc max comp size
calculate_max_comp_size_for_distance_threshold <-
  function(holding_data, distance_threshold){

    #create distance-based transmission probability matrix for holding_data
    local_spread_matrix <-
      movenet:::create_local_spread_matrix(
        holding_data,
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


## loop over distance threshold ##
cl <- makeCluster(n_threads)
clusterExport(cl, c("holding_data", "anonymised_holdings",
                    "calculate_max_comp_size_for_distance_threshold"))
clusterEvalQ(cl, {
  library(movenet)
  library(magrittr)
  library(sna)
  library(network)
})

true_data <-
  pbsapply(distance_thresholds_in_meters, function(n){
    calculate_max_comp_size_for_distance_threshold(holding_data, n)}, cl = cl)


anon_data <-
  pbsapply(distance_thresholds_in_meters, function(n){
    calculate_max_comp_size_for_distance_threshold(anonymised_holdings, n)}, cl = cl)

stopCluster(cl)

n_holdings <- nrow(holding_data)
data <- data.frame(distance_thresholds_in_meters,
                   true_data = true_data*100/n_holdings,
                   anon_data = anon_data*100/n_holdings)

### Plotting ###

ggplot(data = data) +
  xlab("Distance-based transmission threshold (m)") +
  ylab("Holdings included in largest component (%)") +
  labs(colour = "Dataset") +
  geom_line(aes(x = distance_thresholds_in_meters, y = true_data, col = "True data")) +
  geom_line(aes(x = distance_thresholds_in_meters, y = anon_data, col = "Anonymised")) +
  theme_bw()
