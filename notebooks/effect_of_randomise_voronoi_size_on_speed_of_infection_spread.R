##############
### Set-up ###
##############

library(movenet)
library(hexscape)
library(sf)
library(pbapply)
library(parallel)
library(RColorBrewer)

if(!requireNamespace("siminf4movenet")){
  #update when have released version
  #stop("The siminf4movenet package is not installed - run:\ninstall.packages('siminf4movenet', repos=....)")
  stop("The siminf4movenet package is not installed - run:\ninstall_github('digivet-consortium/siminf4movenet')")
}
if(!requireNamespace("SimInf")){
  stop("The SimInf package is not installed - run:\ninstall.packages('SimInf')")
}

if(.Platform$OS.type=="unix"){

  (goldfinger::gy_load(dk_pigfile))
  st_crs(farms) <- 25832
  holding_data <- farms |>
    mutate(Animals = BSTRK_1501+BSTRK_1502+BSTRK_1504) |>
    filter(str_detect(Type, "K\\u00f8d"), !is.na(Animals), Animals>=20L) |>
    select(cph=CHR_ID, herd_size=Animals, holding_type=Type, coordinates=geometry) |>
    st_transform(st_crs(3035)) |>
    mutate(cph = as.character(cph)) |>
    group_by(cph) |>
    arrange(desc(herd_size)) |>
    slice(1L)

  movements |>
    filter(CHR_FROM %in% holding_data$cph, CHR_TO %in% holding_data$cph) |>
    select(departure_cph = CHR_FROM, dest_cph = CHR_TO, departure_date = DATO_FLYTNING, qty_pigs = ANTAL_FLYT_DYR) |>
    mutate(across(c(departure_cph, dest_cph), as.character)) ->
    movement_data

  load_corine("DK") |>
    filter(CLC_Label1 == "Agricultural areas") |>
    summarise(Area = sum(Area), geometry = st_union(geometry)) ->
    map

}else{
  #data and config files  <- PLEASE CHANGE TO SUITABLE DK FILES
  movement_configfile <- "ScotEID"
  holding_configfile <- "fakeScotEID_holding"
  movement_datafile <- "inst/extdata/fake_Scottish_movement_data.csv"
  holding_datafile <- "inst/extdata/fake_Scottish_holding_data.csv"

  #randomise_voronoi options
  map = NUTS_farmland_map #  <- PLEASE INSERT MAP

}

randomise_size_range = c(5L,10L,15L,20L)
from_type = "point"
to_type = "centroid"
mask_landscape = FALSE

#model options
##############
load(system.file("data/local_spread_probabilities_ASF_Halasa_et_al_2016.Rdata",
                 package = "movenet"))
local_spread_transmission_probabilities = local_spread_probabilities_ASF_Halasa_et_al_2016
# look-up table with distance-based transmission probability tiers for ASF, from
# the DTU-DADS-ASF model

incl_nonactive_holdings = TRUE
# whether to include non-active holdings in the model (transmission via local
# spread or other transmission routes only)

weight_unit_transmission_probability = 0.8
# probability of transmission via movement, per unit moved weight (e.g. per
# pig) from an infected holding. 1 assumes all movements from infected holdings
# are 100% infectious, regardless of weight.

whole_months = TRUE
# Do the movement data cover whole months?
# If movement data starts with movements on 3 Jan, TRUE assumes 1 and 2 Jan were
# days with 0 movements, and does include these days when calculating average
# movement weights. FALSE considers the data capture to start on 3 Jan, leaving
# only considering 29 days of Jan when calculating average movement weights.

## We will select the infected_holdings based on proximity to a forest in southern Jutland:
incursion_point <- st_sfc(st_point(c(9.305717069633731, 54.83314782219914)), crs=st_crs("WGS84")) |> st_transform(st_crs(map))
ggplot() + geom_sf(data=map) + geom_sf(data=incursion_point, col="red")

#infected_holdings = c("68/575/1991", "86/580/7898") # <- PLEASE CHANGE TO DANISH HOLDING ID(s)
#identifiers of farms infected at t = 0

if(FALSE){
  ## Code to find the best (worst?) place to start an epidemic:
  movement_data |> count(departure_cph) |> arrange(desc(n)) |> slice(1:40) |>
    mutate(MeanOutbreak =
      pbsapply(departure_cph, function(x){
        data2modeloutput(movement_data, holding_data, incl_nonactive_holdings,
                       weight_unit_transmission_probability, whole_months,
                       x, tspan, 10L) |> pull(Mean) |> max()
      }, cl=10L)
    ) ->
    find_start
  # Row index 8 has the highest outbreak potential
}

infected_holdings <- movement_data |> count(departure_cph) |> arrange(desc(n)) |> slice(8L) |> pull(departure_cph)


#Time over which to run model
days = 365 #number of days to run the simulation. Default is 365 days.
stride = 1 #the increment (integer) between days that are recorded in the model
#U and V matrices. Default is 1 day, i.e., every day is recorded.
tspan <- seq(from = 1L, to = as.integer(days), by = as.integer(stride))

n_simulations = 100 #number of simulations for model (same initially infected nodes)

#####################
### Reformat data ###
#####################
if(FALSE){
  load_config(movement_configfile)
  movement_data <- movement_datafile |> reformat_data("movement")
  load_config(holding_configfile)
  holding_data <- holding_datafile |> reformat_data("holding")
}

###########################
### Prepare & run model ###
###########################

#Helper function: Add summary statistics over all simulations
add_summary_stats <- function(cumul_infected, n_nodes, n_simulations){
  cumul_infected %>%
    as_tibble %>% {
      Apply <- function(fun, ...) select(., 1:eval(n_simulations)) %>% apply(1,fun, ...)
      mutate(., Min = Apply(min)*100/n_nodes,
             Q1 = Apply(quantile, prob = 0.25)*100/n_nodes,
             Mean = Apply(mean)*100/n_nodes,
             Median = Apply(median)*100/n_nodes,
             Q3 = Apply(quantile, prob = 0.75)*100/n_nodes,
             Max = Apply(max)*100/n_nodes)
    }
}

#Main wrapping function
data2modeloutput <- function(movement_data, holding_data,
                             incl_nonactive_holdings,
                             weight_unit_transmission_probability,
                             whole_months, infected_holdings, tspan,
                             n_simulations){

  contacts <-
    holding_data %>%
    data2contactmatrix(movement_data, .,
                       incl_nonactive_holdings = incl_nonactive_holdings,
                       weight_unit_transmission_probability =
                         weight_unit_transmission_probability,
                       whole_months = whole_months,
                       local_spread_transmission_probabilities =
                         local_spread_transmission_probabilities,
                       additional_transmission_prob_matrices = NULL)

  if(requireNamespace("siminf4movenet") & requireNamespace("SimInf")){

    model <- SEIRcm_ASF(infected =
                          create_specified_infected_vector(infected_holdings, contacts$key),
                        tspan = tspan,
                        contact_matrix = contacts$transmission_matrix)

    trajectories <- replicate(n = n_simulations,
                              SimInf::trajectory(siminf4movenet::run(model))) #trajectory["I",1] shows "I" status for c(node, timestep) for simulation 1
  }

  #Get cumulative number of infected farms over time for each simulation
  trajectories %>%
    get_cumulative_infected(n_simulations, ncol(contacts$transmission_matrix)) %>%
    add_summary_stats(ncol(contacts$transmission_matrix), n_simulations)
}

#Run model on true data
true_data <- data2modeloutput(movement_data, holding_data, incl_nonactive_holdings,
                              weight_unit_transmission_probability, whole_months,
                              infected_holdings, tspan, n_simulations)


############################################################
### Anonymise coordinates & run model on anonymised data ###
############################################################

if(.Platform$OS.type=="unix"){
  ## Note: memory requirements are quite high, even with a fork cluster
  cl <- makeForkCluster(6L)
  clusterSetRNGStream(cl)
}else{
  cl <- NULL
}

anonymised_data <-
  pblapply(randomise_size_range,
         function(x){
           anon_holding_data <-
             randomise_voronoi(map = map,
                               points = st_as_sf(holding_data, sf_column_name = "coordinates"),
                               randomise_size = x,
                               from_type = from_type, to_type = to_type,
                               mask_landscape = mask_landscape) |>
             ## Ensure that the original points are not used:
             mutate(coordinates = RandomPoint)
           output <- data2modeloutput(movement_data, anon_holding_data,
                                      incl_nonactive_holdings,
                                      weight_unit_transmission_probability,
                                      whole_months, infected_holdings, tspan,
                                      n_simulations)
           return(output)
           }, cl=cl)
names(anonymised_data) <- randomise_size_range

if(!is.null(cl)) stopCluster(cl)

save(anonymised_data, randomise_size_range, true_data, file="siminf_res.rda")

################
### Plotting ###
################

colour_palette <- brewer.pal(length(randomise_size_range)+1, "Set3")
names(colour_palette) <- c("True data", paste("Randomise_size =",randomise_size_range))

anon_ribbons <-
  lapply(seq_along(anonymised_data),
       function(idx){
         geom_ribbon(data = anonymised_data[[idx]],
                     aes(x = as.numeric(row.names(anonymised_data[[idx]])), ymin = Min, ymax = Max,
                         fill = eval(names(colour_palette)[idx+1])),
                     alpha = 0.2)
       })

anon_lines <-
  lapply(seq_along(anonymised_data),
         function(idx){
           geom_line(data = anonymised_data[[idx]],
                     aes(x = as.numeric(row.names(anonymised_data[[idx]])), y = Mean,
                         colour = eval(names(colour_palette)[idx+1])))
         })

p <-
  ggplot(true_data) +
  xlab("Time (days)") +
  ylab("Holdings infected (%)") +
  labs(colour = "Legend") +
  scale_colour_manual(breaks = names(colour_palette),
                      values = colour_palette) +
  scale_fill_manual(values = colour_palette, guide = "none") +
  theme_bw() +
  geom_ribbon(aes(x = as.numeric(row.names(true_data)), ymin = Min, ymax = Max,
                  fill = "True data"),
              alpha = 0.2) +
  geom_line(aes(x=as.numeric(row.names(true_data)), y = Mean, colour = "True data")) +
  anon_ribbons +
  anon_lines

plot(p)
ggsave("siminf_plot.pdf")

file.copy("siminf_plot.pdf", "~/Dropbox/SimRes/siminf_plot.pdf", overwrite = TRUE)
file.copy("siminf_res.rda", "~/Dropbox/SimRes/siminf_res.rda", overwrite = TRUE)
