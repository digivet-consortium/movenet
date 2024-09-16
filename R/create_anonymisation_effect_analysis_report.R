#' Create a report analysing the effects of modifying movement weights on network
#' properties
#'
#' `create_anonymisation_effect_analysis_report()` takes as input a livestock
#' movement dataset, and analyses how network analyses of these data are
#' affected by privacy-enhancing modifications (jittering or rounding) of
#' movement weights. The function creates an html report with visualisations of
#' the effects of different amounts of jittering and rounding on a selection of
#' epidemiologically relevant global network properties as well as on the
#' ranking of holdings according to various centrality measures. The aim is to
#' help livestock movement data managers and analysts find an appropriate
#' balance between data privacy and their utility in network analyses.
#'
#' @param movement_data A movenet-format movement tibble.
#' @param output_file Output file name and path. If no path is provided, the
#'   file will be saved in the current working directory.
#' @param modify_weights A logical indicating whether to analyse the effects of
#'   modifying weights (default `TRUE`). Currently this MUST be `TRUE` for the
#'   function to proceed.
#' @param modify_dates Does not currently do anything as analysis of modified
#'   dates is not yet implemented (automatically set to `FALSE`).
#' @param n_jitter_sim An integer indicating the number of random jitter
#'   simulations (default `3`).
#' @param time_unit A character string indicating the time unit of analyses
#'   (default `"28 days"`). The overall movement data will be split into
#'   sub-networks based on this time unit.
#' @param data_reference A character string providing an optional subtitle to
#'   the report, e.g. an identifier for the original movement dataset (default
#'   `NULL`).
#' @param verbose A logical indicating whether to print progress messages
#'   (default `FALSE`).
#'
#' @details
#'   This function applies various privacy-enhancing modifications to the
#'   provided `movement_data`, converts the resulting datasets into series of
#'   static networks for each period of the specified `time_unit`, and then
#'   provides users with a report with network analyses comparing the true data
#'   with the modified data.
#'
#'   This function requires that an appropriate movement config file is loaded,
#'   to correctly identify the weight column in `movement_data`.
#'
#'   Prior to applying privacy-enhancing modifications, `movement_data` is
#'   pre-processed: repeated movements between the same holdings on the same day
#'   are aggregated, and self-moves and moves with weight 0 are removed.
#'
#'   Privacy-enhancing modifications include the modification of movement
#'   weights (batch sizes) by jittering and rounding. For jittering, the
#'   function [`jitter_weights()`] is used, with jitter ranges of 5, 10, 50, ...
#'   up until the order of magnitude of the mean movement weight in the data.
#'   For rounding, the function [`round_weights()`] is used, with rounding units
#'   of 5, 10, 50, ... up until the order of magnitude of the largest movement
#'   weight in the data.
#'
#'   The periodic movement networks from true and modified networks are compared
#'   with regards to two epidemiologically relevant weighted global network
#'   properties: mean weighted shortest path length, and strength assortativity.
#'   Additionally, comparisons are made with regards to the relative ranking of
#'   holdings according to three weighted centrality measures: strength
#'   (geometric mean of in- and out-strength), betweenness, and PageRank.
#'
#' @returns An html report file is created with text and visualisations
#'   comparing the effects of movement weight modifications (different amounts
#'   of jittering and rounding) on a selection of global network properties and
#'   on the ranking of holdings according to several centrality measures.
#'
#' @importFrom igraph simplify mean_distance strength betweenness page_rank
#'   assortativity
#' @importFrom tidyr expand_grid
#'
#' @family Privacy-enhancing functions
#' @family Network-related functions
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a movement config file
#' load_config(system.file("configurations", "ScotEID.yml",
#'                         package = "movenet"))
#'
#' # Create a report analysing the effects of modifying movement weights
#' create_anonymisation_effect_analysis_report(example_movement_data,
#'                                             file.path(tempdir(), "mod_weight_analysis.html"),
#'                                             modify_weights = TRUE,
#'                                             modify_dates = FALSE,
#'                                             n_jitter_sim = 3,
#'                                             time_unit = "28 days",
#'                                             data_reference = "Example dataset",
#'                                             verbose = TRUE)
#'
#' # Clean-up: Reinstate previous configurations and remove temporary objects
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv")
#' file.remove(file.path(tempdir(), "mod_weight_analysis.html"))
#'
#'
create_anonymisation_effect_analysis_report <- function(movement_data,
                                                        output_file,
                                                        modify_weights = TRUE,
                                                        modify_dates = FALSE,
                                                        n_jitter_sim = 3,
                                                        # whole_months = TRUE,
                                                        time_unit = "28 days",
                                                        data_reference = NULL,
                                                        verbose = FALSE){

  # In create_temporal_network_analysis_report(), time_unit currently accepts
  # only "month", "quarter" or "year". But months are not equal in nr of days.
  # - Force analyses to be x months for both functions?
  # - Force analyses to be x nr of days for both functions?
  # - Allow range of differently specified time units for both functions?
  # - Allow this to differ between the functions?


# Argument and config checks ----------------------------------------------

  assert_data_frame(movement_data, min.cols = 4, null.ok = FALSE)
  assert_character(movement_data[[1]], any.missing = FALSE)
  assert_character(movement_data[[2]], any.missing = FALSE)
  assert_date(movement_data[[3]], any.missing = FALSE)
  assert_numeric(movement_data[[4]], any.missing = TRUE)

  assert_path_for_output(output_file, overwrite = TRUE)


  assert_flag(modify_dates, null.ok = FALSE)
  #If modify_dates is TRUE, print a warning that this feature is not yet implemented and change to FALSE
  if(!isFALSE(modify_dates)){
    warning("Analysis of modified dates is not yet implemented. Setting modify_dates to FALSE.", call. = FALSE)
    modify_dates <- FALSE
  }
  assert_true(modify_weights, na.ok = FALSE) # replace this with the line below upon implementation of dates
  # assert_flag(modify_weights, null.ok = FALSE)

  # #Need at least one of modify_weights & modify_dates to be TRUE, for the report to make sense...
  # assert(check_true(modify_weights),
  #        check_true(modify_dates))

  assert_count(n_jitter_sim, positive = TRUE)
 # assert_flag(whole_months, null.ok = FALSE)
  assert_flag(verbose, null.ok = FALSE)

  #Not sure how to test time_unit

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded configurations do not match the required type of data (movement data). Please ensure an appropriate config file is loaded.", call. = FALSE)
  }

# Define handles ----------------------------------------------------------
  weight_col_name <- names(movement_data[4])
  date_col_name <- names(movement_data[3])
  from_col_name <- names(movement_data[1])
  to_col_name <- names(movement_data[2])

  # Define output_path to save report to the current working directory if no
  # path is specified in output_file
  output_path <- if (dirname(output_file) == ".") { # No path specified,
    file.path(getwd(), basename(output_file))  # use working directory
  } else { output_file }  # Otherwise use specified path

# Process movement_data ---------------------------------------------------

  # Aggregate weights of all repeated movements between the same holdings on the
  # same day. This is done because temporal network edges can only have 1 value
  # per day. (This treats all "duplicate" entries in the data as real moves).

  # Also remove moves with batch size 0. This is done because the weight
  # coarsening functions do not allow such moves (all moves are given a value > 0).

  # Also remove moves from a holding identifier to the same holding identifier
  # (self-moves, loops). This is done because the current implementation of
  # temporal network creation does not allow loops. This assumption can be
  # removed, but it may affect analytic results (not all network properties
  # deal well with loops).
  if(verbose){cat("Processing movement data...\n")}

  movement_data %<>%
    round_dates("day") %>% # Do this both before modifications and at temporal network creation? need to do the latter anyway to sort out any arising from date jitter.
                           # Should not matter for "true" networks, but will result in slight differences when jittering either weights and/or dates.
    filter(.data[[weight_col_name]] > 0) %>%
    filter(.data[[from_col_name]] != .data[[to_col_name]])

  # # Parse character time_unit input as lubridate time period.
  # time_unit <- period(time_unit) # This screws up split_igraph() so don't do this/don't modify in place.

# Modify weights and/or dates ---------------------------------------------

  if(isTRUE(modify_weights)){
    w_round_set <- determine_weights_round_set(movement_data[[4]])
    w_jitter_ranges <- determine_weights_jitter_set(movement_data[[4]])
    w_jitter_set <- w_jitter_ranges %>% rep(n_jitter_sim)

    if(verbose){cat("Jittering movement weights...\n")}
    jittered_weights_data <-
      lapply(w_jitter_set,
             function(x){jitter_weights(movement_data, range = x)})
    names(jittered_weights_data) <- paste0("jittered (+/- ", w_jitter_set, ")")

    if(verbose){cat("Rounding movement weights...\n")}
    rounded_weights_data <-
      lapply(w_round_set,
             function(x){round_weights(movement_data, unit = x)})
    names(rounded_weights_data) <- paste0("rounded (multiples of ", w_round_set, ")")
  }

  # if(isTRUE(modify_dates)){
  #   d_round_set <- determine_dates_round_set(time_unit)
  #   d_jitter_ranges <- determine_dates_jitter_set(movement_data[[3]])
  #   d_jitter_set <- d_jitter_ranges %>% rep(n_jitter_sim)
  # }

  # if(all(c(modify_weights, modify_dates)){
  #
  # }

# Create static networks --------------------------------------------------

  # For each dataset, first create overall static network, then split up by time_unit
  # In each subnetwork, combine repeated edges: sum up their weights, drop other attributes

  if(verbose){cat("Creating static networks for true data...\n")}
  true_static_network <-
    list(movedata2igraph(movement_data, holding_data = NULL,
                         incl_nonactive_holdings = FALSE))
  names(true_static_network) <- "true"
  true_static_subnetworks <-
    lapply(true_static_network,
           function(x) {split_igraph(x, time_unit) %>%
               lapply(function(g){
                 simplify(g, edge.attr.comb = c(setNames(list("sum"),weight_col_name),"ignore"))})})
  n_periods <- length(true_static_subnetworks$true)


  if(isTRUE(modify_weights)){

    if(verbose){cat("Creating static networks for data with modified weights...\n")}
    weight_mod_subnetwork_properties <-
      expand_grid(
        tibble(modification_treatment = c("true",
                                        rep("jittered", length(w_jitter_set)),
                                        rep("rounded", length(w_round_set))),
               unit_or_range = c(0, w_jitter_set, w_round_set)),
        period = 1:n_periods)

    weight_modified_static_networks <-
      lapply(c(jittered_weights_data, rounded_weights_data),
             function(x) {movedata2igraph(x, holding_data = NULL,
                                            incl_nonactive_holdings = FALSE)})

    weight_modified_static_subnetworks <-
      lapply(weight_modified_static_networks,
             function(x) {split_igraph(x, time_unit) %>%
                   lapply(function(g){
                     simplify(g, edge.attr.comb = c(setNames(list("sum"),weight_col_name),"ignore"))})})

    weight_mod_subnetwork_properties$static_subnetworks <-
      c(true_static_subnetworks, weight_modified_static_subnetworks) %>%
      flatten()
    rm(weight_modified_static_subnetworks)
  }
  rm(true_static_subnetworks)


# Static network analysis (global measures) -------------------------------

  if(isTRUE(modify_weights)){

    if(verbose){cat("Calculating mean distances...\n")}
    weight_mod_subnetwork_properties$mean_distance <-
      sapply(weight_mod_subnetwork_properties$static_subnetworks,
             function(periodic_subnetwork){
               mean_distance(periodic_subnetwork,
                             weights = 1/edge_attr(periodic_subnetwork, weight_col_name))})

    mean_distance_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment %in% c("true", "jittered")) %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(mean_distance = mean(mean_distance)) %>%
      select(unit_or_range, mean_distance)

    trend_mean_distance_jitter <-
      determine_trend(mean_distance_jitter$unit_or_range,
                      mean_distance_jitter$mean_distance)

    mean_distance_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment %in% c("true", "rounded")) %>%
      select(unit_or_range, mean_distance)

    trend_mean_distance_round <-
      determine_trend(mean_distance_round$unit_or_range,
                      mean_distance_round$mean_distance)

    if(verbose){cat("Plotting mean distances...\n")}
    plot_mean_distance_jitter <-
      mean_distance_jitter %>%
      plot_measure_over_anonymisation_gradient2("Mean weighted shortest path length", "jitter", "boxplot")

    plot_mean_distance_round <-
      mean_distance_round %>%
      plot_measure_over_anonymisation_gradient2("Mean weighted shortest path length", "round", "boxplot")

    if(verbose){cat("Calculating strength assortativity...\n")}
    weight_mod_subnetwork_properties$strength_assortativity <-
      sapply(weight_mod_subnetwork_properties$static_subnetworks,
             function(periodic_subnetwork){
                 assortativity(periodic_subnetwork,
                               strength(periodic_subnetwork,
                                        mode = "out",
                                        weights = edge_attr(periodic_subnetwork, weight_col_name)),
                               strength(periodic_subnetwork,
                                        mode = "in",
                                        weights = edge_attr(periodic_subnetwork, weight_col_name)))})

    strength_assortativity_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment %in% c("true", "jittered")) %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(strength_assortativity = mean(strength_assortativity)) %>%
      select(unit_or_range, strength_assortativity)

    trend_strength_assortativity_jitter <-
      determine_trend(strength_assortativity_jitter$unit_or_range,
                      strength_assortativity_jitter$strength_assortativity)

    strength_assortativity_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment %in% c("true", "rounded")) %>%
      select(unit_or_range, strength_assortativity)

    trend_strength_assortativity_round <-
      determine_trend(strength_assortativity_round$unit_or_range,
                      strength_assortativity_round$strength_assortativity)

    if(verbose){cat("Plotting strength assortativity...\n")}
    plot_strength_assortativity_jitter <-
      strength_assortativity_jitter %>%
      plot_measure_over_anonymisation_gradient2("Strength assortativity", "jitter", "boxplot")

    plot_strength_assortativity_round <-
      strength_assortativity_round %>%
      plot_measure_over_anonymisation_gradient2("Strength assortativity", "round", "boxplot")

  }


# Static network analysis (local measures) --------------------------------

  # Rank diff currently set as true rank - modified rank. This can also be set
  # the other way round.
  if(isTRUE(modify_weights)){

    if(verbose){cat("Calculating and ranking holding strengths...\n")}
    weight_mod_subnetwork_properties$strength_gm <-
      lapply(weight_mod_subnetwork_properties$static_subnetworks,
             function(periodic_subnetwork){
                   strength_in <- strength(periodic_subnetwork, mode = "in",
                                           weights = edge_attr(periodic_subnetwork, weight_col_name))
                   strength_out <- strength(periodic_subnetwork, mode = "out",
                                            weights = edge_attr(periodic_subnetwork, weight_col_name))
                   strength_gm <- sqrt(strength_in * strength_out)
                   # Ranking on -(strength_gm) to get decreasing order
                   strength_rank <- rank(-strength_gm, ties.method = "average")
                   return(tibble(gm = strength_gm, ranking = strength_rank))
                })

    if(verbose){cat("Calculating ranking correlation with true data...\n")}
    weight_mod_subnetwork_properties$strength_cor <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        cor(x = row[["strength_gm"]]$ranking,
            y = true_equiv[["strength_gm"]][[1]]$ranking,
            method = "kendall")
      })

    strength_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(strength_cor = mean(strength_cor)) %>%
      select(unit_or_range, strength_cor)

    strength_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, strength_cor)

    if(verbose){cat("Calculating rank differences with true data...\n")}
    weight_mod_subnetwork_properties$strength_rank_diff <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        mod_rank <- row[["strength_gm"]]$ranking
        true_rank <- true_equiv[["strength_gm"]][[1]]$ranking
        rank_diff <- true_rank - mod_rank
        return(mean(rank_diff))
      })

    strength_rd_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(strength_rank_diff = mean(strength_rank_diff)) %>%
      select(unit_or_range, strength_rank_diff)

    strength_rd_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, strength_rank_diff)


    if(verbose){cat("Plotting correlation coefficients for strength...\n")}
    plot_strength_jitter <-
      strength_jitter %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for strength ranking", "jitter", "correlation_coefficients")

    plot_strength_round <-
      strength_round %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for strength ranking", "round", "correlation_coefficients")

    if(verbose){cat("Plotting mean rank differences for strength...\n")}
    plot_strength_rd_jitter <-
      strength_rd_jitter %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for strength", "jitter", "mean_rank_differences")

    plot_strength_rd_round <-
      strength_rd_round %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for strength", "round", "mean_rank_differences")

    if(verbose){cat("Calculating and ranking holding PageRanks...\n")}
    weight_mod_subnetwork_properties$pagerank <-
      lapply(weight_mod_subnetwork_properties$static_subnetworks,
             function(periodic_subnetwork){
                 pagerank <- page_rank(periodic_subnetwork,
                                       weights = edge_attr(periodic_subnetwork, weight_col_name))
                 # Ranking on -(pageRank) to get decreasing order
                 pagerank_rank <- rank(-pagerank$vector, ties.method = "average")
                 return(tibble(pagerank = pagerank$vector, ranking = pagerank_rank))
               })

    if(verbose){cat("Calculating ranking correlation with true data...\n")}
    weight_mod_subnetwork_properties$pagerank_cor <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        cor(x = row[["pagerank"]]$ranking,
            y = true_equiv[["pagerank"]][[1]]$ranking,
            method = "kendall")
      })

    pagerank_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(pagerank_cor = mean(pagerank_cor)) %>%
      select(unit_or_range, pagerank_cor)

    pagerank_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, pagerank_cor)


    if(verbose){cat("Calculating rank differences with true data...\n")}
    weight_mod_subnetwork_properties$pagerank_rank_diff <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        mod_rank <- row[["pagerank"]]$ranking
        true_rank <- true_equiv[["pagerank"]][[1]]$ranking
        rank_diff <- true_rank - mod_rank
        return(mean(rank_diff))
      })

    pagerank_rd_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(pagerank_rank_diff = mean(pagerank_rank_diff)) %>%
      select(unit_or_range, pagerank_rank_diff)

    pagerank_rd_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, pagerank_rank_diff)


    if(verbose){cat("Plotting correlation coefficients for PageRank...\n")}
    plot_pagerank_jitter <-
      pagerank_jitter %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for PageRank ranking", "jitter", "correlation_coefficients")

    plot_pagerank_round <-
      pagerank_round %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for PageRank ranking", "round", "correlation_coefficients")

    if(verbose){cat("Plotting mean rank differences for PageRank...\n")}
    plot_pagerank_rd_jitter <-
      pagerank_rd_jitter %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for PageRank", "jitter", "mean_rank_differences")

    plot_pagerank_rd_round <-
      pagerank_rd_round %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for PageRank", "round", "mean_rank_differences")

    if(verbose){cat("Calculating and ranking holding betweenness...\n")}
    weight_mod_subnetwork_properties$betweenness <-
      lapply(weight_mod_subnetwork_properties$static_subnetworks,
             function(periodic_subnetwork){
                 betweenness <- betweenness(periodic_subnetwork,
                                            weights = 1/edge_attr(periodic_subnetwork, weight_col_name))
                 # Ranking on -(betweenness) to get decreasing order
                 betweenness_rank <- rank(-betweenness, ties.method = "average")
                 return(tibble(betweenness = betweenness, ranking = betweenness_rank))
               })

    if(verbose){cat("Calculating ranking correlation with true data...\n")}
    weight_mod_subnetwork_properties$betweenness_cor <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        cor(x = row[["betweenness"]]$ranking,
            y = true_equiv[["betweenness"]][[1]]$ranking,
            method = "kendall")
      })

    betweenness_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(betweenness_cor = mean(betweenness_cor)) %>%
      select(unit_or_range, betweenness_cor)

    betweenness_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, betweenness_cor)

    if(verbose){cat("Calculating rank differences with true data...\n")}
    weight_mod_subnetwork_properties$betweenness_rank_diff <-
      weight_mod_subnetwork_properties %>%
      apply(MARGIN = 1, FUN = function(row){
        t <- row$period
        true_equiv <- weight_mod_subnetwork_properties[which(
          weight_mod_subnetwork_properties$modification_treatment == "true" &  weight_mod_subnetwork_properties$period == t),]
        mod_rank <- row[["betweenness"]]$ranking
        true_rank <- true_equiv[["betweenness"]][[1]]$ranking
        rank_diff <- true_rank - mod_rank
        return(mean(rank_diff))
      })

    betweenness_rd_jitter <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "jittered") %>%
      # for each period, calculate the average across 3 jitter simulations
      group_by(unit_or_range, period) %>%
      summarise(betweenness_rank_diff = mean(betweenness_rank_diff)) %>%
      select(unit_or_range, betweenness_rank_diff)

    betweenness_rd_round <-
      weight_mod_subnetwork_properties %>%
      filter(modification_treatment == "rounded") %>%
      select(unit_or_range, betweenness_rank_diff)

    if(verbose){cat("Plotting correlation coefficients for betweenness...\n")}
    plot_betweenness_jitter <-
      betweenness_jitter %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for betweenness ranking", "jitter", "correlation_coefficients")

    plot_betweenness_round <-
      betweenness_round %>%
      plot_measure_over_anonymisation_gradient2("Correlation coefficient for betweenness ranking", "round", "correlation_coefficients")

    if(verbose){cat("Plotting mean rank differences for betweenness...\n")}
    plot_betweenness_rd_jitter <-
      betweenness_rd_jitter %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for betweenness", "jitter", "mean_rank_differences")

    plot_betweenness_rd_round <-
      betweenness_rd_round %>%
      plot_measure_over_anonymisation_gradient2("Mean rank difference for betweenness", "round", "mean_rank_differences")

    }

# Create report -----------------------------------------------------------

  report_source <- system.file("reports/anonymisation_effect_analysis.Rmd",
                               package = "movenet")

  #Knit report
  if(verbose){cat("Creating report...\n")}
  render(input = report_source,
         output_format = NULL,
         output_file = output_path,
         params = list(modify_weights = modify_weights,
                       modify_dates = modify_dates))


}

#' Get series of 5, 10, 50, 100, ... until order of magnitude of x
#' @importFrom magrittr raise_to_power multiply_by_matrix
#' @keywords internal
series_5_10_50_100_up_to_oom <- function(x){
  x %>%
    log10 %>%
    floor %>%
    seq(1, .) %>%
    magrittr::raise_to_power(10, .) %>%
    t %>%
    magrittr::multiply_by_matrix(c(0.5,1), .) %>%
    as.vector
}

determine_weights_round_set <- function(weights){
  series_5_10_50_100_up_to_oom(max(weights)) #or mean, median, 3rd quartile?
}
determine_weights_jitter_set <- function(weights){
  series_5_10_50_100_up_to_oom(mean(weights)) #or median, 3rd quartile?
}
# #' @importFrom dplyr case_when
# determine_dates_round_set <- function(time_unit){
#   #week, month, 2 months, 3 months - whichever is greatest within time_unit?
#   #NB this may be difficult to identify considering that 1 month can be 28,29,30,31 days.
# }

#' Plot values of a measure for a varying range of jitter or rounding
#'
#' @param data tibble with measure values for range of jitter or rounding
#' @param measure_name measure name (to refer to in y axis)
#' @param anonymisation "jitter" or "round(ing)"
#' @param plot_type "boxplot" or "scatterplot"
#' @param ... other parameters to pass on to wilcox.test
#'
#' @details Add alternative = "less" or alternative = "greater" for significance
#' stars from a one-sided test.
#'
#' @import ggplot2
#' @importFrom dplyr case_when
#'
#' @keywords internal
plot_measure_over_anonymisation_gradient2 <-
  function(data, measure_name, anonymisation, plot_type, ...){
    datacols <- colnames(data)
    anon_amount <- colnames(data)[1]
    measure <- colnames(data)[2]

    p <-
      ggplot(data = data,
             aes(x = .data[[anon_amount]], y = .data[[measure]])) +
      xlab(ifelse(anonymisation == "jitter", "Jitter range",
                  "Rounding unit")) +
      ylab(measure_name) +
      #ylim(0, NA) +
      theme_bw()

    ### To compare global measures between true data and anonymised data ###

    if(plot_type == "boxplot"){

      # Calculate p-values with wilcox.test, comparing groups with data[[anon_amount]] == 0 and all other data[[anon_amount]]
      # This is done to show if the anonymisation has a significant effect on the measure
      # (i.e. if the measure is significantly different between the true data and the anonymised data)
      non_zero_ranges_or_units <- unique(data[[anon_amount]][data[[anon_amount]] != 0])

      p_values <-
        sapply(non_zero_ranges_or_units,
               function(x) {
                 wilcox.test(data[data[[anon_amount]] == x,][[measure]], # compare the modified data
                             data[data[[anon_amount]] == 0,][[measure]], # to the true data
                             paired = TRUE, # paired test because comparing values for the same period across different treatments
                             ...)$p.value  # ... allows for passing on of alternative hypotheses (e.g. for one-sided tests)
               })

      # convert to stars and stick into tibble for geom_text
      signif_stars <-
        tibble(x = non_zero_ranges_or_units,
               y = max(data[[measure]])+((max(data[[measure]])-min(data[[measure]]))*0.1),
               signif = case_when(p_values <= 0.001 ~ "***",
                                  p_values <= 0.01 ~ "**",
                                  p_values <= 0.05 ~ "*",
                                  p_values > 0.05 ~ "ns"))

      # Create boxplot and add significance stars
      p <- p +
        geom_boxplot(aes(group = .data[[anon_amount]])) +
        geom_text(aes(x = x, y = y, label = signif), data = signif_stars) +

      # Add ("True") to the first tick label to make figure more informative
        scale_x_continuous(
          labels = function(x){
            if (0 %in% x){
              x[which(x==0)] <- "0\n(True data)"
              return(x)
            } else { return(x)}
          })


    ### To compare rankings of holdings between true data and anonymised data ###

    } else if(plot_type == "correlation_coefficients"){

      p <- p +
        geom_boxplot(aes(group = .data[[anon_amount]])) +
        geom_hline(yintercept = 1, linetype = "dotted")

        # Do any statistical markings make sense?

    } else if(plot_type == "mean_rank_differences"){

      p <- p +
        geom_boxplot(aes(group = .data[[anon_amount]])) +
        geom_hline(yintercept = 0, linetype = "dotted") #to show where the true data would be

      # Do any statistical markings make sense?

    ### To draw simple dot plots for a single measure per anonymisation treatment ###

    } else {

      p <- p + geom_point()}

    plot(p)
  }

determine_trend <- function(x, y, alpha = 0.05){
  lm.out <- lm(y ~ x)
  slope <- summary(lm.out)$coefficients['x', 'Estimate'] # estimated slope
  slope.p <- summary(lm.out)$coefficients['x', 'Pr(>|t|)'] # p-value for H0: slope = 0
  if(slope.p > alpha || is.nan(slope.p)){trend <- 'no significant trend'}
   else if(slope > 0){trend <- 'a positive trend'}
   else if(slope < 0){trend <- 'a negative trend'}
  return(trend)
}
