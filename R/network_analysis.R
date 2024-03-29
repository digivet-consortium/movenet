#' Extract summary stats for temporal network node properties, in parallel
#'
#' `parallel_summarise_temporal_node_properties()` calculates summary statistics
#' for a selected node property, over a list of networks. Optionally, identifiers
#' of the nodes with maximum values of the selected node property can also be
#' returned. This function uses a socket cluster with `n_threads` threads to
#' run calculations in parallel.
#'
#' @param networks A list containing movement networks (networkDynamic objects)
#' @param n_threads Numeric indicating number of threads over which to
#'   parallelise.
#' @param node_property Character indicating the network metric for which to
#'   calculate summary statistics. One of `"forward reachability"`,
#'   `"temporal degree"`, `"temporal indegree"`, or `"temporal outdegree"`.
#' @param statistics A list with summary function(s) to calculate for the
#'   selected `node_property`. For example, `list(median, max = max)`.
#' @param identify_nodes A logical indicating whether you want to identify the
#'   nodes with maximal values for `node_property` in each network. Default is
#'   `FALSE`. Requires that `max` is included as a named function within
#'   `statistics`.
#'
#' @returns A named list of lists. For each network, a list consisting of: (1) a
#'   named list `summary_statistics`, with the selected node property summary
#'   statistic(s), (2 - only if `identify_nodes` is set as `TRUE`) a named
#'   character vector `node_pid_with_max_value`, with the persistent
#'   identifier(s) of the node(s) with maximal value(s) for the selected node
#'   property.
#'
#' @importFrom networkDynamic get.vertex.pid
#' @importFrom parallel makeCluster clusterEvalQ stopCluster
#' @importFrom pbapply pblapply
#' @importFrom tsna tReach tDegree
#'
#' @export
parallel_summarise_temporal_node_properties <-
  function(networks, n_threads, node_property, statistics,
           identify_nodes = FALSE){
    cl <- makeCluster(n_threads)
    on.exit(stopCluster(cl))

    clusterExport(cl, c("node_property", "statistics", "identify_nodes"),
                  environment())
    clusterEvalQ(cl, {
      library("tsna")
      library("networkDynamic")
    })

    ### NB need to add arg checks ###

    node_summary_stats <-
      pblapply(networks,
               function(x){
                 # Determine property for all nodes
                 property <- {
                   if(node_property == "forward reachability"){
                     tReach(x, graph.step.time = 1)
                   }else if(node_property == "temporal degree"){
                     colSums(tDegree(x, cmode = "freeman"), na.rm = TRUE)
                   }else if(node_property == "temporal indegree"){
                     colSums(tDegree(x, cmode = "indegree"), na.rm = TRUE)
                   }else if(node_property == "temporal outdegree"){
                     colSums(tDegree(x, cmode = "outdegree"), na.rm = TRUE)}}
                 # Calculate requested summary statistic(s)
                 summary_stats <- lapply(statistics, function(f)(f(property)))
                 # (Optionally) identify nodes with max/min values
                 if(isTRUE(identify_nodes)){
                   id_max_value <-
                     get.vertex.pid(x, which(property == summary_stats$max))
                   # Return summary stats, and optional node identifiers
                   return(list(summary_statistics = summary_stats,
                               node_pid_with_max_value = id_max_value))
                 }else{
                   return(list(summary_statistics = summary_stats))
                 }},
               cl=cl)
    return(node_summary_stats)
  }

#' Extract time periods covered in movement dataset
#'
#' @param data Date column of movement data
#' @param period time period for which to extract dates (n days, week, n weeks,
#'    month, n months, year, n years)
#'
#' @importFrom lubridate floor_date period
#'
#' @return List of c(start date, end date) for each period covered in the data,
#'    with dates in int format
#'
#' @details For periods of days or weeks, the first date in the data is used
#'    as starting date; for periods of months or years, the first day of the
#'    month, n months, year or n years is used as starting date. (OR make
#'    starting date an argument?)
#'
#' @keywords internal
extract_periods <- function(data, period){
  if (isTRUE(grepl("(\\d+\\sdays)|((\\d+\\s)?week(\\s)?)",period))){
    start_dates <- seq(min(data), max(data), by = period)
  } else {
    start_dates <- seq(floor_date(min(data), period),
                       max(data),
                       by = period)
  }
  end_dates <- as.integer(start_dates + period(period))
  start_dates <- as.integer(start_dates)
  Map(c,start_dates,end_dates)
}

#' Extract periodic subnetworks from movement networks
#'
#' @param networks A named list of movement networks.
#' @param n_threads A numeric indicating number of threads over which to
#'   parallelise.
#' @param periods_in_data A list of start and end dates (in int format) for all periods in the data.
#'
#' @importFrom networkDynamic network.extract
#' @importFrom parallel makeCluster clusterExport stopCluster
#' @importFrom pbapply pblapply
#'
#' @details Note from network.extract: Note that only active vertices are
#'    included by default (retain.all.vertices=FALSE). As a result, the size of
#'    the extracted network may be smaller than the original. Vertex and edge
#'    ids will be translated, but may not correspond to their original values.
#'    If it is necessary to maintain the identities of vertices, see
#'    persistent.ids. (Make this an argument? Add ... to pass on arguments?)
#'
#' @keywords internal
extract_periodic_subnetworks <- function(networks, n_threads, periods_in_data){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("network.extract","periods_in_data"), envir = environment())
  periodic_networks <-
    pblapply(networks,
             function(nw){
               lapply(periods_in_data, function(p) {
                 network.extract(nw, onset = p[[1]], terminus = p[[2]],
                                 rule = "any",
                                 #retain.all.vertices = TRUE,
                                 trim.spells = TRUE)})}, cl=cl)
  names(periodic_networks) <- names(networks)
  return(periodic_networks)
}



#' Draw violin plot of distributions of monthly values of a selected network measure, for various jittered & rounded networks
#'
#' @param monthly_data tibble with measure values, for monthly networks
#' @param measure_name measure name (to refer to in y axis)
#'
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @import ggplot2
#'
#' @keywords internal
violinplot_monthly_measures <- function(monthly_data, measure_name){

  #Reformat to long tibble for plotting
  monthly_measures <-
    monthly_data |>
    pivot_longer(everything(),
                 names_to = "network",
                 values_to = measure_name)

  #Set network as a factor with specific order (to avoid default alphabetic order)
  monthly_measures$network <-
    factor(monthly_measures$network,
           levels = unique(names(monthly_data)))

  p <-
    ggplot(data = monthly_measures,
           aes(x = network, y = .data[[measure_name]])) +
    xlab("Movement network") +
    ylab(paste("Monthly", measure_name)) +
    ylim(0, NA) +
    scale_x_discrete(labels = function(x) str_wrap(as.character(x), width = 9))+
    geom_violin(trim = TRUE) +
    theme_bw()

  p <- p + geom_boxplot(width = 0.1)

  plot(p)
}

#' Plot values of a measure for a varying range of jitter or rounding
#'
#' @param data tibble with measure values for range of jitter or rounding
#' @param measure_name measure name (to refer to in y axis)
#' @param anonymisation "jitter" or "round(ing)"
#'
#' @import ggplot2
#'
#' @keywords internal
plot_measure_over_anonymisation_gradient <-
  function(data, measure_name, anonymisation){
    datacols <- colnames(data)
    anon_amount <- colnames(data)[1]
    measure <- colnames(data)[2]
    p <-
      ggplot(data = data,
             aes(x = .data[[anon_amount]], y = .data[[measure]])) +
      xlab(ifelse(anonymisation == "jitter", "Jitter (days)",
                  "Rounding unit equivalent (days)")) +
      ylab(measure_name) +
      ylim(0, NA) +
      theme_bw()

    if(anonymisation == "jitter"){
      p <- p + geom_boxplot(aes(group = .data[[anon_amount]]))
    } else {
      p <- p + geom_point()}

    plot(p)
  }
