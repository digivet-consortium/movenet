#' Create dynamic networks from movenet-format, anonymised movement data
#'
#' @param data Movenet format movement data, with integerish holding IDs
#'
#' @return
#'
#' @importFrom dplyr select
#' @importFrom networkDynamic networkDynamic
#'
#' @export
movedata2networkDynamic <- function(data){
  nd_data <-
    data |>
    select(onset = 3, terminus = 3, tail = 1, head = 2) |>
    lapply(as.integer) |>  #networkDynamic doesnt seem to like tibbles, need to
    data.frame()           #convert to df - hence using lapply cf purrr::modify

  networkDynamic(edge.spells = nd_data, verbose = FALSE)
}
# N.B. Holding IDs need to be integer-ish (in character format is fine), so pass
# movenet-format movement data through anonymise() first.


#' Extract max reachabilities in parallel
#'
#' @param networks list of movement networks
#' @param n_threads
#'
#' @return
#'
#' @importFrom parallel makeCluster clusterEvalQ stopCluster
#' @importFrom pbapply pbsapply
#' @importFrom tsna tReach
#'
#' @export
parallel_max_reachabilities <- function(networks, n_threads){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
  })

  max_reachabilities <-
    pbsapply(networks,
              function(x){max(tReach(x, graph.step.time = 1))}, cl=cl)
  return(max_reachabilities)
}
# N.B. This step is super-slow without parallel processing.
# See https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html#building-a-socket-cluster
# Using a socket cluster, as mclapply doesn't work on Windows

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
#' @export
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
#' @param networks a named list of movement networks
#' @param n_threads
#' @param periods_in_data a list of start and end dates (in int format) for all periods in the data
#'
#' @return
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
#' @export
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
#' @return
#'
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
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
    geom_violin(trim = FALSE)

  p <- p + geom_boxplot(width = 0.1)

  plot(p)
}

#' Plot values of a measures for a varying range of jitter or rounding
#'
#' @param data tibble with measure values for range of jitter or rounding
#' @param measure_name measure name (to refer to in y axis)
#' @param anonymisation "jitter" or "round(ing)"
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
plot_measure_over_anonymisation_gradient <-
  function(data, measure_name, anonymisation){
    datacols <- colnames(data)
    anon_amount <- colnames(data)[1]
    measure <- colnames(data)[2]
    p <-
      ggplot(data = data,
             aes(x = .data[[anon_amount]], y = .data[[measure]],
                 group = .data[[anon_amount]])) +
      xlab(ifelse(anonymisation == "jitter", "Jitter (days)",
                  "Rounding unit equivalent (days)")) +
      ylab(measure_name) +
      ylim(0, NA)

    if(anonymisation == "jitter"){
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_point()}

    plot(p)
  }

