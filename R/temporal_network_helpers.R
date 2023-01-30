#' Create dynamic networks from movenet-format, anonymised movement data
#'
#' @param data
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
#' @importFrom parallel makeCluster clusterExport parSapply stopCluster
#' @importFrom tsna tReach
#'
#' @export
parallel_max_reachabilities <- function(networks, n_threads){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, "tReach")
  max_reachabilities <-
    parSapply(cl, networks,
              function(x){max(tReach(x, graph.step.time = 1))})
  return(max_reachabilities)
}
# N.B. This step is super-slow without parallel processing.
# See https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html#building-a-socket-cluster
# Using a socket cluster, as mclapply doesn't work on Windows

#' Extract months covered in movement dataset
#'
#' @param data Date column of movement data
#'
#' @importFrom lubridate floor_date
#'
#' @return List of c(start date, end date) for each month covered in the dataset, with dates in int format
#' @export
extract_months <- function(data){
  start_dates <- seq(floor_date(min(data),"month"),
                     floor_date(max(data),"month"),
                     by = "month")
  end_dates <- as.integer(start_dates + months(1))
  start_dates <- as.integer(start_dates)
  Map(c,start_dates,end_dates)
}

#' Extract monthly movement networks
#'
#' @param networks a named list of movement networks
#' @param n_threads
#' @param months_in_data a list of start and end dates (in int format) for all months in the data
#'
#' @return
#'
#' @importFrom networkDynamic network.extract
#' @importFrom parallel makeCluster clusterExport parLapply stopCluster
#'
#' @export
extract_monthly_networks <- function(networks, n_threads, months_in_data){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("network.extract","months_in_data"), envir = environment())
  monthly_networks <-
    parLapply(cl, networks,
              function(nw){
                lapply(months_in_data, function(m) {
                  network.extract(nw, onset = m[[1]], terminus = m[[2]],
                                  rule = "any",
                                  trim.spells = TRUE)})})
  names(monthly_networks) <- names(networks)
  return(monthly_networks)
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
    scale_x_discrete(labels = function(x) str_wrap(as.character(x), width = 9))+
    geom_violin(trim = FALSE)

  p <- p + geom_boxplot(width = 0.1)

  plot(p)
}

#' Plot values of a measures for a varying range of jitter or rounding
#'
#' @param data tibble with measure values for range of jitter or rounding
#' @param measure_name measure name (to refer to in y axis)
#' @param anonymisation "jitter" and/or "round(ing)"
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
      ylab(measure_name)

    if(anonymisation == "jitter"){
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_point()}

    plot(p)
  }

