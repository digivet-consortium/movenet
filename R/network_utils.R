#' Split up a movenet movement data tibble by movement date
#'
#' `split_movement_data()` is an internal helper function that splits
#' `movement_data` up into separate tibbles for each time `period`.
#'
#' @param movement_data A movenet-format movement data tibble.
#' @inheritParams extract_periods
#'
#' @returns
#' A list of movenet-format movement data tibbles, for each `period` in the data.
#'
#' @details
#' Requires that an appropriate movement config file is loaded, to correctly
#' identify the `date` column in `movement_data`.
#'
#' For periods of days or weeks, the first date in the data is used
#' as starting date; for periods of months or years, the first day of the
#' month, n months, year or n years is used as starting date. (OR make
#' starting date an argument?)
#'
#' @importFrom dplyr filter
#'
#' @keywords internal
split_movement_data <- function(movement_data, period){

  date_column <- movement_data[[movenetenv$options$movedata_cols$date]]

  periods_in_data <- extract_periods(date_column, period)

  periodic_movement_dfs <-
    lapply(periods_in_data, function(p){
      movement_data %>%
        filter(.data[[movenetenv$options$movedata_cols$date]] >= p[1] &
                 .data[[movenetenv$options$movedata_cols$date]] < p[2])})

  return(periodic_movement_dfs)
}

#' Split up a movenet movement igraph by movement date
#'
#' `split_movement_data()` is an internal helper function that splits a movenet
#' movement data containing igraph up into separate igraphs for each time
#' `period`.
#'
#' @param g An igraph object made from movenet movement (and optional holding) data.
#' @inheritParams extract_periods
#' @param incl_nonactive_holdings A logical that indicates whether to keep
#'   holdings that don't trade within the relevant time `period`. Default is
#'   `FALSE`.
#'
#' @returns
#' A list of igraph objects for each `period` in the data.
#'
#' @details
#' Requires that an appropriate movement config file is loaded, to correctly
#' identify the `date` attribute in `g`.
#'
#' For periods of days or weeks, the first date in the data is used
#' as starting date; for periods of months or years, the first day of the
#' month, n months, year or n years is used as starting date. (OR make
#' starting date an argument?)
#'
#' @importFrom igraph delete_edges edge_attr delete_vertices E degree
#' @importFrom lubridate as_date
#'
#' @keywords internal
split_igraph <- function(g, period, incl_nonactive_holdings = FALSE){

  date_col <- movenetenv$options$movedata_cols$date

  periods_in_data <- extract_periods(as_date(igraph::as_data_frame(g)[[date_col]]), period)

  periodic_igraphs <-
    lapply(periods_in_data, function(p){
      g %>%
        delete_edges(E(g)[edge_attr(g, date_col) < p[1] |
                                 edge_attr(g, date_col) >= p[2]]) %>%
        {if(isFALSE(incl_nonactive_holdings)) delete_vertices(., degree(.)==0) else .}
        })

  return(periodic_igraphs)
}


