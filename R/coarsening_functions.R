#' Title
#'
#' @param data
#' @param level
#' @param aggregate_data
#'
#' @return
#' @export
#'
#' @importFrom dplyr group_by mutate summarise ungroup
#' @importFrom lubridate floor_date
#' @importFrom magrittr %>%
#'
#' @examples
coarsen_date <- function(data, level, aggregate_data = TRUE, ...){
  # accesses each date on an edge in network, changes to level aggregation
  coarsened_data <- data %>% mutate("{movenetenv$options$movedata_cols$move_date}" := floor_date(.data[[movenetenv$options$movedata_cols$move_date]], level)) # result is date object, rounded down to start of unit (e.g. month)

  #Alternatives:
  # format_ISO8601(date_object, precision) from lubridate -> character vector matching ISO format to certain precision
  # format(date_object, format_string) from base R -> character vector matching format_string (e.g %Y-%m)
  # zoo::as.yearmon(date_object) -> yearmon object

  # Allow for aggregation over consistent periods of x number of days (as Lentz has done)?
  # Lentz uses 1d, 7d (weekly), 14d (fortnightly), 28d (monthly), 84d (quarterly) ?

  if (aggregate_data == FALSE){
    if(...length() != 0) warning(paste("As aggregate_data == FALSE, requested summarising functions for the following data columns were ignored:", paste(...names(),collapse=", ")), call. = FALSE)
    return(coarsened_data)
  } else {
    aggregated_data <-
      coarsened_data %>%
      group_by(.data[[movenetenv$options$movedata_cols$origin_ID]], .data[[movenetenv$options$movedata_cols$dest_ID]], .data[[movenetenv$options$movedata_cols$move_date]]) %>%
      summarise("{movenetenv$options$movedata_cols$nr_pigs}" := sum(.data[[movenetenv$options$movedata_cols$nr_pigs]]), ...) %>%
      ungroup
    return(aggregated_data)
  }
#Mind this only SUMS pigs, ... is for additional columns only (can't pass an alternative summary function for pigs)
#Mind this drops columns for which a summary function is not given
}

#' @param data
#'
#' @param column
#' @param jitter
#' @param round
#'
#' @export
coarsen_weight <- function(data, column = movenetenv$options$movedata_cols$nr_pigs, jitter, round){
  # for each weight on an edge in network, apply ± jitter in range jitter, then round to nearest round
  # might want user to be able to just jitter, just round, or have both
  # sensible default values?

  # The below requires column to be a single column.
  # NB jitter may result in negative numbers - what then?
  # NB jitter adds decimal numbers
  # to not need factor, need amount to be >0
  # Add argument checks with checkmate

  if (!identical(jitter, FALSE)){
    data[column] <- jitter(data[[column]], amount = jitter) # what about factor?
  }
  if (!identical(round, FALSE)){
    data[column] <- round(data[[column]], digits = -(round))
  }
  return(data)
}

#' @param data
#'
#' @param prefix
#'
#' @export
anonymise <- function(data, prefix){
  holding_IDs <- unique(c(data[[movenetenv$options$movedata_cols$origin_ID]], data[[movenetenv$options$movedata_cols$dest_ID]]))
  holding_IDs <- setNames(paste0(prefix,1:length(holding_IDs)), holding_IDs)
  data[movenetenv$options$movedata_cols$origin_ID] <- holding_IDs[data[[movenetenv$options$movedata_cols$origin_ID]]]
  data[movenetenv$options$movedata_cols$dest_ID] <- holding_IDs[data[[movenetenv$options$movedata_cols$dest_ID]]]
  return(data)
}
