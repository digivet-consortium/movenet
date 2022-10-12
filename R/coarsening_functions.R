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
#' @importFrom plyr round_any
#'
#' @examples
#' @export
coarsen_date <- function(data, level, aggregate_data = TRUE, ...){
  # accesses each date on an edge in network, changes to level aggregation
  coarsened_data <- data %>% mutate("{movenetenv$options$movedata_cols$date}" := floor_date(.data[[movenetenv$options$movedata_cols$date]], level)) # result is date object, rounded down to start of unit (e.g. month)

  #What to do with floor_date's week_start? default = 7 = Sunday.

  #what to do to allow user to coarsen other date fields? build in column argument like for coarsen_weight?

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
      group_by(.data[[movenetenv$options$movedata_cols$from]], .data[[movenetenv$options$movedata_cols$to]], .data[[movenetenv$options$movedata_cols$date]]) %>%
      summarise("{movenetenv$options$movedata_cols$weight}" := sum(.data[[movenetenv$options$movedata_cols$weight]]), ...) %>%
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
coarsen_weight <- function(data, column = movenetenv$options$movedata_cols$weight, jitter, round){
  # for each weight on an edge in network, apply ± jitter in range jitter, then round to nearest round
  # might want user to be able to just jitter, just round, or have both
  # sensible default values?

  # Add argument checks with checkmate:
  # The below requires column to be a single column.
  # round & jitter should be positive numbers or FALSE

  # Most suitable distribution? Currently uniform, but can change


  if (!identical(jitter, FALSE)){
    replacement_data <- data[[column]] + runif(length(data[[column]]), -jitter, +jitter)
    while (any(replacement_data <= 0)){
      replacement_data[which(replacement_data <= 0)] <- data[[column]][which(replacement_data <= 0)] + runif(sum(replacement_data <= 0), -jitter, +jitter)
    }
    data[column] <- replacement_data
  }

  if (!identical(round, FALSE)){
    data[column] <- round_any(data[[column]], accuracy = round)
    data[column][which(data[column] < round),] <- round
    #is it more efficient to filter first, and only round if data[column] > round, or to just round everything?
  }
  return(data)
}

#' @param data
#'
#' @param prefix
#'
#' @export
anonymise <- function(data, prefix, key = NULL){

  if (has_element(names(movenetenv$options), "movedata_cols") & has_element(names(data), movenetenv$options$movedata_cols$from)){
    col_to_anonymise <- c(movenetenv$options$movedata_cols$from, movenetenv$options$movedata_cols$to)
  } else if (has_element(names(movenetenv$options), "holdingdata_cols") & has_element(names(data), movenetenv$options$holdingdata_cols$id)) {
    col_to_anonymise <- movenetenv$options$holdingdata_cols$id
  } else {
    stop("The loaded config file and the type of data (movement or holding data) do not correspond. Please ensure the appropriate config file is loaded.")
  }

  unique_ids <- unique(unlist(data[col_to_anonymise]))

  if(is.null(key)){
    key <- generate_anonymisation_key(unique_ids, prefix, n_start = 1)
  }

  ids_not_in_key <- !(unique_ids %in% names(key))
  if(any(ids_not_in_key)){
    warning("The provided key has been expanded to include identifiers that were not found in the original key")
    key <- c(key,
             generate_anonymisation_key(unique_ids[which(ids_not_in_key)], prefix, n_start = length(key)+1))
  }

  data[col_to_anonymise]<-lapply(col_to_anonymise,function(x){key[data[[x]]]})

  return(list(data,key))

}

generate_anonymisation_key <- function(identifiers, prefix, n_start){
  key <- setNames(paste0(prefix, seq(n_start, length.out = length(identifiers))), identifiers)
  return(key)
}
