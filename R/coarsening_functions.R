#' Round down movement dates, and summarise data by time unit
#'
#' `coarsen_date()` takes a movement data frame, and rounds down movement dates
#' to the first day of the specified time unit. It optionally summarises
#' movement data with the same origin and destination by this time unit. By
#' default, this involves summation of weights, but alternative or additional
#' summary functions can be provided through `...`.
#'
#' @param data A movement data frame.
#' @param unit A character string specifying a time unit or a multiple of a unit
#'   for movement dates to be rounded down to. Valid base units are `day`,
#'   `week`, `month`, `bimonth`, `quarter`, `season`, `halfyear` and `year`.
#'   Arbitrary unique English abbreviations as in the [lubridate::period()]
#'   constructor are allowed. Rounding to multiples of units (except weeks) is
#'   supported.
#' @param sum_weight If this is `TRUE` (the default), weights are summed over
#'   the specified `unit`, for all rows with the same origin and destination.
#'   The name of the weight column will remain the same.
#' @param ... [<`data-masking`>][dplyr::dplyr_data_masking] Additional or
#'   alternative summary function(s), of the form name = value. Any summary
#'   functions will be applied to `data`, grouped by origin, destination, and
#'   rounded-down date. The specified name will be the column name in the
#'   resulting data frame. Be careful when using existing names: the
#'   corresponding columns will be immediately updated with the new data and
#'   this can affect subsequent operations referring to this name.
#'   The value can be:
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length n, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#'
#' @details
#' Requires that the appropriate movement config file is loaded.
#'
#' @returns
#' A movement data frame with movement dates rounded down to the first day of
#' the specified `unit`.
#'
#' If `sum_weight` is `TRUE`, the weight column (with the same name as the
#' weight column in `data`) contains weights as summed over all rows in `data`
#' with the same origin, destination, and rounded-down date.
#' If any summary functions are provided through `...`, the corresponding
#' columns (with name specified by the summary function name) contain data
#' summarised (as specified by the summary function value) over all rows in
#' `data` with the same origin, destination, and rounded-down date.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting data frame.
#'
#' #'
#' @seealso [lubridate::floor_date()], [dplyr::summarise()] which this function
#' wraps.
#'
#' @export
#'
#' @import checkmate
#' @importFrom dplyr group_by mutate rename summarise ungroup
#' @importFrom lubridate floor_date
#' @importFrom plyr round_any
#'
#' @examples
#' @export
coarsen_date <- function(data, unit, sum_weight = TRUE, ...){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded config file and the type of data (movement data)
    do not correspond. Please ensure the appropriate config file is loaded.")
  }


  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = movenetenv$options$movedata_cols$date)
  assert_date(data[[movenetenv$options$movedata_cols$date]],
              .var.name =
                paste0("data[[", movenetenv$options$movedata_cols$date,"]]"))
  assert_logical(sum_weight, len = 1)

  # How does one check the other arguments?
  #   - unit (from floor_date)
  #   - ... (var = value pairs, in summarise)


  ###########################
  ### Rounding down dates ###
  ###########################

  # round down each date to the first date of the unit (e.g. month)
  coarsened_data <-
    data |>
    mutate("{movenetenv$options$movedata_cols$date}" :=
             floor_date(.data[[movenetenv$options$movedata_cols$date]], unit))

  #What to do with floor_date's week_start? default = 7 = Sunday.
  #what to do to allow user to coarsen other date fields?
  #  build in column argument like for coarsen_weight?

  #Alternatives:
  # format_ISO8601(date_object, precision) from lubridate ->
  #      character vector matching ISO format to certain precision
  # format(date_object, format_string) from base R ->
  #      character vector matching format_string (e.g %Y-%m)
  # as.yearmon(date_object) from zoo ->
  #      yearmon object

  # Allow for aggregation over consistent periods of x number of days?
  # Lentz uses 1d, 7d, 14d, 28d (monthly), 84d (quarterly)


  ###########################
  ### Aggregating by date ###
  ###########################

  if (sum_weight == TRUE){

    aggregated_data <-
      coarsened_data |>
      group_by(.data[[movenetenv$options$movedata_cols$from]],
               .data[[movenetenv$options$movedata_cols$to]],
               .data[[movenetenv$options$movedata_cols$date]]) |>
      summarise(summed_weight =
                sum(.data[[movenetenv$options$movedata_cols$weight]]),
                ...) |>
      ungroup() |>
      rename("{movenetenv$options$movedata_cols$weight}" := summed_weight)

    return(aggregated_data)

  } else if(...length() == 0){

    return(coarsened_data)

  } else {

    aggregated_data <-
      coarsened_data |>
      group_by(.data[[movenetenv$options$movedata_cols$from]],
               .data[[movenetenv$options$movedata_cols$to]],
               .data[[movenetenv$options$movedata_cols$date]]) |>
      summarise(...) |>
      ungroup()

    return(aggregated_data)
  }

  #N.B. this drops columns for which a summary function is not given
}

################################################################################

#' @param data
#'
#' @param column
#' @param jitter
#' @param round
#'
#' @export
coarsen_weight <- function(data,
                           column = movenetenv$options$movedata_cols$weight,
                           jitter, round){
  # for each weight on an edge in network, apply ± jitter in range jitter,
  # then round to nearest multiple of round

  # any sensible default values for jitter and round?

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded config file and the type of data (movement data)
    do not correspond. Please ensure the appropriate config file is loaded.")
  }


  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = column)
  assert_data_frame(data[column], type = "numeric", ncols = 1)
  assert(
    check_numeric(jitter, lower = 0),
    check_false(jitter, na.ok = FALSE)
  )
  assert(
    check_numeric(round, lower = 0),
    check_false(round, na.ok = FALSE)
  )


  #####################
  ### Adding jitter ###
  #####################

  if (jitter != FALSE | jitter != 0){

    replacement_data <-
      data[[column]] +
      runif(length(data[[column]]), -jitter, +jitter)

    while (any(replacement_data <= 0)){ #resampling if result of jitter is <= 0

      replacement_data[which(replacement_data <= 0)] <-
        data[[column]][which(replacement_data <= 0)] +
        runif(sum(replacement_data <= 0), -jitter, +jitter)

    }

    data[column] <- replacement_data
  }

  # Most suitable distribution? Currently uniform, but can change


  ################
  ### Rounding ###
  ################

  if (round != FALSE | round != 0){

    data[column] <- round_any(data[[column]], accuracy = round)

    data[column][which(data[column] < round),] <- round #set round as minimum

    #is it more efficient to filter first & only round if data[column] > round,
    #or to just round everything?
  }

  return(data)
}

################################################################################

#' @param data
#'
#' @param prefix
#'
#' @export
anonymise <- function(data, prefix, key = NULL){

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert(
    check_names(names(data),
                must.include = c(movenetenv$options$movedata_cols$from,
                                 movenetenv$options$movedata_cols$to)),
    check_names(names(data),
                must.include = movenetenv$options$holdingdata_cols$id)
  )
  assert_string(prefix, null.ok = TRUE)
  assert_list(key, types = "character", any.missing = FALSE, names = "unique",
              null.ok = TRUE)


  #########################
  ### Config file check ###
  #########################

  if (has_element(names(movenetenv$options), "movedata_cols") &
      has_element(names(data), movenetenv$options$movedata_cols$from)){
    col_to_anonymise <- c(movenetenv$options$movedata_cols$from,
                          movenetenv$options$movedata_cols$to)
  } else if (has_element(names(movenetenv$options), "holdingdata_cols") &
             has_element(names(data), movenetenv$options$holdingdata_cols$id)){
    col_to_anonymise <- movenetenv$options$holdingdata_cols$id
  } else {
    stop(
    "The loaded config file and the type of data (movement or holding data)
    do not correspond. Please ensure the appropriate config file is loaded.")
  }


  ###########################
  ### Create / expand key ###
  ###########################

  unique_ids <- unique(unlist(data[col_to_anonymise]))

  if(is.null(key)){
    key <- generate_anonymisation_key(unique_ids, prefix, n_start = 1)
  }

  ids_not_in_key <- !(unique_ids %in% names(key))
  if(any(ids_not_in_key)){
    warning(
      "The key has been expanded to include identifiers that were not found in
       the (provided) original key")

    key <-
      c(key,
        generate_anonymisation_key(unique_ids[which(ids_not_in_key)],
                                   prefix,
                                   n_start = length(key)+1))
  }


  #############################
  ### Replace ids using key ###
  #############################

  data[col_to_anonymise]<-lapply(col_to_anonymise,function(x){key[data[[x]]]})

  return(list(data = data,
              key = key))

}

#####################################
### Helper function to create key ###
#####################################

generate_anonymisation_key <- function(ids, prefix, n_start){

  ids_in_random_order <-
    if(length(ids) == 1) ids else sample(ids, size=length(ids), replace=FALSE)

  key <-
    setNames(paste0(prefix,
                    seq(n_start, length.out = length(ids_in_random_order))),
             ids_in_random_order)

  return(key)
}
