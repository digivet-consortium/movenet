#' Jitter (add noise to) movement dates
#'
#' `jitter_dates()` adds random noise (jitter) to movement dates in a
#' movenet-format movement tibble.
#'
#' @param data A movenet-format movement tibble.
#' @param range A positive integer, indicating the maximum amount of jitter (in
#'   days) to apply to the movement dates (see Details).
#'
#' @details
#' Requires that an appropriate movement config file is loaded, to correctly
#' identify the `date` column in `data`.
#'
#' Movement dates are modified by addition of a number of days between `-range`
#' and `range`, following a discrete uniform distribution. If this were to
#' result in a date being moved beyond the original date range, the amount of
#' jitter for this date is resampled, until the resulting date is within the
#' original date range.
#'
#' @returns
#' A movement tibble like `data`, but with jittered movement dates.
#'
#' @import checkmate
#' @importFrom purrr has_element
#'
#' @export
jitter_dates <- function(data, range){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded configurations do not match the required type of data (movement data). Please ensure an appropriate config file is loaded.")
  }

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = movenetenv$options$movedata_cols$date,
               .var.name = "data")
  dates <- data[[movenetenv$options$movedata_cols$date]]
  assert_date(dates,
              .var.name =
                paste0("data[[", movenetenv$options$movedata_cols$date,"]]"))
  assert_count(range, positive = TRUE)

  #####################
  ### Adding jitter ###
  #####################

  range <- as.integer(range)

  replacement_data <-
    dates +
    sample(c(-range:range),
           length(dates),
           replace = TRUE)

  # This adds uniform ints between -range and +range , including 0 !

  while (any(replacement_data < min(dates) | replacement_data > max(dates))){

    #resampling for dates beyond boundaries
    replacement_data[which(replacement_data < min(dates) |
                             replacement_data > max(dates))] <-
      dates[which(replacement_data < min(dates) |
                    replacement_data > max(dates))] +
      sample(c(-range:range),
             sum(any(replacement_data < min(dates) |
                       replacement_data > max(dates))),
             replace = TRUE)

  }

  data[movenetenv$options$movedata_cols$date] <- replacement_data

  return(data)
}

###############################################################################
#' Round down movement dates, and/or summarise data by time unit
#'
#' `round_dates()` rounds movement dates in a movenet-format movement
#' tibble, down to the first day of the specified time unit. It optionally
#' summarises movement data with the same origin and destination by this time
#' unit. By default, this involves summation of weights, but alternative or
#' additional summary functions can be provided through `...`.
#'
#' @param data A movenet-format movement tibble.
#' @param unit A character string specifying a time unit or a multiple of a
#'   unit for movement dates to be rounded down to. Valid base units are `day`,
#'   `week`, `month`, `bimonth`, `quarter`, `season`, `halfyear` and `year`.
#'   Arbitrary unique English abbreviations as in the [lubridate::period()]
#'   constructor are allowed. Rounding to multiples of units (except weeks) is
#'   supported.
#' @param week_start Week start day, only relevant for rounding down dates and/
#'   or aggregating data by week. Default is the value of the
#'   `lubridate.week.start` option, or 7 (Sunday) if this option is not set.
#'   Full or abbreviated names of the days of the week can be in English or as
#'   provided by the current locale.
#' @param sum_weight If this is `TRUE` (the default), weights are summed over
#'   `unit`, for all rows with the same origin and destination. The name of
#'   the weight column will remain the same.
#' @param ... [`data-masking`][dplyr::dplyr_data_masking] Additional or
#'   alternative summary function(s), of the form name = value, to pass on to
#'   [dplyr::summarise()]. Any summary functions will be applied to `data`,
#'   grouped by origin, destination, and rounded-down date. The specified name
#'   will be the column name in the resulting tibble.
#'   The value can be:
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length n, e.g. `quantile()`.
#'   * A tibble, to add multiple columns from a single expression.
#'
#' @details
#' Requires that an appropriate movement config file is loaded, to correctly
#' identify origin (`from`), destination (`to`), `date` and `weight` columns in
#' `data`.
#'
#' Movement dates are rounded to the first day of the rounding unit specified in
#'  `unit`.
#'
#' If `sum_weight` is `TRUE`, weights are summed over all rows in `data` with
#' the same origin, destination, and rounded-down date. The summed weight column
#'  is temporarily renamed, so that any additional weight-dependent summary
#' functions passed through `...` use the original weights rather than the sums.
#'
#' If any summary functions are provided through `...`, the specified data are
#' summarised accordingly, over all rows in `data` with the same origin,
#' destination, and rounded-down date. Be careful when using' existing names:
#' the corresponding columns will be immediately updated with the new data and
#' this can affect subsequent operations referring to this name.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting tibble.
#'
#' @returns
#' A movement tibble like `data`, but with rounded-down movement dates.
#'
#' If `sum_weight` is `TRUE` or any summary functions are provided through
#' `...`, the returned tibble contains data summarised by origin,
#' destination, and `unit`, and may thus have a decreased length compared to
#' `data`.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting tibble.
#'
#' @seealso [lubridate::floor_date()], [dplyr::summarise()] which this function
#' wraps.
#'
#' @import checkmate
#' @importFrom dplyr group_by mutate rename summarise ungroup
#' @importFrom lubridate floor_date
#' @importFrom purrr has_element
#' @importFrom rlang :=
#'
#' @export
round_dates <- function(data, unit,
                        week_start = getOption("lubridate.week.start", 7),
                        sum_weight = TRUE, ...){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded configurations do not match the required type of data (movement data). Please ensure an appropriate config file is loaded.")
  }

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = movenetenv$options$movedata_cols$date,
               .var.name = "data")
  dates <- data[[movenetenv$options$movedata_cols$date]]
  assert_date(dates,
              .var.name =
                paste0("data[[", movenetenv$options$movedata_cols$date,"]]"))
  assert_string(unit) #this does not check accepted/meaningful values
  assert_flag(sum_weight)

  # How does one check the other arguments?
  #   - unit (from floor_date; currently checking for character type, a subset of original requirements)
  #   - week_start (from floor_date)
  #   - ... (from summarise; format should be var = value pairs)

  ###########################
  ### Rounding down dates ###
  ###########################

  # round down each date to the first date of the unit (e.g. month)
  rounded_data <-
    data |>
    mutate("{movenetenv$options$movedata_cols$date}" :=
             floor_date(.data[[movenetenv$options$movedata_cols$date]],
                        unit = unit,
                        week_start = week_start))

  #what to do to allow user to coarsen other date fields?
  #  build in column argument like for coarsen_weight?

  # Allow for aggregation over consistent periods of x number of days?
  # Lentz uses 1d, 7d, 14d, 28d (monthly), 84d (quarterly)

  ###########################
  ### Aggregating by date ###
  ###########################

  if (isTRUE(sum_weight)){

    aggregated_data <-
      rounded_data |>
      group_by(.data[[movenetenv$options$movedata_cols$from]],
               .data[[movenetenv$options$movedata_cols$to]],
               .data[[movenetenv$options$movedata_cols$date]]) |>
      summarise(summed_weight =
                  sum(.data[[movenetenv$options$movedata_cols$weight]]),
                ...) |>
      ungroup() |>
      rename("{movenetenv$options$movedata_cols$weight}" := summed_weight)
    #using "summed_weight" and then renaming to the data-specific weight
    #variable, to avoid problems with additional weight summarising
    #functions. If the original name is kept, any additional functions are
    #performed on the summed weights rather than individual weights

    return(aggregated_data) #return data with summed weights and potential other summary functions in ...

  } else if(...length() == 0){

    return(rounded_data) #return un-summarised rounded data

  } else {

    aggregated_data <-
      rounded_data |>
      group_by(.data[[movenetenv$options$movedata_cols$from]],
               .data[[movenetenv$options$movedata_cols$to]],
               .data[[movenetenv$options$movedata_cols$date]]) |>
      summarise(...) |>
      ungroup()

    return(aggregated_data) #return data summarised only by functions in ...
  }
}

###############################################################################
#' Jitter (add noise to) numeric movement data
#'
#' `jitter_weights()` adds random noise (jitter) to a selected numeric column
#' in a movenet-format movement tibble. By default, the weight column is
#' selected.
#'
#' @param data A movenet-format movement tibble.
#' @param column Name of a single numeric column to jitter. By default this is
#'   the weight column (as specified in the loaded movement configurations).
#' @param range A positive number, indicating the maximum amount of jitter to
#'   apply to the numeric data (see Details).
#'
#' @details
#' Requires that an appropriate movement config file is loaded, to correctly
#' identify the `weight` column in `data`.
#'
#' The data in the selected column are modified by addition of an amount of
#' noise between `-range` and `range`, following a uniform distribution.
#' If this were to result in a data point becoming `<= 0`, the amount of jitter
#' for this data point is resampled, until the resulting data point becomes
#' positive. This is to capture that any movement in a livestock movement
#' database is assumed to have a positive weight (quantity of animals moved).
#'
#' @returns
#' A movement tibble like `data`, but with jitter applied to the selected
#' numeric column.
#'
#' @seealso [base::jitter()], which this function is based on.
#'
#' @import checkmate
#' @importFrom purrr has_element
#' @importFrom stats runif
#'
#' @export
jitter_weights <- function(data, range,
                           column = movenetenv$options$movedata_cols$weight){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded configurations do not match the required type of data (movement data). Please ensure an appropriate config file is loaded.")
  }

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = column,
               .var.name = "data")
  assert_data_frame(data[column], types = "numeric", ncols = 1)
  qassert(range, "N1(0,)") #a single finite numeric(double or int) of value > 0, not missing

  #####################
  ### Adding jitter ###
  #####################

  replacement_data <-
    data[[column]] +
    runif(length(data[[column]]), -range, +range)
  # Most suitable distribution? Currently uniform, but can change

  while (any(replacement_data <= 0)){

    #resampling if result of range is <= 0
    replacement_data[which(replacement_data <= 0)] <-
      data[[column]][which(replacement_data <= 0)] +
      runif(sum(replacement_data <= 0), -range, +range)
  }

  data[column] <- replacement_data

  return(data)

}

################################################################################
#' Round numeric movement data
#'
#' `round_weights()` rounds a selected numeric column in a movenet-format
#' movement tibble. By default, the weight column is selected.
#'
#' @param data A movenet-format movement tibble.
#' @param column Name of a single numeric column to round. By default this is
#'   the weight column (as specified in the loaded movement configurations).
#' @param unit A positive number. The data in the selected column are rounded to
#'   the nearest multiple of this number. `unit` is additionally set as minimum
#'   possible value for the column.
#'
#' @details
#' Requires that the appropriate movement config file is loaded, to correctly
#' identify the `weight` column in `data`.
#'
#' The data in the selected column are modified by rounding to multiples of
#' `unit`.
#' Additionally, any data points `< unit` are set to `unit`, so that this
#' becomes the minimum possible value in the column. This is to capture that any
#' livestock movement, no matter how small, has an inherent risk that is
#' conceptually closer to that of `unit`, than that of no movement at all.
#'
#' @returns
#' A movement tibble like `data`, but with rounding applied to the selected
#' numeric column.
#'
#' @seealso [plyr::round_any()], which this function wraps.
#'
#' @import checkmate
#' @importFrom plyr round_any
#' @importFrom purrr has_element
#'
#' @export
round_weights <- function(data, unit,
                          column = movenetenv$options$movedata_cols$weight){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded configurations do not match the required type of data (movement data). Please ensure an appropriate config file is loaded.")
  }

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = column,
               .var.name = "data")
  assert_data_frame(data[column], types = "numeric", ncols = 1)
  qassert(unit, "N1(0,)") #a single finite numeric(double or int) of value > 0, not missing

  ################
  ### Rounding ###
  ################

  data[column] <- round_any(data[[column]], accuracy = unit)

  data[column][which(data[column] < unit),] <- unit #set unit as minimum

  #is it more efficient to filter first & only round if data[column] > unit,
  #or to just round everything?

  #Also rounds 0-weight movements up to unit

  return(data)
}

################################################################################
#' Anonymise data by replacing holding identifiers with prefix-integer
#' combinations
#'
#' `anonymise()` anonymises a holding or movement data frame by replacing
#' holding identifiers with prefix-integer combinations. Both the anonymised
#' data frame and the anonymisation key are returned. By default, a new
#' anonymisation key is generated; alternatively, an existing key can be
#' provided.
#'
#' @param data A holding or movement data frame.
#' @param prefix Character string, to form the basis of anonymised holding
#'   identifiers. An integer will be appended to form this new identifier.
#' @param key A named character vector to be used as anonymisation key, or
#'   `NULL` (default) to generate a new key. A provided `key` should have
#'   original holding identifiers as names, and new (anonymised) identifiers as
#'   values.
#'
#' @details
#' Requires that the appropriate config file is loaded, to identify the
#' column(s) in `data` that contain(s) holding identifiers: origin (`from`) and
#' destination (`to`) columns for movement data, or the `id` column for holding
#' data.
#'
#' If `key == NULL` (default), a new anonymisation key is generated, with
#' holdings being given new identifiers consisting of `prefix` followed by an
#' integer ranging between 1 and the total number of holdings. Integers are
#' assigned to holdings in a random order.
#'
#' If an existing `key` is provided, its coverage of holding identifiers in
#' `data` is checked. If all holding identifiers in `data` are present among
#' element names in `key`, the `key` is used for anonymisation as-is: holding
#' identifiers in `data` are replaced with the values of elements of the same
#' name in `key`. Otherwise, if `data` contains holding identifiers that are not
#' present in `key`, the `key` is expanded by adding additional `prefix`-integer
#' combinations.
#'
#' @returns
#' A named list with two elements:
#' * `data` containing the anonymised data frame
#' * `key` containing the applied anonymisation key. This has the form of a
#' named character vector, with original holding identifiers as names, and new
#' (anonymised) identifiers as values.
#'
#' @import checkmate
#' @importFrom purrr has_element
#'
#' @export
anonymise <- function(data, prefix = NULL, key = NULL){

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert(
    (test_names(names(data),
                must.include = c(movenetenv$options$movedata_cols$from,
                                 movenetenv$options$movedata_cols$to))
     && !(is.null(movenetenv$options$movedata_cols$from))
     && !(is.null(movenetenv$options$movedata_cols$to))),
    (test_names(names(data),
                must.include = movenetenv$options$holdingdata_cols$id)
     && !(is.null(movenetenv$options$holdingdata_cols$id))),
    .var.name = c("'Data' must be movement data, including 'from' and 'to' columns as defined in the loaded configurations",
      "'Data' must be holding data, including an 'id' column as defined in the loaded configurations")
    )
  assert_string(prefix, null.ok = TRUE)
  assert_character(key, any.missing = FALSE, names = "unique", null.ok = TRUE,
                   unique = TRUE)


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
    "The loaded configurations and the type of data (movement or holding data)
    do not correspond. Please ensure an appropriate config file is loaded.")
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

  data <- replace_ids_w_key(data, col_to_anonymise, key)

  return(list(data = data,
              key = key))

}

#' Generate anonymisation key for holding identifiers
#'
#' `generate_anonymisation_key()` creates an anonymisation key that links
#' existing holding identifiers with new, non-identifiable, identifiers
#' (randomly allocated integers or prefix-integer combinations).
#'
#' @param ids Character vector with existing holding identifiers to generate
#'   replacement identifiers for
#' @param prefix Character string, to form the basis of replacement holding
#'   identifiers. An integer will be appended to form this new identifier.
#' @param n_start Number from which to start numbering the replacement identifiers
#'
#' @returns
#' A named character vector, with original holding identifiers as names
#' and new (non-identifiable) identifiers as values. Replacement identifiers
#' consist of `prefix` followed by an integer. Integers have consecutive values,
#' starting from `n_start`, and are allocated to holdings in a random order.
#'
#' @importFrom stats setNames
#'
#' @keywords internal
generate_anonymisation_key <- function(ids, prefix = '', n_start){

  ids <- unique(ids)

  ids_in_random_order <-
    if(length(ids) == 1) ids else sample(ids, size=length(ids), replace=FALSE)

  key <-
    setNames(paste0(prefix,
                    seq(n_start, length.out = length(ids_in_random_order))),
             ids_in_random_order)

  return(key)
}

#' Replace holding identifiers using a key
#'
#' `replace_ids_w_key()` replaces holding identifiers according to a user-
#' provided key
#'
#' @param data Dataframe or tibble (e.g. movement data)
#' @param col_to_anonymise Headers or indices of columns in `data` that
#'   contain the holding identifiers to be replaced
#' @param key Named character vector, with original holding identifiers as names
#'   and new identifiers as values.
#'
#' @returns
#' A modified version of `data` where, within columns indicated by
#' `col_to_anonymise`, holding identifers have been replaced according to `key`.
#'
#' @keywords internal
replace_ids_w_key <- function(data, col_to_anonymise, key){

  data[col_to_anonymise] <-
    lapply(col_to_anonymise, function(x){
      key[data[[x]]] %>%
      unname()}) #key names = original holding ids; remove to anonymise!

  return(data)
}
