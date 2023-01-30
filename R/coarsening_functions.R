#' Jitter and/or round down movement dates, and/or summarise data by time unit
#'
#' `coarsen_date()` applies jitter to, and/or rounds down, movement dates in a
#' movement data frame. Rounding down is to the first day of the specified time
#' unit. It optionally summarises movement data with the same origin and
#' destination by this time unit. By default, this involves summation of
#' weights, but alternative or additional summary functions can be provided
#' through `...`.
#'
#' @param data A movement data frame.
#' @param jitter Either a positive integer, indicating the amount of jitter (in
#'   days) to apply (see Details), or `FALSE` to not apply any jitter.
#' @param rounding_unit Either a character string specifying a time unit or a
#'   multiple of a unit for movement dates to be rounded down to, or `FALSE` to
#'   not apply any rounding. Valid base units are `day`, `week`, `month`,
#'   `bimonth`, `quarter`, `season`, `halfyear` and `year`. Arbitrary unique
#'   English abbreviations as in the [lubridate::period()] constructor are
#'   allowed. Rounding to multiples of units (except weeks) is supported.
#' @param week_start Week start day, only relevant for rounding down dates and/
#'   or aggregating data by week. Default is the value of the
#'   `lubridate.week.start` option, or 7 (Sunday) if this option is not set.
#'   Full or abbreviated names of the days of the week can be in English or as
#'   provided by the current locale.
#' @param sum_weight If this is `TRUE` (the default), weights are summed over
#'   the specified `rounding_unit`, for all rows with the same origin and
#'   destination. The name of the weight column will remain the same.
#' @param ... [<`data-masking`>][dplyr::dplyr_data_masking] Additional or
#'   alternative summary function(s), of the form name = value, to pass on to
#'   [dplyr::summarise()]. Any summary functions will be applied to `data`,
#'   grouped by origin, destination, and rounded-down date. The specified name
#'   will be the column name in the resulting data frame.
#'   The value can be:
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length n, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#'
#' @details
#' Requires that the appropriate movement config file is loaded, to correctly
#' identify origin (`from`), destination (`to`), `date` and `weight` columns in
#' `data`.
#'
#' If both jitter and rounding are applied, movement dates are first jittered
#' and then rounded down.
#'
#' If `jitter > 0`, movement dates are modified by addition of a number of days
#' between `-jitter` and `jitter`, following a discrete uniform distribution. If
#' this were to result in a date being moved beyond the original date range, the
#' amount of jitter for this date is resampled, until the resulting date is
#' within the original date range.
#' If `jitter` is `FALSE` (or `jitter == 0`), no jitter is applied.
#'
#' If `rounding_unit` is a character string, movement dates are rounded to the
#' first day of the `rounding_unit`.
#' If `rounding_unit` is `FALSE`, no rounding is applied.
#'
#' If `rounding_unit` is a character string and `sum_weight` is `TRUE`, weights
#' are summed over all rows in `data` with the same origin, destination, and
#' rounded-down date. The summed weight column is temporarily renamed, so that
#' any additional weight-dependent summary functions passed through `...` use
#' the original weights rather than the sums.
#' If `rounding_unit` is `FALSE`, `sum_weight` is ignored.
#'
#' If `rounding_unit` is a character string and any summary functions are
#' provided through `...`, the specified data are summarised accordingly, over
#' all rows in `data` with the same origin, destination, and rounded-down date.
#' Be careful when using existing names: the corresponding columns will be
#' immediately updated with the new data and this can affect subsequent
#' operations referring to this name.
#' If `rounding_unit` is `FALSE`, `...` is ignored.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting data frame.
#'
#' @returns
#' A movement data frame like `data`, but with jittered and/or rounded-down
#' movement dates.
#'
#' If `sum_weight` is `TRUE` or any summary functions are provided through
#' `...`, the returned data frame contains data summarised by origin,
#' destination, and `rounding_unit`, and may thus have a decreased length
#' compared to `data`.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting data frame.
#'
#' @seealso [lubridate::floor_date()], [dplyr::summarise()] which this function
#' wraps.
#'
#' @import checkmate
#' @importFrom dplyr group_by mutate rename summarise ungroup
#' @importFrom lubridate floor_date
#' @importFrom purrr has_element
#'
#' @export
coarsen_date <- function(data, jitter,
                         rounding_unit,
                         week_start = getOption("lubridate.week.start", 7),
                         sum_weight = TRUE, ...){

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded config file does not match the type of data (movement
    data). Please ensure the appropriate config file is loaded.")
  }

  dates <- data[[movenetenv$options$movedata_cols$date]]

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = movenetenv$options$movedata_cols$date)
  assert_date(dates,
              .var.name =
                paste0("data[[", movenetenv$options$movedata_cols$date,"]]"))
  assert(
    check_integerish(jitter, len = 1, lower = 0, any.missing = FALSE),
    check_false(jitter, na.ok = FALSE)
  )
  assert(
    check_character(rounding_unit, len = 1, any.missing = FALSE), #this does not check accepted/meaningful values
    check_false(rounding_unit, na.ok = FALSE)
  )
  assert_logical(sum_weight, len = 1)

  if(isFALSE(rounding_unit) & (isTRUE(sum_weight) | ...length() != 0)){
    warning("As 'rounding_unit' is FALSE, no rounding or summarising by date has
    been performed. Arguments 'sum_weight' and '...' have been ignored.",
    call. = FALSE)
  }

  # How does one check the other arguments?
  #   - rounding_unit (unit from floor_date; currently checking for character type, a subset of original requirements)
  #   - week_start (from floor_date)
  #   - ... (from summarise; format should be var = value pairs)


  #####################
  ### Adding jitter ###
  #####################

  if (!isFALSE(jitter) | jitter != 0){

    jitter <- as.integer(jitter)

    replacement_data <-
      dates +
      sample(c(-jitter:jitter),
             length(dates),
             replace = TRUE)

    while (any(replacement_data < min(dates) | replacement_data > max(dates))){

      #resampling for dates beyond boundaries
      replacement_data[which(replacement_data < min(dates) |
                               replacement_data > max(dates))] <-
        dates[which(replacement_data < min(dates) |
                      replacement_data > max(dates))] +
        sample(c(-jitter:jitter),
               sum(any(replacement_data < min(dates) |
                         replacement_data > max(dates))),
               replace = TRUE)

    }

    data[movenetenv$options$movedata_cols$date] <- replacement_data
  }

  # Currently adds uniform ints between -jitter and +jitter , including 0 !


  ###########################
  ### Rounding down dates ###
  ###########################

  if (!isFALSE(rounding_unit)){
  # round down each date to the first date of the rounding_unit (e.g. month)
    rounded_data <-
      data |>
      mutate("{movenetenv$options$movedata_cols$date}" :=
               floor_date(.data[[movenetenv$options$movedata_cols$date]],
                          unit = rounding_unit,
                          week_start = week_start))

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
  ### Aggregating by date ###   (only if !isFALSE(rounding_unit))
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

      return(aggregated_data)

    } else if(...length() == 0){

      return(rounded_data)

    } else {

      aggregated_data <-
        rounded_data |>
        group_by(.data[[movenetenv$options$movedata_cols$from]],
                 .data[[movenetenv$options$movedata_cols$to]],
                 .data[[movenetenv$options$movedata_cols$date]]) |>
        summarise(...) |>
        ungroup()

      return(aggregated_data)
    }

  ###############################################
  ### Returning jittered data, if no rounding ###
  ###############################################

  } else return(data)
}

################################################################################
#' Jitter (add noise to) and/or round numeric movement data
#'
#' `coarsen_weight()` applies jitter to, and/or rounds, a selected numeric
#' column in a movement data frame. By default, the weight column is selected.
#'
#' @param data A movement data frame.
#' @param column Name of a single numeric column to jitter and/or round. By
#'   default this is the name of the weight column (as given in the movement
#'   config file).
#' @param jitter Either a positive number, indicating the amount of jitter to
#'   apply (see Details), or `FALSE` to not apply any jitter.
#' @param round Either a number, or `FALSE` to not apply any rounding. The data
#'   in the selected column are rounded to the nearest multiple of this number.
#'   `round` is additionally set as minimum possible value for the column.
#'
#' @details
#' Requires that the appropriate movement config file is loaded, to correctly
#' identify the `weight` column in `data`.
#'
#' If both jitter and rounding are applied, the data in the selected column are
#' first jittered and then rounded.
#'
#' If `jitter > 0`, the data in the selected column are modified by addition of
#' an amount of noise between `-jitter` and `jitter`, following a uniform
#' distribution. If this were to result in a data point becoming `<= 0`, the
#' amount of jitter for this data point is resampled, until the resulting data
#' point becomes positive. This is to capture that any movement in a livestock
#' movement database, is assumed to have a positive weight (quantity of animals
#' moved).
#' If `jitter` is `FALSE` (or `jitter == 0`), no jitter is applied.
#'
#' If `round > 0`, the data in the selected column are modified by rounding to
#' multiples of `round`.
#' Additionally, any data points `< round` are set to `round`, so that this
#' becomes the minimum possible value in the column. This is to capture that any
#' livestock movement, no matter how small, has an inherent risk that is
#' conceptually closer to that of `round`, than that of no movement at all.
#' If `round` is `FALSE` (or `round == 0`), no rounding is applied.
#'
#' @returns
#' A movement data frame like `data`, but with jitter and/or rounding applied to
#' the selected numeric column.
#'
#' @seealso [base::jitter()], which the jitter part of this function is based
#' on; [plyr::round_any()], which this function wraps.
#'
#' @import checkmate
#' @importFrom plyr round_any
#' @importFrom purrr has_element
#'
#' @export
coarsen_weight <- function(data,
                           column = movenetenv$options$movedata_cols$weight,
                           jitter, round){

  # any sensible default values for jitter and round?

  #########################
  ### Config file check ###
  #########################

  if (!has_element(names(movenetenv$options), "movedata_cols")){
    stop("The loaded config file does not match the type of data (movement
    data). Please ensure the appropriate config file is loaded.")
  }


  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(data)
  assert_names(names(data),
               must.include = column)
  assert_data_frame(data[column], type = "numeric", ncols = 1)
  assert(
    check_numeric(jitter, len = 1, lower = 0),
    check_false(jitter, na.ok = FALSE)
  )
  assert(
    check_numeric(round, len = 1, lower = 0),
    check_false(round, na.ok = FALSE)
  )


  #####################
  ### Adding jitter ###
  #####################

  if (!isFALSE(jitter) | jitter != 0){

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

  if (!isFALSE(round) | round != 0){

    data[column] <- round_any(data[[column]], accuracy = round)

    data[column][which(data[column] < round),] <- round #set round as minimum

    #is it more efficient to filter first & only round if data[column] > round,
    #or to just round everything?
  }

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

  data[col_to_anonymise] <-
    lapply(col_to_anonymise, function(x){unname(key[data[[x]]])})

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
