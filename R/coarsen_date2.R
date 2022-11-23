#' Jitter and/or round movement dates, and/or summarise data by time unit
#'
#' `coarsen_date2()` applies jitter to, and/or rounds, movement dates in a
#' movement data frame. Rounding is to the nearest boundary value of the
#' specified time unit. It optionally summarises movement data with the same
#' origin and destination by this time unit. By default, this involves summation
#' of weights, but alternative or additional summary functions can be provided
#' through `...`.
#'
#' @param data A movement data frame.
#' @param jitter Either a positive integer, indicating the amount of jitter (in
#'   days) to apply (see Details), or `FALSE` to not apply any jitter.
#' @param rounding_unit Either a character string specifying a time unit or a
#'   multiple of a unit for movement dates to be rounded to, or `FALSE` to not
#'   apply any rounding. Valid base units are `day`, `week`, `month`, `bimonth`,
#'   `quarter`, `season`, `halfyear` and `year`. Arbitrary unique English
#'   abbreviations as in the [lubridate::period()] constructor are allowed.
#'   Rounding to multiples of units (except weeks) is supported.
#' @param sum_weight If this is `TRUE` (the default), weights are summed over
#'   the specified `rounding_unit`, for all rows with the same origin and
#'   destination. The name of the weight column will remain the same.
#' @param ... [<`data-masking`>][dplyr::dplyr_data_masking] Additional or
#'   alternative summary function(s), of the form name = value, to pass on to
#'   [dplyr::summarise()]. Any summary functions will be applied to `data`,
#'   grouped by origin, destination, and rounded date. The specified name will
#'   be the column name in the resulting data frame. Be careful when using
#'   existing names: the corresponding columns will be immediately updated with
#'   the new data and this can affect subsequent operations referring to this
#'   name.
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
#' and then rounded.
#'
#' If `jitter > 0`, movement dates are modified by addition of a number of days
#' between `-jitter` and `jitter`, following a discrete uniform distribution. If
#' this were to result in a date being moved beyond the original date range, the
#' amount of jitter for this date is resampled, until the resulting date is
#' within the original date range.
#' If `jitter == FALSE` (or `jitter == 0`), no jitter is applied.
#'
#' If `rounding_unit` is a character string, movement dates are rounded to the
#' nearest boundary value of the `rounding_unit`. For rounding dates which are
#' exactly halfway between two consecutive time units, the convention is to
#' round up.
#' If `rounding_unit == FALSE`, no rounding is applied.
#'
#' If `rounding_unit == FALSE` and `sum_weight == TRUE`, weights are summed over
#' all rows in `data` with the same origin, destination, and rounded date.
#' If `rounding_unit == FALSE`, `sum_weight` is ignored.
#'
#' If `rounding_unit == FALSE` and any summary functions are provided through
#' `...`, the specified data are summarised accordingly, over all rows in `data`
#' with the same origin, destination, and rounded date.
#' If `rounding_unit == FALSE`, `...` is ignored.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting data frame.
#'
#' @returns
#' A movement data frame like `data`, but with jittered and/or rounded
#' movement dates.
#'
#' If `sum_weight == TRUE` or any summary functions are provided through `...`,
#' the returned data frame contains data summarised by origin, destination, and
#' `rounding_unit`, and may thus have a decreased length compared to `data`.
#'
#' Columns for which a summary function is not provided, are dropped from
#' the resulting data frame.
#'
#' @seealso [lubridate::round_date()], [dplyr::summarise()] which this function
#' wraps.
#'
#' @import checkmate
#' @importFrom dplyr group_by mutate rename summarise ungroup
#' @importFrom lubridate round_date
#'
#' @export
coarsen_date2 <- function(data, jitter, rounding_unit, sum_weight = TRUE, ...){

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
                paste0(data,"[[", movenetenv$options$movedata_cols$date,"]]"))
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
  #   - rounding_unit (unit from round_date)
  #   - ... (var = value pairs, in summarise)


  #####################
  ### Adding jitter ###
  #####################

  if (jitter != FALSE | jitter != 0){

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


  ######################
  ### Rounding dates ###
  ######################

  if (rounding_unit != FALSE){
  # round each date to the nearest first date of the rounding_unit (e.g. month)
    rounded_data <-
      data |>
      mutate("{movenetenv$options$movedata_cols$date}" :=
               round_date(.data[[movenetenv$options$movedata_cols$date]],
                          rounding_unit))

  #What to do with round_date's week_start? default = 7 = Sunday.
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
  ### Aggregating by date ###  (only if rounding_unit != FALSE)
  ###########################

    if (sum_weight == TRUE){

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
