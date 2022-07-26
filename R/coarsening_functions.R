coarsen_date <- function(data, level){
  # accesses each date on an edge in network, changes to level aggregation
  # just coarsen the date or aggregate by date?

  # At what stage to do this? data and/or graph?

  ##Coarsening

  coarsened_data <- data %>% mutate("{movenetenv$options$movedata_cols$move_date}" := floor_date(.data[[movenetenv$options$movedata_cols$move_date]], level)) # result is date object, rounded down to start of unit (e.g. month)

  #Alternatives:
  # format_ISO8601(date_object, precision) from lubridate -> character vector matching ISO format to certain precision
  # format(date_object, format_string) from base R -> character vector matching format_string (e.g %Y-%m)
  # zoo::as.yearmon(date_object) -> yearmon object

  ##Aggregating

  # Aggregate over consistent periods of x number of days (as Lentz has done), or extract year-mon, year-quarter, etc?
  # Lentz uses 1d, 7d (weekly), 14d (fortnightly), 28d (monthly), 84d (quarterly) ?

  # can aggregate nr_pigs by summing, but what for other, non-standardised columns?
  # have user provide named list with functions? if so, also include nr_pigs, or assume this is always summed up?
  aggregated_data <-
    coarsened_data %>%
      group_by(.data[[movenetenv$options$movedata_cols$origin_ID]], .data[[movenetenv$options$movedata_cols$dest_ID]], .data[[movenetenv$options$movedata_cols$move_date]]) %>%
      summarise("{movenetenv$options$movedata_cols$nr_pigs}" := sum(.data[[movenetenv$options$movedata_cols$nr_pigs]])) %>%
      ungroup
  #Alternative:
  # Within graph analysis, can use igraph's simplify() with custom functions
}



coarsen_weight <- function(data, round, jitter){
  # for each weight on an edge in network, apply ± jitter in range jitter, then round to nearest round
  # might want user to be able to just jitter, just round, or have both
  # round then jitter, or jitter then round (tbh I don't see the point of the jitter in the latter)?
  # sensible default values?

  # The below requires data to be a particular column, e.g. nr_pigs

  ## jitter
  # suggest: FALSE for no jitter, or...
  # use base R jitter defaults? or alternative jitter function?
  if (jitter != FALSE){
    data <- jitter(data, factor, amount = jitter) # what about factor?
  }

  ## round
  # suggest: FALSE for no rounding, or power-of-ten to round to
  if (round != FALSE){
    data <- round(data, digits = -(round))
  }
  # (note rounding rule implementation is dependent on OS services and on representation error)

  #coarsened_weight <-
  #  data %>%
  #  mutate("{movenetenv$options$movedata_cols$nr_pigs}" := .data[[movenetenv$options$movedata_cols$move_date]])
}
