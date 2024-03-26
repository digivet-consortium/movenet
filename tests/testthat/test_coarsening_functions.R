old_config<-movenetenv$options
suppressMessages(load_config("ScotEID"))
suppressMessages(load_config("fakeScotEID_holding.yml"))
unrelated_df <- as.data.frame("foo")
test_movement_data <- head(example_movement_data, 10)


# tests for jitter_dates() ------------------------------------------------

test_that("appropriate error is raised if `data` is not a dataframe",{
  expect_error(jitter_dates("f",5),
               "Must be of type 'data.frame', not 'character'.",
               fixed = TRUE)
})
test_that("appropriate error is raised if `data` is not a movement dataframe",{
  expect_error(jitter_dates(unrelated_df,5),
               "Names must include the elements {'departure_date'}, but is missing elements {'departure_date'}",
               fixed = TRUE)
})

#arg check for range is straight-forward, doesn't need a test
#config check is straight-forward, doesn't need a test
#basic addition of jitter is straight-forward, doesn't need a test

test_that("dates never exceed min/max dates of true dataset, even if run lots of times", {
  true_min_date <- min(test_movement_data$departure_date)
  true_max_date <- max(test_movement_data$departure_date)
  output <- replicate(100, jitter_dates(test_movement_data, 5)$departure_date,
                      simplify = FALSE)
  jittered_min_dates <- sapply(output,min)
  jittered_max_dates <- sapply(output,max)
  expect_lte(min(jittered_min_dates), as.numeric(true_min_date))
  expect_gte(max(jittered_max_dates), as.numeric(true_max_date))
})

test_that("jittered dates are not all the same as true dates", {
  jittered_dates <- jitter_dates(test_movement_data, 5)$departure_date
  true_dates <- test_movement_data$departure_date
  expect_failure(expect_equal(jittered_dates, true_dates))
})

test_that("jittered dates do not differ by more than `range` from true dates", {
  jittered_dates <- jitter_dates(test_movement_data, 5)$departure_date
  true_dates <- test_movement_data$departure_date
  date_differences <- true_dates - jittered_dates
  expect_lte(max(date_differences), 5)
  expect_gte(min(date_differences), -5)
})

# tests for round_dates() -------------------------------------------------

test_that("appropriate error is raised if `data` is not a dataframe",{
  expect_error(round_dates("f",5), "Must be of type 'data.frame', not 'character'.", fixed = TRUE)
})
test_that("appropriate error is raised if `data` is not a movement dataframe",{
  expect_error(round_dates(unrelated_df,5),
               "Names must include the elements {'departure_date'}, but is missing elements {'departure_date'}",
               fixed = TRUE)
})

#arg check for unit ?!
#arg check for week_start
#arg check for sum_weight is straight-forward, doesn't need a test
#arg check for ...

#config check is straight-forward, doesn't need a test
#basic use of floor_date is straight-forward, doesn't much testing

test_that("dates rounded to the month are not all the same as true dates", {
  rounded_dates <- round_dates(test_movement_data, "month", sum_weight = FALSE)$departure_date
  true_dates <- test_movement_data$departure_date
  expect_failure(expect_equal(rounded_dates, true_dates))
})

test_that("rounded dates do not differ by more than 1 `unit` from true dates", {
  rounded_dates <- round_dates(test_movement_data, "week", sum_weight = FALSE)$departure_date
  true_dates <- test_movement_data$departure_date
  date_differences <- true_dates - rounded_dates
  expect_lte(max(date_differences), 6)
})

test_that("if not aggregating by date, data with rounded dates has same number of rows and cols as true data", {
  rounded_data <- round_dates(test_movement_data, "week", sum_weight = FALSE)
  expect_equal(nrow(rounded_data), nrow(test_movement_data))
  expect_equal(ncol(rounded_data), ncol(test_movement_data))
})

#aggregating by date, weights only

test_that("rounding dates while aggregating weights results in correct number of rows and only keeps required cols", {
  true_data <- rbind(test_movement_data, test_movement_data)
  rounded_aggr_data <- round_dates(true_data, "month")
  expect_equal(nrow(rounded_aggr_data), nrow(test_movement_data))
  expect_equal(ncol(rounded_aggr_data), 4)
  expect_named(rounded_aggr_data,
               c('departure_cph', 'dest_cph', 'departure_date', 'qty_pigs'))
})

test_that("rounding dates while aggregating weights produces correct weights and dates", {
  true_data <- rbind(test_movement_data, test_movement_data)
  rounded_aggr_data <- round_dates(true_data, "month") %>% dplyr::arrange(qty_pigs, departure_cph)
  expect_equal(rounded_aggr_data$qty_pigs, sort(test_movement_data$qty_pigs)*2)
  expect_equal(sort(rounded_aggr_data$departure_date),
               sort(floor_date(test_movement_data$departure_date, "month")))
})

#aggregating by date, non-weight field only

#conceptual error in round_dates ...
#round_dates(true_data, "month", sum_weight = FALSE, movement_reference = sum(movement_reference))
#results in weights col being removed. This should either be aggregated or return some error...

test_that("rounding dates while aggregating movement refs results in correct number of rows and cols", {
  true_data <- rbind(test_movement_data, test_movement_data)
  rounded_aggr_data <- round_dates(true_data, "month", sum_weight = FALSE, movement_reference = sum(movement_reference))
  expect_equal(nrow(rounded_aggr_data), nrow(test_movement_data))
  expect_equal(ncol(rounded_aggr_data), ncol(test_movement_data))
  expect_named(rounded_aggr_data,
               c('departure_cph', 'dest_cph', 'departure_date', 'qty_pigs', 'movement_reference'))
})

#aggregating by date, weights AND non-weight field


# tests for jitter_weights() ----------------------------------------------

test_that("appropriate error is raised if `data` is not a dataframe",{
  expect_error(jitter_weights("f",5), "Must be of type 'data.frame', not 'character'.", fixed = TRUE)
})
test_that("appropriate error is raised if `data` is not a movement dataframe",{
  expect_error(jitter_weights(unrelated_df,5),
               "Names must include the elements {'qty_pigs'}, but is missing elements {'qty_pigs'}",
               fixed = TRUE)
})

#arg check for range is straight-forward, doesn't need a test
#arg check for column is straight-forward, doesn't need a test
#config check is straight-forward, doesn't need a test
#basic addition of jitter is straight-forward, doesn't need a test

test_that("jittered weights are always > 0, even if function run lots of times", {
  output <- replicate(100, jitter_weights(test_movement_data, 150)$qty_pigs,
                      simplify = FALSE)
  jittered_min_weights <- sapply(output,min)
  expect_gt(min(jittered_min_weights), 0)
})

test_that("jittered weights are not all the same as true weights", {
  jittered_weights <- jitter_weights(test_movement_data, 5)$qty_pigs
  true_weights <- test_movement_data$qty_pigs
  expect_failure(expect_equal(jittered_weights, true_weights))
})

test_that("jittered weights do not differ by more than `range` from true weights", {
  jittered_weights <- jitter_weights(test_movement_data, 5)$qty_pigs
  true_weights <- test_movement_data$qty_pigs
  weight_differences <- true_weights - jittered_weights
  expect_lte(max(weight_differences), 5)
  expect_gte(min(weight_differences), -5)
})

# tests for round_weights() -----------------------------------------------

test_that("appropriate error is raised if `data` is not a dataframe",{
  expect_error(round_weights("f",5), "Must be of type 'data.frame', not 'character'.", fixed = TRUE)
})

test_that("appropriate error is raised if `data` is not a movement dataframe",{
  expect_error(round_weights(unrelated_df,5),
               "Names must include the elements {'qty_pigs'}, but is missing elements {'qty_pigs'}",
               fixed = TRUE)
})

test_that("rounded weights are always >= `unit`", {
  rounded_weights <- round_weights(test_movement_data, 100)$qty_pigs
  expect_gte(min(rounded_weights), 100)
})

test_that("rounded weights are not all the same as true weights", {
  rounded_weights <- round_weights(test_movement_data, 5)$qty_pigs
  true_weights <- test_movement_data$qty_pigs
  expect_failure(expect_equal(rounded_weights, true_weights))
})

test_that("rounded weights do not differ by more than half a `unit` from true weights, unless the minimum value is applied", {
  rounded_weights <- round_weights(test_movement_data, 75)$qty_pigs
  true_weights <- test_movement_data$qty_pigs
  weight_differences <- true_weights - rounded_weights
  expect_lte(max(weight_differences), 37.5)
  rounded_weights_not_set_to_min <- rounded_weights[which(!(rounded_weights == 75))]
  true_weights_not_set_to_min <- true_weights[which(!(rounded_weights == 75))]
  weight_differences_for_those_not_set_to_min <-
    true_weights_not_set_to_min - rounded_weights_not_set_to_min
  expect_gte(min(weight_differences_for_those_not_set_to_min), -37.5)
})

test_that("data with rounded weights have the same number of rows and cols as true data", {
  rounded_data <- round_weights(test_movement_data, 5)
  expect_equal(nrow(rounded_data), nrow(test_movement_data))
  expect_equal(ncol(rounded_data), ncol(test_movement_data))
})

# tests for anonymise() ---------------------------------------------------

test_that("appropriate error is raised if `data` is not a dataframe",{
  expect_error(anonymise("f"), "Must be of type 'data.frame', not 'character'.", fixed = TRUE)
})
test_that("appropriate error is raised if `data` is not a movement or holding dataframe",{
  expect_error(anonymise(unrelated_df),
               "'Data' must be movement data, including 'from' and 'to' columns",
               fixed = TRUE)
})

#test config does not match data

#test prefix does not match with provided key

#test output of anonymise does not have cols with names as attributes
#(previously the anonymised cols had their old ids as names still)

# tests for generate_anonymisation_key() ----------------------------------

# tests for replace_ids_w_key() -------------------------------------------


movenetenv$options<-old_config

