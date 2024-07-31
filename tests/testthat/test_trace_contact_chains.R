#helper function to remove pseudo-random widget id that is generated anew each time
remove_widget_ids <- function(x) {
  gsub("htmlwidget-[a-z0-9]{20}","htmlwidget-",x)
}

test_that("trace_contact_chains() works when a single root is given, with tEnd, and both in- and outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              "95/216/1100", tEnd = "2019-07-01", 90)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "95/216/1100", tEnd = "2019-07-01", 90),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
 })

test_that("trace_contact_chains() works when a single root is given, with inBegin etc., and both in- and outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              "95/216/1100",
                              inBegin = "2019-04-01", inEnd = "2019-07-01",
                              outBegin = "2019-04-01", outEnd = "2019-07-01")
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "95/216/1100",
                                      inBegin = "2019-04-01", inEnd = "2019-07-01",
                                      outBegin = "2019-04-01", outEnd = "2019-07-01"),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)

})

test_that("trace_contact_chains() works when a single root is given, with tEnd, and only outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              "95/216/1100", tEnd = "2019-07-01", 30)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "95/216/1100", tEnd = "2019-07-01", 30),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
})

test_that("trace_contact_chains() works when a single root is given, with inBegin etc., and only outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              "95/216/1100",
                              inBegin = "2019-06-01", inEnd = "2019-07-01",
                              outBegin = "2019-06-01", outEnd = "2019-07-01")
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "95/216/1100",
                                      inBegin = "2019-06-01", inEnd = "2019-07-01",
                                      outBegin = "2019-06-01", outEnd = "2019-07-01"),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
})

test_that("trace_contact_chains() works when a single root is given, with tEnd, and only ingoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              "95/216/1100", tEnd = "2019-05-01", 15)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "95/216/1100", tEnd = "2019-05-01", 15),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
})

test_that("trace_contact_chains() prints message and doesn't generate a map, when no contact chains exist", {
  expect_message(trace_contact_chains(example_movement_data,
                                      example_holding_data,
                                      "95/216/1100", tEnd = "2019-05-01", 2),
                 "No ingoing or outgoing contact chains for root(s) 95/216/1100 during the search period.",
                 fixed = TRUE)
  expect_message(trace_contact_chains(example_movement_data,
                                      example_holding_data,
                                      "95/216/1100", tEnd = "2019-05-01", 2),
                 "No contact chains to plot.", fixed = TRUE)
  expect_null(trace_contact_chains(example_movement_data,
                                   example_holding_data,
                                   "95/216/1100", tEnd = "2019-05-01", 2)) #no map produced

  expect_message(trace_contact_chains(example_movement_data,
                                      example_holding_data,
                                      c("95/216/1100","76/613/8076"),
                                      tEnd = "2019-05-01", 2),
                 "No ingoing or outgoing contact chains for root(s) 95/216/1100, 76/613/8076 during the search period.",
                 fixed = TRUE)
  expect_message(trace_contact_chains(example_movement_data,
                                      example_holding_data,
                                      c("95/216/1100","76/613/8076"),
                                      tEnd = "2019-05-01", 2),
                 "No contact chains to plot.", fixed = TRUE)
  expect_null(trace_contact_chains(example_movement_data,
                                   example_holding_data,
                                   c("95/216/1100","76/613/8076"),
                                   tEnd = "2019-05-01", 2)) #no map produced
})

test_that("trace_contact_chains() works when multiple roots are given, and in- and outgoing contact chains exist for both roots", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              c("95/216/1100","76/613/8076"),
                              tEnd = "2019-05-01", 50)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      c("95/216/1100","76/613/8076"),
                                      tEnd = "2019-05-01", 50),
                 "Creating map with contact chains for root(s) 95/216/1100, 76/613/8076.",
                 fixed = TRUE)
})

test_that("trace_contact_chains() works when multiple roots are given, and contact chains exist for only one root", {
  map <- trace_contact_chains(example_movement_data, example_holding_data,
                              c("95/216/1100","76/613/8076"),
                              tEnd = "2019-05-01", 15)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      c("95/216/1100","76/613/8076"),
                                      tEnd = "2019-05-01", 15),
                 "No ingoing or outgoing contact chains for root(s) 76/613/8076 during the search period.",
                 fixed = TRUE)
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      c("95/216/1100","76/613/8076"),
                                      tEnd = "2019-05-01", 15),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
})

test_that("trace_contact_chains() throws an error when coordinates are not provided for all holdings", {
  expect_error(trace_contact_chains(example_movement_data,
                                    example_holding_data[1:2,],
                                    "95/216/1100", tEnd = "2019-05-01", 2),
               "Coordinates must be provided for all holdings in 'movement data', but not all holdings in 'movement_data' are present in 'holding_data'.",
               fixed = TRUE)
  example_holding_data$coordinates[[1]]<-sf::st_point()  #Create EMPTY geometry for one of the holdings
  expect_error(trace_contact_chains(example_movement_data, example_holding_data,
                                    "95/216/1100", tEnd = "2019-05-01", 2),
               "Assertion on 'holding_data[\"coordinates\"]' failed: Coordinates must be provided for all holdings, but some geometries are empty (missing coordinates).",
               fixed = TRUE)
})

test_that("trace_contact_chains() prints 'No ingoing or outgoing contact chains' message and doesn't generate map when root not found in movement data", {
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "XX", tEnd = "2019-07-01", 90),
                 "No ingoing or outgoing contact chains for root(s) XX during the search period.",
                 fixed = TRUE)
  expect_message(trace_contact_chains(example_movement_data, example_holding_data,
                                      "XX", tEnd = "2019-07-01", 90),
                 "No contact chains to plot.", fixed = TRUE)
  expect_null(trace_contact_chains(example_movement_data, example_holding_data,
                                   "XX", tEnd = "2019-07-01", 90))  #no map produced
})

test_that("trace_contact_chains() behaviour for multiple in/out/root statuses within a single contact chain is as expected", {
  movements <-
    structure(list(departure_cph = c("69/196/5890", "39/103/5541", "41/788/6464"),
                   dest_cph = c("39/103/5541", "41/788/6464", "69/196/5890"),
                   departure_date = structure(c(17899, 17897, 17898), class = "Date"),
                   qty_pigs = c(3, 5, 6)), row.names = c(NA, -3L),
                   class = c("tbl_df", "tbl", "data.frame"))
  map <- trace_contact_chains(movements, example_holding_data,
                              "39/103/5541", "2019-01-05", 5)
  expect_message(trace_contact_chains(movements, example_holding_data,
                                      "39/103/5541", "2019-01-05", 5),
                 "Creating map with contact chains for root(s) 39/103/5541.",
                 fixed = TRUE)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
  # In this scenario, the same 3 movements root-holding1-holding2-root could be seen as both ingoing and outgoing
  # contact chains, if one considers the respective dates.
  # However, only root-holding1-holding2 is considered outgoing, and only holding1-holding2-root as ingoing by
  # the underlying EpiContactTrace functionality.
  # Therefore, in the map:
  # - the root holding "39/103/5541" only appears as "root" and not as originating or destination holding.
  # - holdings "41/788/6464" and "69/196/5890" are shown as both originating and destination holdings.
  # - the contact chain "39/103/5541" (root) -> "41/788/6464" -> "69/196/5890" is shown as outgoing contact chain.
  # - the contact chain "41/788/6464" -> "69/196/5890" -> "39/103/5541" (root) is shown as ingoing contact chain.
  # - the movement "41/788/6464" -> "69/196/5890" has 2 arrows, for an ingoing and an outgoing movement respectively.
  # - the movement "39/103/5541" (root) -> "41/788/6464" is shown only as an outgoing movement, not an ingoing movement.
  # - the movement "69/196/5890" -> "39/103/5541" (root) is shown only as an ingoing movement, not an outgoing movement.
})

test_that("trace_contact_chains() works with duplicated movements and prints a message", {
  map <- trace_contact_chains(rbind(example_movement_data, example_movement_data),
                              example_holding_data,
                              "95/216/1100", tEnd = "2019-07-01", 90)
  expect_message(trace_contact_chains(rbind(example_movement_data, example_movement_data),
                                      example_holding_data,
                                      "95/216/1100", tEnd = "2019-07-01", 90),
                 "'movement_data' contains duplicated rows (movements). To optimise visibility on the map, aggregate all movements occurring between the same holdings on the same day, for example with `round_dates(movement_data, 'day')`.",
                 fixed = TRUE)
  expect_message(trace_contact_chains(rbind(example_movement_data, example_movement_data),
                                      example_holding_data,
                                      "95/216/1100", tEnd = "2019-07-01", 90),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() works with near-duplicate movements (difference in non-required field)", {
  movements <- rbind(example_movement_data, example_movement_data)
  movements$movement_reference <- c(example_movement_data$movement_reference,
                                    example_movement_data$movement_reference+10)

  map <- trace_contact_chains(movements, example_holding_data,
                              "95/216/1100", tEnd = "2019-07-01", 90)
  expect_message(trace_contact_chains(movements, example_holding_data,
                                      "95/216/1100", tEnd = "2019-07-01", 90),
                 "Creating map with contact chains for root(s) 95/216/1100.",
                 fixed = TRUE)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() throws an error with duplicate holding ids", {
  holdings <- rbind(example_holding_data, example_holding_data)
  expect_error(trace_contact_chains(example_movement_data, holdings,
                                    "95/216/1100", tEnd = "2019-07-01", 90),
               "Assertion on 'holding_data$cph' failed: Contains duplicated values, position 501.",
               fixed = TRUE)
})

