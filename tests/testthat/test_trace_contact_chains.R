#helper function to remove pseudo-random widget id that is generated anew each time
remove_widget_ids <- function(x) {
  gsub("htmlwidget-[a-z0-9]{20}","htmlwidget-",x)
}

test_that("trace_contact_chains() works when a single root is given, with single tEnd, and both in- and outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", tEnd = "2019-07-01", 90)
  expect_snapshot(map)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
 })

test_that("trace_contact_chains() works when a single root is given, with single tEnd, and only outgoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", tEnd = "2019-07-01", 30)
  expect_snapshot(map)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() works when a single root is given, with single tEnd, and only ingoing contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", tEnd = "2019-05-01", 15)
  expect_snapshot(map)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() works when a single root is given, with single tEnd, and no contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", tEnd = "2019-05-01", 2)
  expect_snapshot(map)
  expect_null(map)  #no map produced
})

test_that("trace_contact_chains() works when multiple roots are given, with single tEnd, and in- and outgoing contact chains exist for both roots", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, c("95/216/1100","76/613/8076"), tEnd = "2019-05-01", 50)
  expect_snapshot(map)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() works when multiple roots are given, with single tEnd, and contact chains exist for only one root", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, c("95/216/1100","76/613/8076"), tEnd = "2019-05-01", 15)
  expect_snapshot(map)
  expect_snapshot_output(remove_widget_ids(htmltools::renderTags(map)$html))
})

test_that("trace_contact_chains() works when multiple roots are given, with single tEnd, and no contact chains exist", {
  map <- trace_contact_chains(example_movement_data, example_holding_data, c("95/216/1100","76/613/8076"), tEnd = "2019-05-01", 2)
  expect_snapshot(map)
  expect_null(map)  #no map produced
})

test_that("trace_contact_chains() throws an error when coordinates are not provided for all holdings", {
  expect_error(trace_contact_chains(example_movement_data, example_holding_data[1:2,], "95/216/1100", tEnd = "2019-05-01", 2),
               "Coordinates must be provided for all holdings in 'movement data', but not all holdings in 'movement_data' are present in 'holding_data'.",
               fixed = TRUE)
  example_holding_data$coordinates[[1]]<-sf::st_point()  #Create EMPTY geometry for one of the holdings
  expect_error(trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", tEnd = "2019-05-01", 2),
               "Assertion on 'holding_data[\"coordinates\"]' failed: Coordinates must be provided for all holdings, but some geometries are empty (missing coordinates).",
               fixed = TRUE)
})
