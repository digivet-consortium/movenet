test_that("are_ids_consec_intchars_from_1 returns false if the IDs are not numbers",{
  not_numbers <- c("alice's farm", "bob's farm", "charlie's farm")
  expect_false(are_ids_consec_intchars_from_1(not_numbers))
})

test_that("are_ids_consec_intchars_from_1 returns false if the IDs are not consecutive",{
  not_consecutive <- c("1", "2", "5")
  expect_false(are_ids_consec_intchars_from_1(not_consecutive))
})

test_that("are_ids_consec_intchars_from_1 returns true if the IDs are in order and consecutive",{
  right_ids <- c("1", "2", "3", "4")
  expect_true(are_ids_consec_intchars_from_1(right_ids))
})

test_that("are_ids_consec_intchars_from_1 still returns true if the IDs are out of order but consecutive",{
  right_ids <- c("2", "3", "1", "4")
  expect_true(are_ids_consec_intchars_from_1(right_ids))
})

test_that("holdingids2consecints works correctly with a small input", {
  load_config("ScotEID")
  movement_data <- reformat_data("test_input_files/ScotEID_testdata.csv", "movement")
  set.seed(1)
  new_movement_data <- holdingids2consecints(movement_data)$movement_data
  departure_dest_cph <- new_movement_data %>% select(departure_cph, dest_cph)
  expected_cph <- dplyr::tibble(departure_cph=c('1', '5', '3'), dest_cph=c('2', '6', '4'))
  expect_equal(departure_dest_cph, expected_cph)
})

test_that("holdingids2consecints works correctly with an empty input", {
  load_config("ScotEID")
  empty_movement_data <- dplyr::tibble(departure_cph=character(),
                                       dest_cph=character(),
                                       departure_date=date(),
                                       qty_pigs=double(),
                                       movement_reference=double())
  new_movement_data <- holdingids2consecints(empty_movement_data)$movement_data
  expect_equal(nrow(new_movement_data), 0)
})
