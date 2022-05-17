test_that("validate_config.R accepts config file with expected structure",{

  ScotEID_config<-system.file("configurations", paste0("ScotEID", ".yml"),package="movenet")
  expect_true(validate_config(ScotEID_config))

})

test_that("validate_config.R accepts config file with additional fields",{

  morefields_config<-"test_input_files/ScotEID_testmore.yml"
  expect_true(validate_config(morefields_config))

})

test_that("validate_config.R returns FALSE and warnings when input is not a yaml file",{

  not_yaml<-"../../LICENSE.md"
  expect_false(validate_config(not_yaml))
  expect_warning(validate_config(not_yaml), "Input file not valid yaml format")

})

test_that("validate_config.R returns FALSE and warnings when input is an unrelated yaml file",{

  unrelated_yaml<-"../../DESCRIPTION"
  expect_false(validate_config(unrelated_yaml))
  expect_warning(validate_config(unrelated_yaml), "Missing top-level keys: movement_data, holding_data")
  expect_warning(validate_config(unrelated_yaml), "Missing movement_data keys")
  #expect_warning(validate_config(unrelated_yaml), "Missing holding_data keys")

})

test_that("validate_config.R returns FALSE and warnings when essential fields are missing",{

  missing_toplevel<-"test_input_files/ScotEID_testmissing2.yml"
  missing_field<-"test_input_files/ScotEID_testmissing.yml"
  missing_field_typo<-"test_input_files/ScotEID_testmissing3.yml"

  expect_false(validate_config(missing_toplevel))
  expect_false(validate_config(missing_field))
  expect_false(validate_config(missing_field_typo))

  expect_warning(validate_config(missing_toplevel), "Missing top-level keys: holding_data")
  expect_warning(validate_config(missing_field), "Missing movement_data keys: nr_pigs")
  expect_warning(validate_config(missing_field_typo), "Missing movement_data keys: move_date")

  #add tests that missing_field & missing_field_typo do NOT raise "Data fields not in expected character format" errors ?
})

test_that("validate_config.R returns FALSE and warnings when essential fields are not in expected format (character)",{

  numeric_data<-"test_input_files/ScotEID_testnumeric.yml"
  expect_false(validate_config(numeric_data))
  expect_warning(validate_config(numeric_data), "Data fields not in expected character format: nr_pigs")

})
