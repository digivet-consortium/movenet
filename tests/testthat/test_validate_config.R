test_that("validate_config.R accepts config file with expected structure",{

  ScotEID_config <- system.file("configurations", paste0("ScotEID", ".yml"),package="movenet")
  ScotEID_yaml <- yaml.load_file(ScotEID_config)

  expect_type(validate_yaml(ScotEID_config),"list")
  expect_true(validate_yaml(ScotEID_config)$test)
  expect_null(validate_yaml(ScotEID_config)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_silent(validate_config_root(ScotEID_yaml))
  expect_null(validate_config_root(ScotEID_yaml))

  expect_silent(validate_config_move(ScotEID_yaml))
  expect_null(validate_config_move(ScotEID_yaml))

  expect_silent(validate_config_datatype(ScotEID_yaml))
  expect_null(validate_config_datatype(ScotEID_yaml))

  expect_silent(internal_validate_config(ScotEID_config))
  expect_null(internal_validate_config(ScotEID_config))

  expect_silent(validate_config(ScotEID_config))
  expect_true(validate_config(ScotEID_config))
  expect_invisible(validate_config(ScotEID_config))

})

test_that("validate_config.R accepts config file with additional (top-level & movement_data) fields",{

  morefields_config <- "test_input_files/ScotEID_testmore.yml"
  morefields_yaml <- yaml.load_file(morefields_config)

  expect_silent(validate_config_root(morefields_yaml))
  expect_null(validate_config_root(morefields_yaml))

  expect_silent(validate_config_move(morefields_yaml))
  expect_null(validate_config_move(morefields_yaml))

  expect_silent(validate_config_datatype(morefields_yaml))
  expect_null(validate_config_datatype(morefields_yaml))

  expect_silent(internal_validate_config(morefields_config))
  expect_null(internal_validate_config(morefields_config))

  expect_silent(validate_config(morefields_config))
  expect_true(validate_config(morefields_config))
  expect_invisible(validate_config(morefields_config))

})

test_that("validate_config.R raises ERROR with informative message when stated input file does not exist",{

  expect_error(validate_config("foo"), "no such file exists")
  expect_error(internal_validate_config("foo"), "no such file exists")
  expect_error(validate_yaml("foo"), "no such file exists")

})

test_that("validate_config.R returns ERROR with informative message when input is not a yaml file",{

  not_yaml <- "../../LICENSE.md"

  expect_type(validate_yaml(not_yaml),"list")
  expect_false(validate_yaml(not_yaml)$test)
  expect_match(validate_yaml(not_yaml)$msg, "is not valid yaml format", fixed = TRUE)
  #test expect_visible / invisible for validate_yaml?

  expect_silent(internal_validate_config(not_yaml))
  expect_invisible(internal_validate_config(not_yaml))
  expect_length(internal_validate_config(not_yaml),1)
  expect_match(internal_validate_config(not_yaml), "is not valid yaml format", fixed = TRUE)

  error_msg <- expect_error(validate_config(not_yaml))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "is not valid yaml format", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when input is an unrelated yaml file",{

  unrelated_yaml <- "../../DESCRIPTION"
  unrelated_yaml_read <- yaml.load_file(unrelated_yaml)

  expect_type(validate_yaml(unrelated_yaml),"list")
  expect_true(validate_yaml(unrelated_yaml)$test)
  expect_null(validate_yaml(unrelated_yaml)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_silent(validate_config_root(unrelated_yaml_read))
  expect_match(validate_config_root(unrelated_yaml_read), "Missing mandatory top-level keys", fixed = TRUE)

  expect_silent(validate_config_move(unrelated_yaml_read))
  expect_match(validate_config_move(unrelated_yaml_read), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  expect_silent(validate_config_datatype(unrelated_yaml_read))
  expect_null(validate_config_datatype(unrelated_yaml_read))

  expect_silent(internal_validate_config(unrelated_yaml))
  expect_invisible(internal_validate_config(unrelated_yaml))
  expect_length(internal_validate_config(unrelated_yaml), 2)
  expect_match(internal_validate_config(unrelated_yaml)[1], "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(internal_validate_config(unrelated_yaml)[2], "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  error_msg <- expect_error(validate_config(unrelated_yaml))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory top-level keys are missing",{

  missing_toplevel <- "test_input_files/ScotEID_testmissing2.yml"
  missing_toplevel_yaml <- yaml.load_file(missing_toplevel)

  expect_silent(validate_config_root(missing_toplevel_yaml))
  expect_match(validate_config_root(missing_toplevel_yaml), "Missing mandatory top-level keys", fixed = TRUE)

  expect_silent(validate_config_move(missing_toplevel_yaml))
  expect_match(validate_config_move(missing_toplevel_yaml), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)
  #validate_config_move generates the missing message, despite keys being there - just not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated

  expect_silent(validate_config_datatype(missing_toplevel_yaml))
  expect_null(validate_config_datatype(missing_toplevel_yaml))

  expect_silent(internal_validate_config(missing_toplevel))
  expect_invisible(internal_validate_config(missing_toplevel))
  expect_length(internal_validate_config(missing_toplevel), 2)
  expect_match(internal_validate_config(missing_toplevel)[1], "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(internal_validate_config(missing_toplevel)[2], "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_toplevel))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory movement_data keys are missing",{

  missing_field <- "test_input_files/ScotEID_testmissing.yml"
  missing_field_yaml <- yaml.load_file(missing_field)

  expect_silent(validate_config_root(missing_field_yaml))
  expect_null(validate_config_root(missing_field_yaml))

  expect_silent(validate_config_move(missing_field_yaml))
  expect_match(validate_config_move(missing_field_yaml), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  expect_silent(validate_config_datatype(missing_field_yaml))
  expect_null(validate_config_datatype(missing_field_yaml))

  expect_silent(internal_validate_config(missing_field))
  expect_invisible(internal_validate_config(missing_field))
  expect_length(internal_validate_config(missing_field), 1)
  expect_match(internal_validate_config(missing_field), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_field))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory top-level and movement_data keys are missing",{

  missing_both <- "test_input_files/ScotEID_testmissing3.yml"
  missing_both_yaml <- yaml.load_file(missing_both)

  expect_silent(validate_config_root(missing_both_yaml))
  expect_match(validate_config_root(missing_both_yaml), "Missing mandatory top-level keys", fixed = TRUE)

  expect_silent(validate_config_move(missing_both_yaml))
  expect_match(validate_config_move(missing_both_yaml), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  expect_silent(validate_config_datatype(missing_both_yaml))
  expect_null(validate_config_datatype(missing_both_yaml))

  expect_silent(internal_validate_config(missing_both))
  expect_invisible(internal_validate_config(missing_both))
  expect_length(internal_validate_config(missing_both), 2)
  expect_match(internal_validate_config(missing_both)[1], "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(internal_validate_config(missing_both)[2], "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_both))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when an extra key level is present",{

  added_lvl <- "test_input_files/ScotEID_testextralevel.yml"
  added_lvl_yaml <- yaml.load_file(added_lvl)

  expect_silent(validate_config_root(added_lvl_yaml))
  expect_match(validate_config_root(added_lvl_yaml), "Missing mandatory top-level keys", fixed = TRUE)

  expect_silent(validate_config_move(added_lvl_yaml))
  expect_match(validate_config_move(added_lvl_yaml), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  expect_silent(validate_config_datatype(added_lvl_yaml))
  expect_null(validate_config_datatype(added_lvl_yaml))

  expect_silent(internal_validate_config(added_lvl))
  expect_invisible(internal_validate_config(added_lvl))
  expect_length(internal_validate_config(added_lvl), 2)
  expect_match(internal_validate_config(added_lvl)[1], "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(internal_validate_config(added_lvl)[2], "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  error_msg <- expect_error(validate_config(added_lvl))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level keys", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory fields are not in expected format (character)",{

  numeric_data <- "test_input_files/ScotEID_testnumeric.yml"
  numeric_yaml <- yaml.load_file(numeric_data)

  expect_silent(validate_config_move(numeric_yaml))
  expect_null(validate_config_move(numeric_yaml))

  expect_silent(validate_config_datatype(numeric_yaml))
  expect_match(validate_config_datatype(numeric_yaml), "Data fields not in expected character format", fixed = TRUE)

  expect_silent(internal_validate_config(numeric_data))
  expect_invisible(internal_validate_config(numeric_data))
  expect_length(internal_validate_config(numeric_data), 1)
  expect_match(internal_validate_config(numeric_data), "Data fields not in expected character format", fixed = TRUE)

  error_msg <- expect_error(validate_config(numeric_data))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Data fields not in expected character format", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory movement_data keys are missing and some also not in expected format (character)",{

  missing_field_numeric <- "test_input_files/ScotEID_testnumericmissing.yml"
  missing_numeric_yaml <- yaml.load_file(missing_field_numeric)

  expect_silent(validate_config_move(missing_numeric_yaml))
  expect_match(validate_config_move(missing_numeric_yaml), "Missing mandatory second-level (movement_data) keys", fixed = TRUE)

  expect_silent(validate_config_datatype(missing_numeric_yaml))
  expect_match(validate_config_datatype(missing_numeric_yaml), "Data fields not in expected character format", fixed = TRUE)

  expect_silent(internal_validate_config(missing_field_numeric))
  expect_invisible(internal_validate_config(missing_field_numeric))
  expect_length(internal_validate_config(missing_field_numeric), 2)
  expect_match(internal_validate_config(missing_field_numeric)[1], "Missing mandatory second-level (movement_data) keys", fixed = TRUE)
  expect_match(internal_validate_config(missing_field_numeric)[2], "Data fields not in expected character format", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_field_numeric))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movement_data) keys", fixed = TRUE)
  expect_match(error_msg$message, "Data fields not in expected character format", fixed = TRUE)

})
