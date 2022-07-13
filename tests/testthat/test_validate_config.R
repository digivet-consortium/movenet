test_that("validate_config.R accepts config file with expected structure",{

  ScotEID_config <- system.file("configurations", paste0("ScotEID", ".yml"),package="movenet")
  ScotEID_yaml <- yaml.load_file(ScotEID_config)

  expect_type(validate_yaml(ScotEID_config),"list")
  expect_true(validate_yaml(ScotEID_config)$test)
  expect_null(validate_yaml(ScotEID_config)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_condition(validate_config_root(ScotEID_yaml), NA)
  expect_null(validate_config_root(ScotEID_yaml))

  expect_condition(validate_config_moveopts(ScotEID_yaml), NA)
  expect_null(validate_config_moveopts(ScotEID_yaml))

  expect_condition(validate_config_movecols(ScotEID_yaml), NA)
  expect_null(validate_config_movecols(ScotEID_yaml))

  expect_condition(validate_config_datatype(ScotEID_yaml), NA)
  expect_null(validate_config_datatype(ScotEID_yaml))

  expect_condition(internal_validate_config(ScotEID_config), NA)
  expect_null(internal_validate_config(ScotEID_config))

  expect_condition(validate_config(ScotEID_config), NA)
  expect_true(validate_config(ScotEID_config))
  expect_invisible(validate_config(ScotEID_config))

})

test_that("validate_config.R accepts config file with additional (top-level & movedata_cols) fields",{

  morefields_config <- "test_input_files/ScotEID_testmore.yml"
  morefields_yaml <- yaml.load_file(morefields_config)

  expect_condition(validate_config_root(morefields_yaml), NA)
  expect_null(validate_config_root(morefields_yaml))

  expect_condition(validate_config_moveopts(morefields_yaml), NA)
  expect_null(validate_config_moveopts(morefields_yaml))

  expect_condition(validate_config_movecols(morefields_yaml), NA)
  expect_null(validate_config_movecols(morefields_yaml))

  expect_condition(validate_config_datatype(morefields_yaml), NA)
  expect_null(validate_config_datatype(morefields_yaml))

  expect_condition(internal_validate_config(morefields_config), NA)
  expect_null(internal_validate_config(morefields_config))

  expect_condition(validate_config(morefields_config), NA)
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

  expect_condition(internal_validate_config(not_yaml), NA)
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

  expect_condition(validate_config_root(unrelated_yaml_read), NA)
  expect_match(validate_config_root(unrelated_yaml_read), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_moveopts(unrelated_yaml_read), NA)
  expect_match(validate_config_moveopts(unrelated_yaml_read), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_movecols(unrelated_yaml_read), NA)
  expect_match(validate_config_movecols(unrelated_yaml_read), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(unrelated_yaml_read), NA)
  expect_null(validate_config_datatype(unrelated_yaml_read))

  expect_condition(internal_validate_config(unrelated_yaml), NA)
  expect_invisible(internal_validate_config(unrelated_yaml))
  expect_length(internal_validate_config(unrelated_yaml), 3)
  expect_match(internal_validate_config(unrelated_yaml)[1], "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(internal_validate_config(unrelated_yaml)[2], "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(internal_validate_config(unrelated_yaml)[3], "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(unrelated_yaml))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory top-level keys are missing",{

  missing_toplevel <- "test_input_files/ScotEID_testmissing2.yml"
  missing_toplevel_yaml <- yaml.load_file(missing_toplevel)

  expect_condition(validate_config_root(missing_toplevel_yaml), NA)
  expect_match(validate_config_root(missing_toplevel_yaml), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_moveopts(missing_toplevel_yaml), NA)
  expect_match(validate_config_moveopts(missing_toplevel_yaml), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_movecols(missing_toplevel_yaml), NA)
  expect_match(validate_config_movecols(missing_toplevel_yaml), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)
  #validate_config_movecols generates the missing message, despite keys being there - just not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated

  expect_condition(validate_config_datatype(missing_toplevel_yaml), NA)
  expect_null(validate_config_datatype(missing_toplevel_yaml))

  expect_condition(internal_validate_config(missing_toplevel), NA)
  expect_invisible(internal_validate_config(missing_toplevel))
  expect_length(internal_validate_config(missing_toplevel), 3)
  expect_match(internal_validate_config(missing_toplevel)[1], "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_toplevel)[2], "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_toplevel)[3], "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_toplevel))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory movedata_cols keys are missing",{

  missing_field <- "test_input_files/ScotEID_testmissing.yml"
  missing_field_yaml <- yaml.load_file(missing_field)

  expect_condition(validate_config_root(missing_field_yaml), NA)
  expect_null(validate_config_root(missing_field_yaml))

  expect_condition(validate_config_moveopts(missing_field_yaml), NA)
  expect_null(validate_config_moveopts(missing_field_yaml))

  expect_condition(validate_config_movecols(missing_field_yaml), NA)
  expect_match(validate_config_movecols(missing_field_yaml), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_field_yaml), NA)
  expect_null(validate_config_datatype(missing_field_yaml))

  expect_condition(internal_validate_config(missing_field), NA)
  expect_invisible(internal_validate_config(missing_field))
  expect_length(internal_validate_config(missing_field), 1)
  expect_match(internal_validate_config(missing_field), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_field))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory top-level and movedata_cols keys are missing",{

  missing_both <- "test_input_files/ScotEID_testmissing3.yml"
  missing_both_yaml <- yaml.load_file(missing_both)

  expect_condition(validate_config_root(missing_both_yaml), NA)
  expect_match(validate_config_root(missing_both_yaml), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_moveopts(missing_both_yaml), NA)
  expect_match(validate_config_moveopts(missing_both_yaml), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_movecols(missing_both_yaml), NA)
  expect_match(validate_config_movecols(missing_both_yaml), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_both_yaml), NA)
  expect_null(validate_config_datatype(missing_both_yaml))

  expect_condition(internal_validate_config(missing_both), NA)
  expect_invisible(internal_validate_config(missing_both))
  expect_length(internal_validate_config(missing_both), 3)
  expect_match(internal_validate_config(missing_both)[1], "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_both)[2], "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_both)[3], "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_both))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when an extra key level is present",{

  added_lvl <- "test_input_files/ScotEID_testextralevel.yml"
  added_lvl_yaml <- yaml.load_file(added_lvl)

  expect_condition(validate_config_root(added_lvl_yaml), NA)
  expect_match(validate_config_root(added_lvl_yaml), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_moveopts(added_lvl_yaml), NA)
  expect_match(validate_config_moveopts(added_lvl_yaml), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_movecols(added_lvl_yaml), NA)
  expect_match(validate_config_movecols(added_lvl_yaml), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(added_lvl_yaml), NA)
  expect_null(validate_config_datatype(added_lvl_yaml))

  expect_condition(internal_validate_config(added_lvl), NA)
  expect_invisible(internal_validate_config(added_lvl))
  expect_length(internal_validate_config(added_lvl), 3)
  expect_match(internal_validate_config(added_lvl)[1], "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(internal_validate_config(added_lvl)[2], "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(internal_validate_config(added_lvl)[3], "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(added_lvl))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

})

test_that("validate_config.R accepts config file with expected structure, with integer movedata_cols keys",{

  numeric_data <- "test_input_files/ScotEID_testint.yml"
  numeric_yaml <- yaml.load_file(numeric_data)

  expect_type(validate_yaml(numeric_data),"list")
  expect_true(validate_yaml(numeric_data)$test)
  expect_null(validate_yaml(numeric_data)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_condition(validate_config_moveopts(numeric_yaml), NA)
  expect_null(validate_config_moveopts(numeric_yaml))

  expect_condition(validate_config_movecols(numeric_yaml), NA)
  expect_null(validate_config_movecols(numeric_yaml))

  expect_condition(validate_config_datatype(numeric_yaml), NA)
  expect_null(validate_config_datatype(numeric_yaml))

  expect_condition(internal_validate_config(numeric_data), NA)
  expect_null(internal_validate_config(numeric_data))

  expect_condition(validate_config(numeric_data), NA)
  expect_true(validate_config(numeric_data))
  expect_invisible(validate_config(numeric_data))

})

test_that("validate_config.R returns ERROR with informative messages when keys are not in expected format",{

  wrongformat_data <- "test_input_files/ScotEID_testwrongformat.yml"
  wrongformat_yaml <- yaml.load_file(wrongformat_data)

  expect_condition(validate_config_moveopts(wrongformat_yaml), NA)
  expect_null(validate_config_moveopts(wrongformat_yaml))

  expect_condition(validate_config_movecols(wrongformat_yaml), NA)
  expect_null(validate_config_movecols(wrongformat_yaml))

  expect_condition(validate_config_datatype(wrongformat_yaml), NA)
  expect_length(validate_config_datatype(wrongformat_yaml), 4)
  expect_match(validate_config_datatype(wrongformat_yaml)[1], "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml)[2], "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml)[3], "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml)[4], "Data field(s) not in expected character or integer format", fixed = TRUE)

  expect_condition(internal_validate_config(wrongformat_data), NA)
  expect_invisible(internal_validate_config(wrongformat_data))
  expect_length(internal_validate_config(wrongformat_data), 4)
  expect_match(internal_validate_config(wrongformat_data)[1], "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[2], "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[3], "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[4], "Data field(s) not in expected character or integer format", fixed = TRUE)

  error_msg <- expect_error(validate_config(wrongformat_data))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(error_msg$message, "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(error_msg$message, "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(error_msg$message, "Data field(s) not in expected character or integer format", fixed = TRUE)

})


test_that("validate_config.R returns ERROR with informative message when mandatory movedata_cols keys are missing and some also not in expected format (char/int)",{

  missing_field_numeric <- "test_input_files/ScotEID_testnumericmissing.yml"
  missing_numeric_yaml <- yaml.load_file(missing_field_numeric)

  expect_condition(validate_config_moveopts(missing_numeric_yaml), NA)
  expect_null(validate_config_moveopts(missing_numeric_yaml))

  expect_condition(validate_config_movecols(missing_numeric_yaml), NA)
  expect_match(validate_config_movecols(missing_numeric_yaml), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_numeric_yaml), NA)
  expect_match(validate_config_datatype(missing_numeric_yaml), "Data field(s) not in expected character or integer format", fixed = TRUE)

  expect_condition(internal_validate_config(missing_field_numeric), NA)
  expect_invisible(internal_validate_config(missing_field_numeric))
  expect_length(internal_validate_config(missing_field_numeric), 2)
  expect_match(internal_validate_config(missing_field_numeric)[1], "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_field_numeric)[2], "Data field(s) not in expected character or integer format", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_field_numeric))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Data field(s) not in expected character or integer format", fixed = TRUE)

})
