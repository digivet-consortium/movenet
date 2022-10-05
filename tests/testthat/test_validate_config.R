test_that("validate_config.R accepts movement config file with expected structure",{

  ScotEID_config <- system.file("configurations", paste0("ScotEID", ".yml"),package="movenet")
  ScotEID_yaml <- yaml.load_file(ScotEID_config)

  expect_type(validate_yaml(ScotEID_config),"list")
  expect_true(validate_yaml(ScotEID_config)$test)
  expect_null(validate_yaml(ScotEID_config)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_match(sub("data", "", regmatches(names(ScotEID_yaml),regexpr("(move|holding)data",names(ScotEID_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_root(ScotEID_yaml,"move"), NA)
  expect_null(validate_config_root(ScotEID_yaml,"move"))

  expect_condition(validate_config_fileopts(ScotEID_yaml,"move"), NA)
  expect_null(validate_config_fileopts(ScotEID_yaml,"move"))

  expect_condition(validate_config_cols(ScotEID_yaml,"move"), NA)
  expect_null(validate_config_cols(ScotEID_yaml,"move"))

  expect_condition(validate_config_datatype(ScotEID_yaml,"move"), NA)
  expect_null(validate_config_datatype(ScotEID_yaml,"move"))

  expect_condition(internal_validate_config(ScotEID_config), NA)
  expect_null(internal_validate_config(ScotEID_config))

  expect_condition(validate_config(ScotEID_config), NA)
  expect_true(validate_config(ScotEID_config))
  expect_invisible(validate_config(ScotEID_config))

})

test_that("validate_config.R accepts holding config file with expected structure",{

  ScotEID_config <- "test_input_files/fakeScotEID_holding.yml"
  ScotEID_yaml <- yaml.load_file(ScotEID_config)

  expect_type(validate_yaml(ScotEID_config),"list")
  expect_true(validate_yaml(ScotEID_config)$test)
  expect_null(validate_yaml(ScotEID_config)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_match(sub("data", "", regmatches(names(ScotEID_yaml),regexpr("(move|holding)data",names(ScotEID_yaml)))[1]), "holding", fixed = TRUE)

  expect_condition(validate_config_root(ScotEID_yaml,"holding"), NA)
  expect_null(validate_config_root(ScotEID_yaml,"holding"))

  expect_condition(validate_config_fileopts(ScotEID_yaml,"holding"), NA)
  expect_null(validate_config_fileopts(ScotEID_yaml,"holding"))

  expect_condition(validate_config_cols(ScotEID_yaml,"holding"), NA)
  expect_null(validate_config_cols(ScotEID_yaml,"holding"))

  expect_condition(validate_config_datatype(ScotEID_yaml,"holding"), NA)
  expect_null(validate_config_datatype(ScotEID_yaml,"holding"))

  expect_condition(internal_validate_config(ScotEID_config), NA)
  expect_null(internal_validate_config(ScotEID_config))

  expect_condition(validate_config(ScotEID_config), NA)
  expect_true(validate_config(ScotEID_config))
  expect_invisible(validate_config(ScotEID_config))

})

test_that("validate_config.R accepts config file with additional (top-level & movedata_cols) fields",{

  morefields_config <- "test_input_files/ScotEID_testmore.yml"
  morefields_yaml <- yaml.load_file(morefields_config)

  expect_match(sub("data", "", regmatches(names(morefields_yaml),regexpr("move|holding",names(morefields_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_root(morefields_yaml,"move"), NA)
  expect_null(validate_config_root(morefields_yaml,"move"))

  expect_condition(validate_config_fileopts(morefields_yaml,"move"), NA)
  expect_null(validate_config_fileopts(morefields_yaml,"move"))

  expect_condition(validate_config_cols(morefields_yaml,"move"), NA)
  expect_null(validate_config_cols(morefields_yaml,"move"))

  expect_condition(validate_config_datatype(morefields_yaml,"move"), NA)
  expect_null(validate_config_datatype(morefields_yaml,"move"))

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

  expect_equal(is.na(sub("data", "", regmatches(names(unrelated_yaml_read),regexpr("move|holding",names(unrelated_yaml_read)))[1])), TRUE)

  expect_condition(validate_config_root(unrelated_yaml_read, NA), NA)
  expect_match(validate_config_root(unrelated_yaml_read, NA), "Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

  expect_condition(validate_config_fileopts(unrelated_yaml_read, NA), NA)
  expect_null(validate_config_fileopts(unrelated_yaml_read, NA))

  expect_condition(validate_config_cols(unrelated_yaml_read, NA), NA)
  expect_null(validate_config_cols(unrelated_yaml_read, NA))

  expect_condition(validate_config_datatype(unrelated_yaml_read,NA), NA)
  expect_null(validate_config_datatype(unrelated_yaml_read, NA))

  expect_condition(internal_validate_config(unrelated_yaml), NA)
  expect_invisible(internal_validate_config(unrelated_yaml))
  expect_length(internal_validate_config(unrelated_yaml), 1)
  expect_match(internal_validate_config(unrelated_yaml), "Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

  error_msg <- expect_error(validate_config(unrelated_yaml))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when one of the mandatory top-level keys is missing",{

  missing_toplevel <- "test_input_files/ScotEID_testmissing2.yml"
  missing_toplevel_yaml <- yaml.load_file(missing_toplevel)

  expect_match(sub("data", "", regmatches(names(missing_toplevel_yaml),regexpr("move|holding",names(missing_toplevel_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_root(missing_toplevel_yaml,"move"), NA)
  expect_match(validate_config_root(missing_toplevel_yaml,"move"), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_fileopts(missing_toplevel_yaml,"move"), NA)
  expect_match(validate_config_fileopts(missing_toplevel_yaml,"move"), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_cols(missing_toplevel_yaml,"move"), NA)
  expect_null(validate_config_cols(missing_toplevel_yaml,"move"))

  expect_condition(validate_config_datatype(missing_toplevel_yaml,"move"), NA)
  expect_null(validate_config_datatype(missing_toplevel_yaml,"move"))

  expect_condition(internal_validate_config(missing_toplevel), NA)
  expect_invisible(internal_validate_config(missing_toplevel))
  expect_length(internal_validate_config(missing_toplevel), 2)
  expect_match(internal_validate_config(missing_toplevel)[1], "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(internal_validate_config(missing_toplevel)[2], "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  error_msg <- expect_error(validate_config(missing_toplevel))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory top-level key(s)", fixed = TRUE)
  expect_match(error_msg$message, "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when mandatory movedata_cols keys are missing",{

  missing_field <- "test_input_files/ScotEID_testmissing.yml"
  missing_field_yaml <- yaml.load_file(missing_field)

  expect_match(sub("data", "", regmatches(names(missing_field_yaml),regexpr("move|holding",names(missing_field_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_root(missing_field_yaml,"move"), NA)
  expect_null(validate_config_root(missing_field_yaml,"move"))

  expect_condition(validate_config_fileopts(missing_field_yaml,"move"), NA)
  expect_null(validate_config_fileopts(missing_field_yaml,"move"))

  expect_condition(validate_config_cols(missing_field_yaml,"move"), NA)
  expect_match(validate_config_cols(missing_field_yaml,"move"), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_field_yaml,"move"), NA)
  expect_null(validate_config_datatype(missing_field_yaml,"move"))

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

  expect_match(sub("data", "", regmatches(names(missing_both_yaml),regexpr("move|holding",names(missing_both_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_root(missing_both_yaml,"move"), NA)
  expect_match(validate_config_root(missing_both_yaml,"move"), "Missing mandatory top-level key(s)", fixed = TRUE)

  expect_condition(validate_config_fileopts(missing_both_yaml,"move"), NA)
  expect_match(validate_config_fileopts(missing_both_yaml,"move"), "Missing mandatory second-level (movedata_fileopts) key(s)", fixed = TRUE)

  expect_condition(validate_config_cols(missing_both_yaml,"move"), NA)
  expect_match(validate_config_cols(missing_both_yaml,"move"), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_both_yaml,"move"), NA)
  expect_null(validate_config_datatype(missing_both_yaml,"move"))

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

  expect_equal(is.na(sub("data", "", regmatches(names(added_lvl_yaml),regexpr("move|holding",names(added_lvl_yaml)))[1])), TRUE)

  expect_condition(validate_config_root(added_lvl_yaml, NA), NA)
  expect_match(validate_config_root(added_lvl_yaml, NA), "Unexpected config file structure. Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

  expect_condition(validate_config_fileopts(added_lvl_yaml, NA), NA)
  expect_null(validate_config_fileopts(added_lvl_yaml, NA))

  expect_condition(validate_config_cols(added_lvl_yaml, NA), NA)
  expect_null(validate_config_cols(added_lvl_yaml, NA))

  expect_condition(validate_config_datatype(added_lvl_yaml, NA), NA)
  expect_null(validate_config_datatype(added_lvl_yaml, NA))

  expect_condition(internal_validate_config(added_lvl), NA)
  expect_invisible(internal_validate_config(added_lvl))
  expect_length(internal_validate_config(added_lvl), 1)
  expect_match(internal_validate_config(added_lvl)[1], "Unexpected config file structure. Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

  error_msg <- expect_error(validate_config(added_lvl))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Unexpected config file structure. Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`", fixed = TRUE)

})

test_that("validate_config.R returns ERROR with informative message when faced with duplicate movedata_cols values",{

  dupl_cols <- "test_input_files/ScotEID_dupl.yml"
  dupl_cols_yaml <- yaml.load_file(dupl_cols)

  expect_match(sub("data", "", regmatches(names(dupl_cols_yaml),regexpr("move|holding",names(dupl_cols_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_cols(dupl_cols_yaml,"move"), NA)
  expect_match(validate_config_cols(dupl_cols_yaml,"move"), "Values for movedata_cols options must be unique.", fixed = TRUE)

  expect_condition(internal_validate_config(dupl_cols), NA)
  expect_length(internal_validate_config(dupl_cols), 1)
  expect_match(internal_validate_config(dupl_cols)[1], "Values for movedata_cols options must be unique.", fixed = TRUE)

  error_msg <- expect_error(validate_config(dupl_cols))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Values for movedata_cols options must be unique.", fixed = TRUE)

})

test_that("validate_config.R accepts config file with expected structure, with integer movedata_cols keys",{

  numeric_data <- "test_input_files/ScotEID_testint.yml"
  numeric_yaml <- yaml.load_file(numeric_data)

  expect_match(sub("data", "", regmatches(names(numeric_yaml),regexpr("move|holding",names(numeric_yaml)))[1]), "move", fixed = TRUE)

  expect_type(validate_yaml(numeric_data),"list")
  expect_true(validate_yaml(numeric_data)$test)
  expect_null(validate_yaml(numeric_data)$msg)
  #test expect_visible / invisible / silent for validate_yaml?

  expect_condition(validate_config_fileopts(numeric_yaml,"move"), NA)
  expect_null(validate_config_fileopts(numeric_yaml,"move"))

  expect_condition(validate_config_cols(numeric_yaml,"move"), NA)
  expect_null(validate_config_cols(numeric_yaml,"move"))

  expect_condition(validate_config_datatype(numeric_yaml,"move"), NA)
  expect_null(validate_config_datatype(numeric_yaml,"move"))

  expect_condition(internal_validate_config(numeric_data), NA)
  expect_null(internal_validate_config(numeric_data))

  expect_condition(validate_config(numeric_data), NA)
  expect_true(validate_config(numeric_data))
  expect_invisible(validate_config(numeric_data))

})

test_that("validate_config.R returns ERROR with informative messages when movement config has keys that are not in expected format",{

  wrongformat_data <- "test_input_files/ScotEID_testwrongformat.yml"
  wrongformat_yaml <- yaml.load_file(wrongformat_data)

  expect_match(sub("data", "", regmatches(names(wrongformat_yaml),regexpr("move|holding",names(wrongformat_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_fileopts(wrongformat_yaml,"move"), NA)
  expect_null(validate_config_fileopts(wrongformat_yaml,"move"))

  expect_condition(validate_config_cols(wrongformat_yaml,"move"), NA)
  expect_null(validate_config_cols(wrongformat_yaml,"move"))

  expect_condition(validate_config_datatype(wrongformat_yaml,"move"), NA)
  expect_length(validate_config_datatype(wrongformat_yaml,"move"), 4)
  expect_match(validate_config_datatype(wrongformat_yaml,"move")[1], "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"move")[2], "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"move")[3], "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"move")[4], "Data field(s) not in expected character or integer format", fixed = TRUE)

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

test_that("validate_config.R returns ERROR with informative messages when holding config has keys that are not in expected format",{

  wrongformat_data <- "test_input_files/fakeScotEID_holdingwrongformat.yml"
  wrongformat_yaml <- yaml.load_file(wrongformat_data)

  expect_match(sub("data", "", regmatches(names(wrongformat_yaml),regexpr("move|holding",names(wrongformat_yaml)))[1]), "holding", fixed = TRUE)

  expect_condition(validate_config_fileopts(wrongformat_yaml,"holding"), NA)
  expect_null(validate_config_fileopts(wrongformat_yaml,"holding"))

  expect_condition(validate_config_cols(wrongformat_yaml,"holding"), NA)
  expect_null(validate_config_cols(wrongformat_yaml,"holding"))

  expect_condition(validate_config_datatype(wrongformat_yaml,"holding"), NA)
  expect_length(validate_config_datatype(wrongformat_yaml,"holding"), 5)
  expect_match(validate_config_datatype(wrongformat_yaml,"holding")[1], "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"holding")[2], "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"holding")[3], "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"holding")[4], "Data field `coord_EPSG_code` not in expected integer format", fixed = TRUE)
  expect_match(validate_config_datatype(wrongformat_yaml,"holding")[5], "Data field(s) not in expected character or integer format", fixed = TRUE)

  expect_condition(internal_validate_config(wrongformat_data), NA)
  expect_invisible(internal_validate_config(wrongformat_data))
  expect_length(internal_validate_config(wrongformat_data), 5)
  expect_match(internal_validate_config(wrongformat_data)[1], "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[2], "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[3], "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[4], "Data field `coord_EPSG_code` not in expected integer format", fixed = TRUE)
  expect_match(internal_validate_config(wrongformat_data)[5], "Data field(s) not in expected character or integer format", fixed = TRUE)

  error_msg <- expect_error(validate_config(wrongformat_data))
  expect_match(error_msg$message, "is not a valid movenet config file", fixed = TRUE)
  expect_match(error_msg$message, "Data field(s) not in expected character format", fixed = TRUE)
  expect_match(error_msg$message, "Data field `decimal` doesn't have the expected format of a single character", fixed = TRUE)
  expect_match(error_msg$message, "Data field `date_format` doesn't match readr date format specifications", fixed = TRUE)
  expect_match(error_msg$message, "Data field `coord_EPSG_code` not in expected integer format", fixed = TRUE)
  expect_match(error_msg$message, "Data field(s) not in expected character or integer format", fixed = TRUE)

})


test_that("validate_config.R returns ERROR with informative message when mandatory movedata_cols keys are missing and some also not in expected format (char/int)",{

  missing_field_numeric <- "test_input_files/ScotEID_testnumericmissing.yml"
  missing_numeric_yaml <- yaml.load_file(missing_field_numeric)

  expect_match(sub("data", "", regmatches(names(missing_numeric_yaml),regexpr("move|holding",names(missing_numeric_yaml)))[1]), "move", fixed = TRUE)

  expect_condition(validate_config_fileopts(missing_numeric_yaml,"move"), NA)
  expect_null(validate_config_fileopts(missing_numeric_yaml,"move"))

  expect_condition(validate_config_cols(missing_numeric_yaml,"move"), NA)
  expect_match(validate_config_cols(missing_numeric_yaml,"move"), "Missing mandatory second-level (movedata_cols) key(s)", fixed = TRUE)

  expect_condition(validate_config_datatype(missing_numeric_yaml,"move"), NA)
  expect_match(validate_config_datatype(missing_numeric_yaml,"move"), "Data field(s) not in expected character or integer format", fixed = TRUE)

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
