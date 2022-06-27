ScotEID_datafile <- "test_input_files/ScotEID_testdata.csv"
old_config<-movenetenv$options
suppressMessages(load_config("ScotEID"))
ScotEID_config <- yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))
ScotEID_movedata <- ScotEID_config$movement_data
ScotEID_colnames <- unlist(unname(ScotEID_movedata[c("movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs","movenet.move_ID")]))


test_that("when config has all min cols only, and input has implicit iso dateformat + int nr_pigs, output is data.frame with correct colnames & coltypes", {
  move_ID <- movenetenv$options$movement_data$movenet.move_ID
  movenetenv$options$movement_data$movenet.move_ID<-NULL
  output <- expect_condition(reformat_move_data(ScotEID_datafile), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[ScotEID_movedata$movenet.origin_ID], "character")
  expect_type(output[ScotEID_movedata$movenet.dest_ID], "character")
  expect_s3_class(output[ScotEID_movedata$movenet.move_date], "POSIXct") #this also works for wrong datetime formats though
  expect_true(all(!is.NA(output[ScotEID_movedata$movenet.move_date]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[ScotEID_movedata$movenet.nr_pigs], "integer")
  movenetenv$options$movement_data$movenet.move_ID <- move_ID
})

test_that("when config has min cols + extra move_ID col, and input has numeric move_ID (+ implicit iso dateformat + int nr_pigs), output data.frame has extra column with correct name and type AFTER minimum cols", {
  output <- expect_condition(reformat_move_data(ScotEID_datafile), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_type(output[ScotEID_movedata$movenet.move_ID], "double")
  })

#remove if dealt with at the change_config stage, otherwise add similar test/behaviour for non-char/non-int values
test_that("when config misses a min col, an error is raised", {
  origin_ID <- movenetenv$options$movement_data$movenet.origin_ID
  movenetenv$options$movement_data$movenet.origin_ID<-NULL
  expect_error(reformat_move_data(ScotEID_datafile), "Unexpected config structure. Missing mandatory movement_data keys")
  movenetenv$options$movement_data$movenet.origin_ID <- origin_ID
})

test_that("When input datafile is missing, an error is raised", {
  expect_error(reformat_move_data(), 'argument "move_data_file" is missing, with no default')
})

test_that("When input datafile does not exist, an error is raised", {
  expect_error(reformat_move_data(), 'no such file exists')
})

#WIP!! Requires catching error from read_delim
test_that("when input datafile misses a min col, an error is raised", {
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_colmissing.csv"), "")
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_2colsmissing.csv"), "")
})

#WIP!! Requires catching error from read_delim
test_that("when input datafile misses a requested extra col, a warning is raised", {
})

test_that("datetime is interpreted correctly, when input has implicit iso format, but some dates are missing (NAs)", {
  output <- expect_condition(reformat_move_data("test_input_files/ScotEID_testdata_NAdate.csv"), NA) #test lack of warning/error
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[ScotEID_movedata$movenet.move_date], "POSIXct") #this also works for wrong datetime formats though
  expect_false(all(!is.NA(output[ScotEID_movedata$movenet.move_date]))) #tests some dates are NA
})

test_that("datetime is interpreted correctly, when providing a format string that is correct for all data", {
  output <- expect_condition(reformat_move_data("test_input_files/ScotEID_testdata_dmY.csv",datetime_format = "%d%m%Y"), NA) #test lack of warning/error
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[ScotEID_movedata$movenet.move_date], "POSIXct") #this also works for wrong datetime formats though
  expect_true(all(!is.NA(output[ScotEID_movedata$movenet.move_date]))) #tests that all dates are interpretable = no NAs generated
})

#WIP!! Requires catching message or warning from read_delim (I think it's a vroom warning, resulting in a readr message)
test_that("warning/error is raised when datetime format string is invalid (not a recognisable datetime format string)", {
  #This results in NAs + "Warning message:  One or more parsing issues, see `problems()` for details"
  #But running "problems()" then doesn't give any details , not even if I store the dataframe in var + use this as arg, or do this within the function environment. Bug in readr?
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_dmY.csv",datetime_format = "foo"), "")
})

#WIP!! Requires catching message or warning from read_delim (I think it's a vroom warning, resulting in a readr message)
test_that("warning/error is raised when datetime format string is invalid (format does not match data)", {
  #This results in NAs + "Warning message:  One or more parsing issues, see `problems()` for details"
  #But running "problems()" then doesn't give any details , not even if I store the dataframe in var + use this as arg, or do this within the function environment. Bug in readr?
  output <- reformat_move_data("test_input_files/ScotEID_testdata_dmY.csv",datetime_format = "%m%d%Y")
  expect_warning(reformat_move_data("test_input_files/ScotEID_testdata_dmY.csv",datetime_format = "%m%d%Y"),"")
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[ScotEID_movedata$movenet.move_date], "POSIXct") #this also works for wrong datetime formats
  expect_false(all(!is.NA(output[ScotEID_movedata$movenet.move_date]))) #tests that at least some dates were uninterpretable, resulting in NAs
})
#currently the same as - though may want a different error message:
test_that("warning/error is raised when one or more invalid dates (e.g. 29 Feb , or even 'foo')", {
  #"Elements that could not be parsed (or did not generate valid dates) will be set to NA, and a warning message will inform you of the total number of failures."
  #set example data with invalid date, run function
  #test
})
#currently the same as - though may want a different error message:
test_that("warning/error is raised when no datetime format is provided, but date is non-iso format", {
  #"Elements that could not be parsed (or did not generate valid dates) will be set to NA, and a warning message will inform you of the total number of failures."
  #set example data with only non-iso dates, run function without format string options
  #test
})

#WIP!! Requires catching message or warning from read_delim (I think it's a vroom warning, resulting in a readr message)
test_that("warning/error is raised [or what?] when 'number of pigs' contains numbers with decimals", {
  #This results in NAs + "Warning message:  One or more parsing issues, see `problems()` for details"
  #But running "problems()" then doesn't give any details , not even if I store the dataframe in var + use this as arg, or do this within the function environment. Bug in readr?
  output <- reformat_move_data("test_input_files/ScotEID_testdata_pigsdecimal.csv")
})

#WIP!! Requires catching message or warning from read_delim (I think it's a vroom warning, resulting in a readr message)
test_that("warning/error is raised [or what?] when 'number of pigs' contains numbers with grouping marks", {
  #This results in NAs + "Warning message:  One or more parsing issues, see `problems()` for details"
  #But running "problems()" then doesn't give any details , not even if I store the dataframe in var + use this as arg, or do this within the function environment. Bug in readr?
  output <- reformat_move_data("test_input_files/ScotEID_testdata_pigsgrouping.csv")
})

movenetenv$options<-old_config
