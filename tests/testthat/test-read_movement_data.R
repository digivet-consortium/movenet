old_config<-movenetenv$options
suppressMessages(load_config("ScotEID"))
ScotEID_config <- yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))
ScotEID_movecols <- ScotEID_config$movedata_cols
ScotEID_colnames <- unlist(unname(ScotEID_movecols[c("origin_ID","dest_ID","move_date","nr_pigs","move_ID")]))

test_that("when config has all min cols only, and input has correct dateformat + int nr_pigs, output is data.frame with correct colnames & coltypes", {
  move_ID <- movenetenv$options$movedata_cols$move_ID
  movenetenv$options$movedata_cols$move_ID<-NULL
  output <- expect_condition(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$origin_ID]], "character")
  expect_type(output[[ScotEID_movecols$dest_ID]], "character")
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
  movenetenv$options$movedata_cols$move_ID <- move_ID
})

test_that("when config has min cols + extra move_ID col, and input has numeric move_ID (+ correct dateformat + int nr_pigs), output data.frame has extra column with correct name and type AFTER minimum cols", {
  output <- expect_warning(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_type(output[[ScotEID_movecols$move_ID]], "double")
  })

test_that("when config has all min cols, indicated as col indices, output is data.frame with correct colnames & coltypes",{
  suppressMessages(load_config("ScotEID_mincolnrs"))
  output <- expect_condition(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$origin_ID]], "character")
  expect_type(output[[ScotEID_movecols$dest_ID]], "character")
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has all min cols + extra col, with some of both indicated as col indices, output is data.frame with correct colnames & coltypes",{
  suppressMessages(load_config("ScotEID_mixcolnamesnrs"))
  output <- expect_warning(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,c(ScotEID_colnames,"foreign_reference"))
  expect_type(output[[ScotEID_movecols$origin_ID]], "character")
  expect_type(output[[ScotEID_movecols$dest_ID]], "character")
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
  expect_type(output[[ScotEID_movecols$move_ID]], "double")
  expect_type(output[["foreign_reference"]], "character")
  suppressMessages(load_config("ScotEID"))
})

#remove if dealt with at the change_config stage, otherwise add similar test/behaviour for non-char/non-int values
test_that("when config misses a min col, an error is raised", {
  origin_ID <- movenetenv$options$movedata_cols$origin_ID
  movenetenv$options$movedata_cols$origin_ID<-NULL
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"), "Unexpected config structure. Missing mandatory movedata_cols keys")
  movenetenv$options$movedata_cols$origin_ID <- origin_ID
})

test_that("when input datafile is missing, an error is raised", {
  expect_error(reformat_move_data(), 'argument "move_data_file" is missing, with no default')
})

test_that("when input datafile does not exist, an error is raised", {
  expect_error(reformat_move_data("foo.csv"), 'no such file exists')
})

test_that("when input datafile misses (a) mandatory col(s), an error is raised", {
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_colmissing.csv"), "Can't find the following mandatory columns in the datafile\\: departure_date")
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_2colsmissing.csv"), "Can't find the following mandatory columns in the datafile\\: dest_cph, departure_date")
})

test_that("when input datafile misses a requested optional col, a warning is raised and results are produced without the col", {
  expect_warning(reformat_move_data("test_input_files/ScotEID_testdata_optcolmissing.csv"), "Can't find the following requested optional columns in the datafile\\: movement_reference\\.\nProceeding without missing optional columns\\.")
  output <- reformat_move_data("test_input_files/ScotEID_testdata_optcolmissing.csv")
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$origin_ID]], "character")
  expect_type(output[[ScotEID_movecols$dest_ID]], "character")
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
})

test_that("when config has a min col, indicated as col index, that exceeds the col range of input datafile, an error is raised",{
  suppressMessages(load_config("ScotEID_mincolnrtoolarge"))
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"),
               "Can't find the following mandatory columns in the datafile\\: #20 \\(dest_ID\\)\\.\nThese column indices exceed the number of columns in the datafile\\.")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has an optional col, indicated as col index, that exceeds the col range of input datafile, a warning is raised and results are produced without the col",{
  suppressMessages(load_config("ScotEID_optcolnrtoolarge"))
  expect_warning(reformat_move_data("test_input_files/ScotEID_testdata.csv"),
                 "Can't find the following requested optional columns in the datafile\\: #20 \\(move_ID\\)\\.\nThese column indices exceed the number of columns in the datafile\\.\nProceeding without missing optional columns\\.")
  output <- reformat_move_data("test_input_files/ScotEID_testdata.csv")
  expect_s3_class(output,"data.frame")
  expect_named(output,c(ScotEID_colnames[c(1:4)],"foreign_reference"))
  expect_type(output[[ScotEID_movecols$origin_ID]], "character")
  expect_type(output[[ScotEID_movecols$dest_ID]], "character")
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
  expect_type(output[["foreign_reference"]], "character")
  suppressMessages(load_config("ScotEID"))
})

test_that("date is interpreted correctly, when config has a correct dateformat string, but some dates are missing (NAs)", {
  output <- expect_warning(reformat_move_data("test_input_files/ScotEID_testdata_NAdate.csv"), NA) #test lack of warning/error
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_NAdate.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_false(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests some dates are NA
})

test_that("date is interpreted correctly, when using implicit iso format date, that is correct for all data", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- ""
  output <- expect_warning(reformat_move_data("test_input_files/ScotEID_testdata_isodate.csv"), NA) #test lack of warning/error
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_isodate.csv"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[[ScotEID_movecols$move_date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$move_date]]))) #tests that all dates are interpretable = no NAs generated
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date column in datafile contains entries that can't be dates (no numbers)", {
  move_date <- movenetenv$options$movedata_cols$move_date
  movenetenv$options$movedata_cols$move_date<-"foreign_reference" #this column contains chr vectors w only letters
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"),
               "Can\\'t parse column `foreign_reference` as date\\.\nColumn `foreign_reference` does not contain any numbers\\.\nHave you identified the correct column name under the option `move_date`\\?")
  movenetenv$options$movedata_cols$move_date <- move_date
})

test_that("error is raised, when date format string is invalid (format does not match data)", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- "%d%m%Y"
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value `%d%m%Y`\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date is non-iso format but no date format string is provided", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- ""
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata.csv"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value ``\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date column in datafile contains one or more invalid dates (e.g. 30 Feb)", {
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_invaliddate.csv"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value `%Y%m%d`\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
})

test_that("no warning or error is raised, when 'number of pigs' column in datafile contains numbers with decimals", {
  output <- expect_warning(reformat_move_data("test_input_files/ScotEID_testdata_pigsdecimal.csv"), NA)
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_pigsdecimal.csv"), NA)
  expect_type(output[[ScotEID_movecols$nr_pigs]], "double")
})

test_that("error is raised, when 'number of pigs' column in datafile contains numbers with grouping marks", {
  expect_error(reformat_move_data("test_input_files/ScotEID_testdata_pigsgrouping.csv"), "Column `qty_pigs` must be numeric and can't contain a grouping mark")
})

movenetenv$options<-old_config
