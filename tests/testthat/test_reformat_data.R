old_config<-movenetenv$options
suppressMessages(load_config("ScotEID"))
suppressMessages(load_config("fakeScotEID_holding.yml"))
ScotEID_config <- yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))
ScotEID_movecols <- ScotEID_config$movedata_cols
ScotEID_colnames <- unlist(unname(ScotEID_movecols[c("from","to","date","weight")]))

test_that("when argument `type` (datatype) is missing, an error is raised with an informative message", {
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv"), 'argument "type" is missing, with no default')
})

test_that("when argument `type` (datatype) is an unexpected value, an error is raised with an informative message", {
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","foo"), "Must be element of set \\{'movement','holding'\\}, but is 'foo'")
})

test_that("when movement config has all min cols only, and input is file & has correct dateformat + int weight, output is data.frame with correct colnames & coltypes", {
  output <- expect_condition(reformat_data("test_input_files/ScotEID_testdata.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
})

test_that("when movement config has all min cols only, and input is df & has correct dateformat + int weight, output is data.frame with correct colnames & coltypes", {
  data <- suppressMessages(read_csv("test_input_files/ScotEID_testdata.csv"))
  output <- expect_condition(reformat_data(data,"movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
})

test_that("when movement config has min cols + extra col, output data.frame has extra column with correct name and type AFTER minimum cols", {
  movenetenv$options$movedata_cols$move_ID <- "movement_reference"
  output <- expect_warning(reformat_data("test_input_files/ScotEID_testdata.csv","movement"), NA)
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,c(ScotEID_colnames, movenetenv$options$movedata_cols$move_ID))
  expect_type(output[[movenetenv$options$movedata_cols$move_ID]], "double")
  suppressMessages(load_config("ScotEID"))
})

test_that("when holding config has required & opt col, in correct data formats, output is data.frame with correct colnames & coltypes", {
  output <- expect_error(reformat_data("test_input_files/test_holdingdata_generic.csv","holding"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,c("cph","holding_type","herd_size","coordinates"))
  expect_type(output[["cph"]], "character")
  expect_class(output[["coordinates"]], c("sfc_POINT","sfc"))
  expect_type(output[["holding_type"]], "character")
  expect_type(output[["herd_size"]], "double")
})

test_that("when config has all min cols, indicated as col indices, output is data.frame with correct colnames & coltypes, a warning is raised, and configs are changed",{
  suppressMessages(load_config("test_input_files/ScotEID_mincolnrs.yml"))
  expect_warning(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
                 "The following options have been changed from column indices to column headers within the loaded configurations:\n - from: #14 -> 'departure_cph'\n - to: #19 -> 'dest_cph'\n - date: #5 -> 'departure_date'\n - weight: #7 -> 'qty_pigs'", fixed=TRUE)
  expect_mapequal(get_config("movedata_cols.from","movedata_cols.to","movedata_cols.date","movedata_cols.weight"),
                  list(movedata_cols.from = 'departure_cph', movedata_cols.to = 'dest_cph',
                       movedata_cols.date = 'departure_date', movedata_cols.weight = 'qty_pigs'))
  output <- expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has all min cols + extra col, with some of both indicated as col indices, output is data.frame with correct colnames & coltypes, a warning is raised, and configs are changed",{
  suppressMessages(load_config("test_input_files/ScotEID_mixcolnamesnrs.yml"))
  expect_warning(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
                 "The following options have been changed from column indices to column headers within the loaded configurations:\n - to: #19 -> 'dest_cph'\n - date: #5 -> 'departure_date'\n - weight: #7 -> 'qty_pigs'\n - move_ID: #1 -> 'movement_reference'", fixed=TRUE)
  expect_mapequal(get_config("movedata_cols.to","movedata_cols.date","movedata_cols.weight","movedata_cols.move_ID"),
                  list(movedata_cols.to = 'dest_cph', movedata_cols.date = 'departure_date', movedata_cols.weight = 'qty_pigs', movedata_cols.move_ID = 'movement_reference'))
  output <- expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,c(ScotEID_colnames,"movement_reference","foreign_reference"))
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
  expect_type(output[["movement_reference"]], "double")
  expect_type(output[["foreign_reference"]], "character")
  suppressMessages(load_config("ScotEID"))
})

test_that("when input data is missing, an error is raised", {
  expect_error(reformat_data(), 'argument "data" is missing, with no default')
})

test_that("when input datafile does not exist, an error is raised", {
  expect_error(reformat_data("foo.csv","movement"), 'an existing delimited data file with read', fixed = TRUE)
})

test_that("when input datafile misses (a) mandatory col(s), an error is raised", {
  expect_error(reformat_data("test_input_files/ScotEID_testdata_colmissing.csv","movement"), "Can't find the following mandatory columns in the data\\: departure_date")
  expect_error(reformat_data("test_input_files/ScotEID_testdata_2colsmissing.csv","movement"), "Can't find the following mandatory columns in the data\\: dest_cph, departure_date")
})

test_that("when input datafile misses a requested optional col, a warning is raised and results are produced without the col", {
  movenetenv$options$movedata_cols$move_ID <- "movement_reference"
  expect_warning(reformat_data("test_input_files/ScotEID_testdata_optcolmissing.csv","movement"), "Can't find the following requested optional columns in the data: movement_reference.\nProceeding without missing optional columns.", fixed = TRUE)
  output <- suppressWarnings(reformat_data("test_input_files/ScotEID_testdata_optcolmissing.csv","movement"))
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames[c(1:4)])
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has a min col, indicated as col index, that exceeds the col range of input datafile, an error is raised",{
  suppressMessages(load_config("test_input_files/ScotEID_mincolnrtoolarge.yml"))
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
               "Can't find the following mandatory columns in the data\\: #20 \\(to\\)\\.\nThese column indices exceed the number of columns in the data\\.")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has an optional col, indicated as col index, that exceeds the col range of input datafile, a warning is raised and results are produced without the col",{
  suppressMessages(load_config("test_input_files/ScotEID_optcolnrtoolarge.yml"))
  suppressWarnings(expect_warning(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
                 "Can't find the following requested optional columns in the data\\: #20 \\(move_ID\\)\\.\nThese column indices exceed the number of columns in the data\\.\nProceeding without missing optional columns\\."))
  suppressWarnings(expect_warning(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
                 "The following options have been changed from column indices to column headers within the loaded configurations", fixed=TRUE))
  output <- suppressWarnings(reformat_data("test_input_files/ScotEID_testdata.csv","movement"))
  expect_s3_class(output,"data.frame")
  expect_named(output,c(ScotEID_colnames[c(1:4)],"foreign_reference"))
  expect_type(output[[ScotEID_movecols$from]], "character")
  expect_type(output[[ScotEID_movecols$to]], "character")
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  expect_type(output[[ScotEID_movecols$weight]], "double")
  expect_type(output[["foreign_reference"]], "character")
  suppressMessages(load_config("ScotEID"))
})

test_that("when config has a movedata_cols option indicated as col index, but that upon translation to colname reveals to be a duplicate col, an error is raised",{
  suppressMessages(change_config(movedata_cols.from=19L))
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
               "Values for movedata_cols/holdingdata_cols options must be unique\\. Translation of column indices to column headers identified the following options with duplicate values\\: from, to")
  suppressMessages(load_config("ScotEID"))
})

test_that("date is interpreted correctly, when config has a correct dateformat string, but some dates are missing (NAs)", {
  output <- expect_warning(reformat_data("test_input_files/ScotEID_testdata_NAdate.csv","movement"), NA) #test lack of warning/error
  expect_error(reformat_data("test_input_files/ScotEID_testdata_NAdate.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_false(all(!is.na(output[[ScotEID_movecols$date]]))) #tests some dates are NA
})

test_that("date is interpreted correctly, when using implicit '%AD' (flexible YMD) format, that is correct for all data", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- ""
  output <- expect_warning(reformat_data("test_input_files/ScotEID_testdata_isodate.csv","movement"), NA) #test lack of warning/error
  expect_error(reformat_data("test_input_files/ScotEID_testdata_isodate.csv","movement"), NA)
  expect_s3_class(output,"data.frame")
  expect_named(output,ScotEID_colnames)
  expect_s3_class(output[[ScotEID_movecols$date]], "Date") #this also works for wrong date formats though
  expect_true(all(!is.na(output[[ScotEID_movecols$date]]))) #tests that all dates are interpretable = no NAs generated
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date column in datafile contains entries that can't be dates (no numbers)", {
  date <- movenetenv$options$movedata_cols$date
  movenetenv$options$movedata_cols$date<-"foreign_reference" #this column contains chr vectors w only letters
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
               "Can\\'t parse column `foreign_reference` as date\\.\nColumn `foreign_reference` does not contain any numbers\\.\nHave you identified the correct column name under the option `date`\\?")
  movenetenv$options$movedata_cols$date <- date
})

test_that("error is raised, when date format string is invalid (format does not match data)", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- "%d%m%Y"
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value `%d%m%Y`\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date is not YMD and no date format string is provided", {
  old_dateformat <- movenetenv$options$movedata_fileopts$date_format
  movenetenv$options$movedata_fileopts$date_format <- ""
  expect_error(reformat_data("test_input_files/ScotEID_testdata.csv","movement"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value ``\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
  movenetenv$options$movedata_fileopts$date_format <- old_dateformat
})

test_that("error is raised, when date column in datafile contains one or more invalid dates (e.g. 30 Feb)", {
  expect_error(reformat_data("test_input_files/ScotEID_testdata_invaliddate.csv","movement"),
               "Can\\'t parse column `departure_date` as date\\.\nThe date format specification given through the option `date_format` \\(value `%Y%m%d`\\) and the actual format of column `departure_date` don't appear to match\\.\nAlternatively, column `departure_date` contains one or more invalid dates\\.")
})

test_that("no warning or error is raised, when 'weight' column in datafile contains numbers with decimals", {
  output <- expect_warning(reformat_data("test_input_files/ScotEID_testdata_pigsdecimal.csv","movement"), NA)
  expect_error(reformat_data("test_input_files/ScotEID_testdata_pigsdecimal.csv","movement"), NA)
  expect_type(output[[ScotEID_movecols$weight]], "double")
})

test_that("error is raised, when 'weight' column in datafile contains numbers with grouping marks", {
  expect_error(reformat_data("test_input_files/ScotEID_testdata_pigsgrouping.csv","movement"), "Column `qty_pigs` must be numeric and can't contain a grouping mark")
})

test_that("error is raised, when 'coord_x' column in datafile contains characters", {
  expect_error(reformat_data("test_input_files/test_holdingdata_notnumeric.csv","holding"), "Column `easting` must be numeric and can't contain a grouping mark")
})

movenetenv$options<-old_config
