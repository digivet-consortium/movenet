#locale doesn't influence sep
#read in .csv
#read in .tsv
#read in .csv2
#read in .csv with wrong file extension

#all required fields are present in file
# test more fields
# test some fields not there
# test duplicate field selection [probably want this to raise a warning]

#(expect number of pigs to be int)
#error message if number of pigs is not int (or not numeric, if preferred)
#correct copying of data with thousands separator
#locale doesn't influence thousands separator

test_that("datetime is interpreted correctly, when using iso format", {
  #set example data with iso dates, run function
  expect_s3_class(data$move_date, "POSIXct")
  })

test_that("datetime is interpreted correctly, when providing a format string", {
  #set example data with non-iso dates, run function with valid format string options
  expect_s3_class(data$move_date, "POSIXct")
})

test_that("warning/error is raised when datetime format string is invalid (not a recognisable datetime format string)", {
  #set example data, run function with invalid format string options
  #test
})

test_that("warning/error is raised when datetime format string is invalid (format does not match data)", {
  #"Elements that could not be parsed (or did not generate valid dates) will be set to NA, and a warning message will inform you of the total number of failures."
  #This could relate to erroneous data entry of one or more dates, or erroneous format entry
  #set example data, run function with invalid format string options
  #test
})
#should be the same as / similar to:
test_that("warning/error is raised when one or more invalid dates (e.g. 29 Feb)", {
  #"Elements that could not be parsed (or did not generate valid dates) will be set to NA, and a warning message will inform you of the total number of failures."
  #set example data with invalid date, run function
  #test
})
#Possibly the same as / similar to - though may want a different error message:
test_that("warning/error is raised when no datetime format is provided, but date is non-iso format", {
  #"Elements that could not be parsed (or did not generate valid dates) will be set to NA, and a warning message will inform you of the total number of failures."
  #set example data with only non-iso dates, run function without format string options
  #test
})


#char identifiers are read in as char
#numeric identifiers are read in as char

#warning if moveid not unique

#colnames are correct


