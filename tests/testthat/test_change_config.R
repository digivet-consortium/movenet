test_that("load_config() loads options from existing, valid config file w quotes, if given as name of preinstalled config, without file extension",{
  expect_message(load_config("Denmark"), "Successfully loaded config file:")
  load_config("Denmark")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_fileopts)
})

test_that("load_config() loads options from existing, valid config file w quotes, if given as name of preinstalled config, with file extension",{
  expect_message(load_config("Denmark.yml"), "Successfully loaded config file:")
  load_config("Denmark.yml")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_fileopts)
})

test_that("load_config() loads options from existing, valid config file, if given as path",{
  expect_message(load_config("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/movenet/tests/testthat/test_input_files/ScotEID_testmore.yml"), "Successfully loaded config file:")
  load_config("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/movenet/tests/testthat/test_input_files/ScotEID_testmore.yml")
  expect_mapequal(movenetenv$options, yaml.load_file("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/movenet/tests/testthat/test_input_files/ScotEID_testmore.yml"))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/movenet/tests/testthat/test_input_files/ScotEID_testmore.yml")$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/movenet/tests/testthat/test_input_files/ScotEID_testmore.yml")$movedata_fileopts)
})

test_that("load_config() raises error when called without argument",{
  expect_error(load_config(),"Argument `configfile` is missing\\. Please provide either the name of a preinstalled config file, or the path of the config file you wish to load\\.")
})

test_that("load_config() raises error when faced with invalid config file (config file with unquoted special chars)",{
  expect_error(load_config("test_input_files/ScotEID_noquotationmarks.yml"),"is not a valid movenet config file")
  expect_error(load_config("test_input_files/ScotEID_noquotationmarks.yml"),"is not valid yaml format")
})

test_that("load_config() loads options from existing, valid config file with unquoted movedata_cols options",{
  expect_message(load_config("test_input_files/ScotEID_noquotationmarks2.yml"),"Successfully loaded config file:")
  load_config("test_input_files/ScotEID_noquotationmarks2.yml")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))$movedata_fileopts)
})

test_that("load_config() raises error when requested config file does not exist",{
  expect_error(load_config("doesnotexist"), "Specified config file not found:")
})
#Also when path, or filename with extension, is given.

local_save_config <- function(outfile){
  #this helper function runs save_config and then deletes the resulting file when the parent env is left
  save_config(outfile)
  withr::defer_parent(unlink(outfile))
}

local({
  local_test_context()
  test_that("save_config() correctly saves file containing current movenetenv options",{
    expect_message(local_save_config("test_save_config.yml"), "Successfully saved configurations to:")
    local_save_config("test_save_config.yml")
    expect_mapequal(movenetenv$options, yaml.load_file("test_save_config.yml"))
    expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file("test_save_config.yml")$movedata_cols)
    expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file("test_save_config.yml")$movedata_fileopts)
  })
})
local({
  local_test_context()
  test_that("save_config() gives an error when outfile is an empty string",{
    expect_error(local_save_config(""), "is not a valid value for `outfile`\\. Please provide a path to which to save the config file to\\.")
    })
})
local({
  local_test_context()
  test_that("save_config() raises error when called without argument",{
    expect_error(local_save_config(),"Argument `outfile` is missing\\. Please provide a path to which to save the config file to\\.")
  })
})


#Any edge / error cases to test for save_config? (no writing permission?)

local_new_config <- function(){
  #this helper function runs new_config and then deletes the resulting file when the parent env is left
  new_config()
  withr::defer_parent(unlink("template.yml"))
}

local({
  local_test_context()
  test_that("new_config() correctly copies template to working directory",{
    expect_message(local_new_config(), "Successfully saved config template")
    local_new_config()
    expect_mapequal(yaml.load_file("template.yml"), yaml.load_file(system.file("configurations", "template.yml", package="movenet")))
    expect_mapequal(yaml.load_file("template.yml")$movedata_cols, yaml.load_file(system.file("configurations", "template.yml", package="movenet"))$movedata_cols)
    expect_mapequal(yaml.load_file("template.yml")$movedata_fileopts, yaml.load_file(system.file("configurations", "template.yml", package="movenet"))$movedata_fileopts)
    #expect_mapequal seems a bit excessive here, I do not need to test copy(), but I don't really know how best to test
  })
})

#Any edge / error cases to test for new_config? (no writing permission? shouldn't happen in wd?)

test_that("get_config() returns value of a single option, if this is requested by its full name",{
  expect_vector(get_config("from"), ptype=list(), size =1)
  expect_named(get_config("from"), c("from"))
  expect_mapequal(get_config("from"), movenetenv$options$movedata_cols["from"])
})
test_that("get_config() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(get_config("fro"), ptype=list(), size =1)
  expect_named(get_config("fro"), c("from"))
  expect_mapequal(get_config("fro"), movenetenv$options$movedata_cols["from"])
})
test_that("get_config() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(get_config("from", "weight"), ptype=list(), size = 2)
  expect_named(get_config("from", "weight"), c("from", "weight"))
  expect_mapequal(get_config("from", "weight"), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(get_config(c("from", "weight")), ptype=list(), size = 2)
  expect_named(get_config(c("from", "weight")), c("from", "weight"))
  expect_mapequal(get_config(c("from", "weight")), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single list",{
  expect_vector(get_config(list("from", "weight")), ptype=list(), size = 2)
  expect_named(get_config(list("from", "weight")), c("from", "weight"))
  expect_mapequal(get_config(list("from", "weight")), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as separate character strings",{
  expect_vector(get_config("fro", "weigh"), ptype=list(), size = 2)
  expect_named(get_config("fro", "weigh"), c("from", "weight"))
  expect_mapequal(get_config("fro", "weigh"), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single character vector",{
  expect_vector(get_config(c("fro", "weigh")), ptype=list(), size = 2)
  expect_named(get_config(c("fro", "weigh")), c("from", "weight"))
  expect_mapequal(get_config(c("fro", "weigh")), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single list",{
  expect_vector(get_config(list("fro", "weigh")), ptype=list(), size = 2)
  expect_named(get_config(list("fro", "weigh")), c("from", "weight"))
  expect_mapequal(get_config(list("fro", "weigh")), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is unmatched",{
  expect_warning(get_config("froX"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("froX"), ptype=list(), size =0)
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is ambiguous",{
  expect_warning(get_config(""), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(""), ptype=list(), size =0)
})
test_that("get_config() returns the value of recognised options and a warning, if these are requested as separate character strings and include an unmatched option",{
  expect_warning(get_config("froX", "weight"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("froX", "weight"), ptype=list(), size = 1)
  expect_named(get_config("froX", "weight"), c("weight"))
  expect_mapequal(get_config("froX", "weight"), movenetenv$options$movedata_cols["weight"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as single char vector, incl an unmatched option",{
  expect_warning(get_config(c("froX", "weight")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(c("froX", "weight")), ptype=list(), size = 1)
  expect_named(get_config(c("froX", "weight")), c("weight"))
  expect_mapequal(get_config(c("froX", "weight")), movenetenv$options$movedata_cols["weight"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as list, and incl an unmatched option",{
  expect_warning(get_config(list("froX", "weight")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(list("froX", "weight")), ptype=list(), size = 1)
  expect_named(get_config(list("froX", "weight")), c("weight"))
  expect_mapequal(get_config(list("froX", "weight")), movenetenv$options$movedata_cols["weight"])
})
test_that("get_config() returns all options, if no argument is given",{
  expect_vector(get_config(), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(get_config(), flatten(movenetenv$options))
})
test_that("get_config() returns all options, if an empty list is given",{
  expect_vector(get_config(list()), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(get_config(list()), flatten(movenetenv$options))
})


test_that("change_config() returns value of a single option, if this is requested by its full name",{
  #This results in nothing. What do I want output for this to be?
  expect_vector(change_config("from"), ptype=list(), size =1)
  expect_named(change_config("from"), c("from"))
  expect_mapequal(change_config("from"), movenetenv$options$movedata_cols["from"])
})
test_that("change_config() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(change_config("fro"), ptype=list(), size =1)
  expect_named(change_config("fro"), c("from"))
  expect_mapequal(change_config("fro"), movenetenv$options$movedata_cols["from"])
})
test_that("change_config() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(change_config("from", "weight"), ptype=list(), size = 2)
  expect_named(change_config("from", "weight"), c("from", "weight"))
  expect_mapequal(change_config("from", "weight"), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("change_config() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(change_config(c("from", "weight")), ptype=list(), size = 2)
  expect_named(change_config(c("from", "weight")), c("from", "weight"))
  expect_mapequal(change_config(c("from", "weight")), movenetenv$options$movedata_cols[c("from","weight")])
})
test_that("change_config() returns all options, if no argument is given",{
  expect_vector(change_config(), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(change_config(), flatten(movenetenv$options))
})
test_that("change_config() returns all options, if an empty list is given",{
  expect_vector(change_config(list()), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(change_config(list()), flatten(movenetenv$options))
})


