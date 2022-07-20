test_that("load_config() loads options from existing, valid config file w quotes",{
  expect_message(load_config("Denmark"), "Successfully loaded config file:")
  load_config("Denmark")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movedata_fileopts)
})

test_that("load_config() loads options from existing, valid config file wo quotes",{
  #This does not work. Any special characters like . or , or % need to be part of quoted string.
  #Caught by validate_config as "not valid yaml format" but could be more informative...
  expect_message(load_config("ScotEID_noquotationmarks"),"Successfully loaded config file:")
  load_config("ScotEID_noquotationmarks")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))$movedata_cols)
  expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))$movedata_fileopts)
})

test_that("load_config() raises error when requested config file does not exist",{
  expect_error(load_config("doesnotexist"), "Specified config file not found:")
})

# ? want error if not valid config file ?
# If so need to call validate_config() from load_config() -> expect_error

local_save_config <- function(configname){
  #this helper function runs save_config and then deletes the resulting file when the parent env is left
  save_config(configname)
  withr::defer_parent(unlink(paste0(system.file("configurations", package = "movenet"),"/",configname,".yml")))
}

local({
  local_test_context()
  test_that("save_config() correctly saves file containing current movenetenv options",{
    expect_message(local_save_config("test_save_config"), "Successfully saved config file:")
    local_save_config("test_save_config")
    expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "test_save_config.yml", package="movenet")))
    expect_mapequal(movenetenv$options$movedata_cols, yaml.load_file(system.file("configurations", "test_save_config.yml", package="movenet"))$movedata_cols)
    expect_mapequal(movenetenv$options$movedata_fileopts, yaml.load_file(system.file("configurations", "test_save_config.yml", package="movenet"))$movedata_fileopts)
  })
})
local({
  local_test_context()
  test_that("save_config() gives an error when configname is an empty string",{
    expect_error(local_save_config(""), "is not a valid configname")
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
  expect_vector(get_config("origin_ID"), ptype=list(), size =1)
  expect_named(get_config("origin_ID"), c("origin_ID"))
  expect_mapequal(get_config("origin_ID"), movenetenv$options$movedata_cols["origin_ID"])
})
test_that("get_config() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(get_config("origin_I"), ptype=list(), size =1)
  expect_named(get_config("origin_I"), c("origin_ID"))
  expect_mapequal(get_config("origin_I"), movenetenv$options$movedata_cols["origin_ID"])
})
test_that("get_config() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(get_config("origin_ID", "nr_pigs"), ptype=list(), size = 2)
  expect_named(get_config("origin_ID", "nr_pigs"), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config("origin_ID", "nr_pigs"), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(get_config(c("origin_ID", "nr_pigs")), ptype=list(), size = 2)
  expect_named(get_config(c("origin_ID", "nr_pigs")), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config(c("origin_ID", "nr_pigs")), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single list",{
  expect_vector(get_config(list("origin_ID", "nr_pigs")), ptype=list(), size = 2)
  expect_named(get_config(list("origin_ID", "nr_pigs")), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config(list("origin_ID", "nr_pigs")), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as separate character strings",{
  expect_vector(get_config("origin_I", "nr_pig"), ptype=list(), size = 2)
  expect_named(get_config("origin_I", "nr_pig"), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config("origin_I", "nr_pig"), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single character vector",{
  expect_vector(get_config(c("origin_I", "nr_pig")), ptype=list(), size = 2)
  expect_named(get_config(c("origin_I", "nr_pig")), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config(c("origin_I", "nr_pig")), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single list",{
  expect_vector(get_config(list("origin_I", "nr_pig")), ptype=list(), size = 2)
  expect_named(get_config(list("origin_I", "nr_pig")), c("origin_ID", "nr_pigs"))
  expect_mapequal(get_config(list("origin_I", "nr_pig")), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is unmatched",{
  expect_warning(get_config("origin_IX"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("origin_IX"), ptype=list(), size =0)
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is ambiguous",{
  expect_warning(get_config(""), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(""), ptype=list(), size =0)
})
test_that("get_config() returns the value of recognised options and a warning, if these are requested as separate character strings and include an unmatched option",{
  expect_warning(get_config("origin_IX", "nr_pigs"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("origin_IX", "nr_pigs"), ptype=list(), size = 1)
  expect_named(get_config("origin_IX", "nr_pigs"), c("nr_pigs"))
  expect_mapequal(get_config("origin_IX", "nr_pigs"), movenetenv$options$movedata_cols["nr_pigs"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as single char vector, incl an unmatched option",{
  expect_warning(get_config(c("origin_IX", "nr_pigs")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(c("origin_IX", "nr_pigs")), ptype=list(), size = 1)
  expect_named(get_config(c("origin_IX", "nr_pigs")), c("nr_pigs"))
  expect_mapequal(get_config(c("origin_IX", "nr_pigs")), movenetenv$options$movedata_cols["nr_pigs"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as list, and incl an unmatched option",{
  expect_warning(get_config(list("origin_IX", "nr_pigs")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(list("origin_IX", "nr_pigs")), ptype=list(), size = 1)
  expect_named(get_config(list("origin_IX", "nr_pigs")), c("nr_pigs"))
  expect_mapequal(get_config(list("origin_IX", "nr_pigs")), movenetenv$options$movedata_cols["nr_pigs"])
})
test_that("get_config() returns all options, if no argument is given",{
  expect_vector(get_config(), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(get_config(), flatten(movenetenv$options))
})
test_that("get_config() returns all options, if an empty list is given",{
  expect_vector(get_config(list()), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(get_config(list()), flatten(movenetenv$options))
})


test_that("movenet.options() returns value of a single option, if this is requested by its full name",{
  expect_vector(movenet.options("origin_ID"), ptype=list(), size =1)
  expect_named(movenet.options("origin_ID"), c("origin_ID"))
  expect_mapequal(movenet.options("origin_ID"), movenetenv$options$movedata_cols["origin_ID"])
})
test_that("movenet.options() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(movenet.options("origin_I"), ptype=list(), size =1)
  expect_named(movenet.options("origin_I"), c("origin_ID"))
  expect_mapequal(movenet.options("origin_I"), movenetenv$options$movedata_cols["origin_ID"])
})
test_that("movenet.options() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(movenet.options("origin_ID", "nr_pigs"), ptype=list(), size = 2)
  expect_named(movenet.options("origin_ID", "nr_pigs"), c("origin_ID", "nr_pigs"))
  expect_mapequal(movenet.options("origin_ID", "nr_pigs"), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("movenet.options() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(movenet.options(c("origin_ID", "nr_pigs")), ptype=list(), size = 2)
  expect_named(movenet.options(c("origin_ID", "nr_pigs")), c("origin_ID", "nr_pigs"))
  expect_mapequal(movenet.options(c("origin_ID", "nr_pigs")), movenetenv$options$movedata_cols[c("origin_ID","nr_pigs")])
})
test_that("movenet.options() returns all options, if no argument is given",{
  expect_vector(movenet.options(), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(movenet.options(), flatten(movenetenv$options))
})
test_that("movenet.options() returns all options, if an empty list is given",{
  expect_vector(movenet.options(list()), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(movenet.options(list()), flatten(movenetenv$options))
})


