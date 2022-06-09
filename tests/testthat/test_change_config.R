test_that("load_config() loads options from existing, valid config file w quotes",{
  expect_message(load_config("Denmark"), "Successfully loaded config file:")
  load_config("Denmark")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movement_data, yaml.load_file(system.file("configurations", "Denmark.yml", package="movenet"))$movement_data)
}
)

test_that("load_config() loads options from existing, valid config file wo quotes",{
  expect_message(load_config("ScotEID_noquotationmarks"),"Successfully loaded config file:")
  load_config("ScotEID_noquotationmarks")
  expect_mapequal(movenetenv$options, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet")))
  expect_mapequal(movenetenv$options$movement_data, yaml.load_file(system.file("configurations", "ScotEID.yml", package="movenet"))$movement_data)

}
)

test_that("load_config() raises error when requested config file does not exist",{
  expect_error(load_config("doesnotexist"), "Specified config file not found:")
  }
)

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
    expect_mapequal(movenetenv$options$movement_data, yaml.load_file(system.file("configurations", "test_save_config.yml", package="movenet"))$movement_data)
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
    expect_mapequal(yaml.load_file("template.yml")$movement_data, yaml.load_file(system.file("configurations", "template.yml", package="movenet"))$movement_data)
    #expect_mapequal seems a bit excessive here, I do not need to test copy(), but I don't really know how best to test
  })
})

#Any edge / error cases to test for new_config? (no writing permission? shouldn't happen in wd?)


test_that("get_config() returns value of a single option, if this is requested by its full name",{
  expect_vector(get_config("movenet.move_ID"), ptype=list(), size =1)
  expect_named(get_config("movenet.move_ID"), c("movenet.move_ID"))
  expect_mapequal(get_config("movenet.move_ID"), movenetenv$options$movement_data["movenet.move_ID"])
})
test_that("get_config() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(get_config("movenet.move_I"), ptype=list(), size =1)
  expect_named(get_config("movenet.move_I"), c("movenet.move_ID"))
  expect_mapequal(get_config("movenet.move_I"), movenetenv$options$movement_data["movenet.move_ID"])
})
test_that("get_config() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(get_config("movenet.move_ID", "movenet.nr_pigs"), ptype=list(), size = 2)
  expect_named(get_config("movenet.move_ID", "movenet.nr_pigs"), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config("movenet.move_ID", "movenet.nr_pigs"), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(get_config(c("movenet.move_ID", "movenet.nr_pigs")), ptype=list(), size = 2)
  expect_named(get_config(c("movenet.move_ID", "movenet.nr_pigs")), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config(c("movenet.move_ID", "movenet.nr_pigs")), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by their full names as a single list",{
  expect_vector(get_config(list("movenet.move_ID", "movenet.nr_pigs")), ptype=list(), size = 2)
  expect_named(get_config(list("movenet.move_ID", "movenet.nr_pigs")), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config(list("movenet.move_ID", "movenet.nr_pigs")), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as separate character strings",{
  expect_vector(get_config("movenet.move_I", "movenet.nr_pig"), ptype=list(), size = 2)
  expect_named(get_config("movenet.move_I", "movenet.nr_pig"), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config("movenet.move_I", "movenet.nr_pig"), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single character vector",{
  expect_vector(get_config(c("movenet.move_I", "movenet.nr_pig")), ptype=list(), size = 2)
  expect_named(get_config(c("movenet.move_I", "movenet.nr_pig")), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config(c("movenet.move_I", "movenet.nr_pig")), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns value of multiple options, if requested by partial matches as a single list",{
  expect_vector(get_config(list("movenet.move_I", "movenet.nr_pig")), ptype=list(), size = 2)
  expect_named(get_config(list("movenet.move_I", "movenet.nr_pig")), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(get_config(list("movenet.move_I", "movenet.nr_pig")), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is unmatched",{
  expect_warning(get_config("movenet.move_IX"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("movenet.move_IX"), ptype=list(), size =0)
})
test_that("get_config() returns a warning and an empty list, if a single option is requested but this is ambiguous",{
  expect_warning(get_config("movenet."), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("movenet."), ptype=list(), size =0)
})
test_that("get_config() returns the value of recognised options and a warning, if these are requested as separate character strings and include an unmatched option",{
  expect_warning(get_config("movenet.move_IX", "movenet.nr_pigs"), "Ignoring unmatched or ambiguous")
  expect_vector(get_config("movenet.move_IX", "movenet.nr_pigs"), ptype=list(), size = 1)
  expect_named(get_config("movenet.move_IX", "movenet.nr_pigs"), c("movenet.nr_pigs"))
  expect_mapequal(get_config("movenet.move_IX", "movenet.nr_pigs"), movenetenv$options$movement_data["movenet.nr_pigs"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as single char vector, incl an unmatched option",{
  expect_warning(get_config(c("movenet.move_IX", "movenet.nr_pigs")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(c("movenet.move_IX", "movenet.nr_pigs")), ptype=list(), size = 1)
  expect_named(get_config(c("movenet.move_IX", "movenet.nr_pigs")), c("movenet.nr_pigs"))
  expect_mapequal(get_config(c("movenet.move_IX", "movenet.nr_pigs")), movenetenv$options$movement_data["movenet.nr_pigs"])
})
test_that("get_config() returns value of recognised options and a warning, if these are requested as list, and incl an unmatched option",{
  expect_warning(get_config(list("movenet.move_IX", "movenet.nr_pigs")), "Ignoring unmatched or ambiguous")
  expect_vector(get_config(list("movenet.move_IX", "movenet.nr_pigs")), ptype=list(), size = 1)
  expect_named(get_config(list("movenet.move_IX", "movenet.nr_pigs")), c("movenet.nr_pigs"))
  expect_mapequal(get_config(list("movenet.move_IX", "movenet.nr_pigs")), movenetenv$options$movement_data["movenet.nr_pigs"])
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
  expect_vector(movenet.options("movenet.move_ID"), ptype=list(), size =1)
  expect_named(movenet.options("movenet.move_ID"), c("movenet.move_ID"))
  expect_mapequal(movenet.options("movenet.move_ID"), movenetenv$options$movement_data["movenet.move_ID"])
})
test_that("movenet.options() returns value of a single option, if this is requested by a correct partial match",{
  expect_vector(movenet.options("movenet.move_I"), ptype=list(), size =1)
  expect_named(movenet.options("movenet.move_I"), c("movenet.move_ID"))
  expect_mapequal(movenet.options("movenet.move_I"), movenetenv$options$movement_data["movenet.move_ID"])
})
test_that("movenet.options() returns value of multiple options, if requested by their full names as separate character strings",{
  expect_vector(movenet.options("movenet.move_ID", "movenet.nr_pigs"), ptype=list(), size = 2)
  expect_named(movenet.options("movenet.move_ID", "movenet.nr_pigs"), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(movenet.options("movenet.move_ID", "movenet.nr_pigs"), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("movenet.options() returns value of multiple options, if requested by their full names as a single character vector",{
  expect_vector(movenet.options(c("movenet.move_ID", "movenet.nr_pigs")), ptype=list(), size = 2)
  expect_named(movenet.options(c("movenet.move_ID", "movenet.nr_pigs")), c("movenet.move_ID", "movenet.nr_pigs"))
  expect_mapequal(movenet.options(c("movenet.move_ID", "movenet.nr_pigs")), movenetenv$options$movement_data[c("movenet.move_ID","movenet.nr_pigs")])
})
test_that("movenet.options() returns all options, if no argument is given",{
  expect_vector(movenet.options(), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(movenet.options(), flatten(movenetenv$options))
})
test_that("movenet.options() returns all options, if an empty list is given",{
  expect_vector(movenet.options(list()), ptype=list(), size = length(flatten(movenetenv$options)))
  expect_mapequal(movenet.options(list()), flatten(movenetenv$options))
})


