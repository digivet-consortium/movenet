test_that("all config files in configurations folder are valid",{
  for (config in Sys.glob(paste0(system.file("configurations",package="movenet"),"/*.yml"))){
    expect_true(validate_config(!!config))
   }
 })
