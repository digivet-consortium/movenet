for (config in Sys.glob(paste0(system.file("configurations",package="movenet"),"/*.yml"))){
  test_that(paste(config, "is a valid config file"),{
    expect_true(validate_config(!!config))
  }
  )
}
