config_names <- c("hello", "date_time")

for(cn in config_names){
  test_that("get_config works", {
    expect_length(movenet:::get_config(cn), 1)
  })
}
