#' Reformats movement data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param move_data_file a delimited file with movement data
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom readr read_delim parse_datetime
#'
#' @return reformatted movement data (selected & renamed columns)
#' @export
#'
#' @examples
#'
reformat_move_data <- function(move_data_file, delim = NULL, datetime_format = ""){

  source("R/config.R") #better as argument to function, or turned into .yml file?
  varlist <- c(move_ID,origin_ID,dest_ID,dep_date,arr_date,nr_pigs)

  read_delim(move_data_file, delim = delim,
             col_select = eval(varlist),
             col_types = cols(.default = col_character())
             ) %>%
    `colnames<-`(c("move_ID","origin_ID","dest_ID","dep_date","arr_date","nr_pigs")) %>%
    mutate(dep_date = parse_datetime(dep_date, format = datetime_format),
           arr_date = parse_datetime(arr_date, format = datetime_format),
           nr_pigs = as.integer(nr_pigs))

  }
