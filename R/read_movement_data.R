#' Reformats movement data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param move_data_file Path to a delimited file with movement data (alternatively: literal data or a connection).
#' @param delim Character used as delimiter between fields (optional). The default value (NULL) results in an automatic guess based on the first 1000 lines of data.
#' @param datetime_format Datetime format specification, as described for readr::parse_datetime. The default value ("") results in datetimes being parsed as ISO8601. The format specification must match the complete string.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @import readr
#'
#' @return reformatted movement data (selected & renamed columns)
#' @export
#'
#' @examples
#'
reformat_move_data <- function(move_data_file, delim = NULL, datetime_format = ""){

  #source("R/config.R") #Never use source in a package - use other way of setting options
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



