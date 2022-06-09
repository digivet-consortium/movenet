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

  varlist <- movenetenv$options$movement_data[c("movenet.move_ID","movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs")]

  read_delim(move_data_file, delim = delim,
             col_select = unlist(varlist),
             col_types = cols(.default = col_character())
             ) %>%
    `colnames<-`(gsub("movenet\\.(.*)","\\1",names(varlist))) %>%
    mutate(move_date = parse_datetime(move_date, format = datetime_format),
           nr_pigs = as.integer(nr_pigs)) #or any numeric? (to allow for use with units / application of some probability function)

}



