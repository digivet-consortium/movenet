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

  minvars <- movenetenv$options$movement_data[c("movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs")]
  extra <- movenetenv$options$movement_data[is.na(match(names(movenetenv$options$movement_data),c("movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs")))]

  #create list with column types for minimal variables
  minvar_coltypes <- list("c", "c", col_datetime(format = datetime_format), "i") #or any numeric for nr_pigs? (to allow for use with units / application of some probability function)
  names(minvar_coltypes) <- lapply(unname(minvars),FUN=as.name)

  read_delim(move_data_file, delim = delim,
             col_select = unname(unlist(c(minvars,extra))),
             col_types = minvar_coltypes #this guesses column type when not specified, i.e. for extra variables
             )

}



