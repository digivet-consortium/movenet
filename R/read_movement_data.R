#' Reformats movement data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param move_data_file Path to a delimited file with movement data (alternatively: literal data or a connection).
#' @param delim Character used as delimiter between fields (optional). The default value (NULL) results in an automatic guess based on the first 1000 lines of data.
#' @param datetime_format Datetime format specification, as described for readr::parse_datetime. The default value ("") results in datetimes being parsed as ISO8601. The format specification must match the complete string.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr has_element
#' @import readr
#'
#' @return reformatted movement data (selected & renamed columns)
#' @export
#'
#' @examples
#'
reformat_move_data <- function(move_data_file, delim = NULL, datetime_format = ""){

  if (!file.exists(move_data_file)){
    stop(paste0(move_data_file, ": no such file exists"))
  }

  min_move_keys <- c("movenet.origin_ID", "movenet.dest_ID", "movenet.move_date", "movenet.nr_pigs")
  if (!all(min_move_keys %in% names(movenetenv$options$movement_data))){
    stop(sprintf("Unexpected config structure. Missing mandatory movement_data keys: %s", paste0(min_move_keys[!min_move_keys %in% names(movenetenv$options$movement_data)],collapse=", ")))
  }
  minvars <- movenetenv$options$movement_data[min_move_keys]

  extra <- movenetenv$options$movement_data[is.na(match(names(movenetenv$options$movement_data),c("movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs")))]


  #create list with column types for minimal variables - first keep all minvars as character, then change in separate step below
  minvar_coltypes <- list("c", "c", "c", "c")
  names(minvar_coltypes) <- lapply(unname(minvars),FUN=as.name)

  #read in selected columns, with min cols as character type
  selected_data <- read_delim(move_data_file, delim = delim,
                              col_select = unname(unlist(c(minvars,extra))),
                              col_types = minvar_coltypes #this guesses column type when not specified, i.e. for extra variables
                              )
  #Here need to trycatch and (i) if minvar missing, error; (ii) if extra var missing, warning + take this out of the col_select list and run again
  #Or: first read in ALL columns, then select columns afterwards

  #check and change col types for nr_pigs and move_date; or raise informative errors
  selected_data[minvars$movenet.nr_pigs] <- reformat_nrpigs(selected_data[minvars$movenet.nr_pigs])
  selected_data[minvars$movenet.move_date] <- reformat_date(selected_data[minvars$movenet.move_date], datetime_format = datetime_format)

  return(selected_data)
}

reformat_nrpigs <- function(nr_pigs_col){
  tryCatch(
    warning = function(cnd) {
      cnd$message <- paste0("Column `",colnames(nr_pigs_col),"` must be an integer: it can't contain a decimal or grouping mark.")
      suppressWarnings(stop(cnd, call. = FALSE))
    },
    parse_integer(nr_pigs_col[[colnames(nr_pigs_col)]])
  )
}

reformat_date <- function(date_col, datetime_format){
  tryCatch(
    warning = function(cnd) {
      old_message <- cnd$message
      msg <- paste0("Can't parse column `",colnames(date_col),"` as date.\n")
      msglist <- c()
      #if datetime format string is invalid (not a recognisable datetime format string)
      if(!grepl("%(Y|y|AD|D|F|x|s)|^$",datetime_format)){
         msglist <- c(msglist,paste0("`datetime_format` must match readr date(time) format specifications; its specified value `",datetime_format,"` doesn't.\nSee `?parse_datetime` for guidance."))
      }
      #if a date column contains strings with no numbers (e.g. "foo")
      if(!any(grepl("[0-9]",date_col[[colnames(date_col)]]))){
         msglist <- c(msglist,paste0("Column `",colnames(date_col),"` does not contain any numbers.\nHave you identified the correct column name under the option `movenet.move_date`?"))
      }
      #Otherwise (difficult to assess what specific situation applies)
      #- other forms of "date column can simply not be a date" - e.g. 34/345/12
      #- if datetime format string is invalid (format does not match data) - e.g. %m%d%Y when it is %d%m%Y
      #- if datetime format string is missing (format is incorrectly interpreted as iso)
      #- if a date column contains some invalid dates - e.g. 30 Feb
      if(length(msglist)==0){
        msglist <- c(msglist,paste0("The specified `datetime_format` (value `",datetime_format,"`) and the actual format of column `",colnames(date_col),"` don't appear to match.\nAlternatively, column `",colnames(date_col),
                                    "` contains one or more invalid dates.\nSee `?parse_datetime` for guidance on readr date(time) format specifications.\nOriginal readr warning message:\n",old_message))
      }
      cnd$message <- paste0(msg, paste0(msglist,collapse="\nAdditionally:\n"))
      suppressWarnings(stop(cnd, call. = FALSE))
    },
    parse_datetime(date_col[[colnames(date_col)]], format = datetime_format)
  )
}





