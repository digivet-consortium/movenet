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

  #read in datafile (all columns), with col type initially character for all columns
  all_data <- read_delim(move_data_file, delim = delim, col_types = cols(.default = col_character()), lazy = TRUE)

  #select columns of interest
  minvars <- movenetenv$options$movement_data[min_move_keys] #mandatory
  extra <- movenetenv$options$movement_data[is.na(match(names(movenetenv$options$movement_data),min_move_keys))] #optional
  #convert options with integer values (column indices) to column names
  if(any(sapply(movenetenv$options$movement_data,is.integer))){
    opt_w_names <- colindex2name(data = all_data, minvars = minvars, extra = extra)
    minvars <- opt_w_names[[1]]
    extra <- opt_w_names[[2]]
  }
  selected_data <- select_cols(data = all_data, minvars = minvars, extra = extra)

  #check data & change col types; or raise informative errors
  selected_data[minvars$movenet.nr_pigs] <- reformat_nrpigs(selected_data[minvars$movenet.nr_pigs])
  selected_data[minvars$movenet.move_date] <- reformat_date(selected_data[minvars$movenet.move_date], datetime_format = datetime_format)
  if (length(selected_data) > 4){
    selected_data[unlist(extra[extra %in% names(selected_data)])] <- suppressMessages(type_convert(selected_data[unlist(extra[extra %in% names(selected_data)])])) #guess coltype of extra columns
  }

  return(selected_data)
}

colindex2name <- function(data, minvars, extra){
  if (any(sapply(minvars, is.integer))){
    if (any(sapply(minvars, function(x) (is.integer(x) & x > length(data))))){
      outofrange_minvars <- minvars[which(sapply(minvars, function(x) (is.integer(x) & x > length(data))))]
      stop(sprintf("Can't find the following mandatory columns in the datafile: %s.\nThese column indices exceed the number of columns in the datafile.",
                   paste0("#",outofrange_minvars," (",names(outofrange_minvars),")", collapse = ", ")), call. = FALSE)
    }
    minvars[names(minvars[which(sapply(minvars, is.integer))])] <- colnames(data)[unlist(minvars[which(sapply(minvars, is.integer))])]
  }
  if (any(sapply(extra, is.integer))){
    if (any(sapply(extra, function(x) (is.integer(x) & x > length(data))))){
      outofrange_extra <- extra[which(sapply(extra, function(x) (is.integer(x) & x > length(data))))]
      warning(sprintf("Can't find the following requested optional columns in the datafile: %s.\nThese column indices exceed the number of columns in the datafile.\nProceeding without missing optional columns.",
                      paste0("#",outofrange_extra," (",names(outofrange_extra),")", collapse = ", ")), call. = FALSE)
      extra[which(extra %in% outofrange_extra)] <- NULL
    }
    withinrange_extra <- extra[which(sapply(extra, function(x) (is.integer(x) & x <= length(data))))]
    extra[names(extra[which(extra %in% withinrange_extra)])] <- colnames(data)[unlist(withinrange_extra)]
  }
  return(list(minvars,extra))
}

select_cols <- function(data, minvars, extra){
  if (!(all(unlist(minvars) %in% colnames(data)))){
    missing_minvars <- unname(unlist(minvars))[which(!(unlist(minvars) %in% colnames(data)))]
    stop(sprintf("Can't find the following mandatory columns in the datafile: %s.", paste0(missing_minvars, collapse=", ")), call. = FALSE)
  }
  if (!(all(unlist(extra) %in% colnames(data)))){
    missing_extra <- unname(unlist(extra))[which(!(unlist(extra) %in% colnames(data)))]
    warning(sprintf("Can't find the following requested optional columns in the datafile: %s.\nProceeding without missing optional columns.",
                    paste0(missing_extra, collapse=", ")),
            call. = FALSE)
    to_extract <- unname(unlist(c(minvars,extra)))[-which(unname(unlist(c(minvars,extra))) %in% missing_extra)]
  }else{
    to_extract <- unname(unlist(c(minvars,extra)))
  }
  data[to_extract]
}

reformat_nrpigs <- function(nr_pigs_col){
  tryCatch(
    error = function(cnd) {
      cnd$message <- paste0("Column `",colnames(nr_pigs_col),"` must be an integer: it can't contain a decimal or grouping mark.")
      cnd$call <- NULL
      stop(cnd)
    },
    withr::with_options(list(warn=2),parse_integer(nr_pigs_col[[colnames(nr_pigs_col)]])))
}

reformat_date <- function(date_col, datetime_format){
  tryCatch(
    error = function(cnd) {
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
      cnd$message <- paste0(msg, paste0(msglist,collapse="\nIn addition:\n"))
      cnd$call <- NULL
      stop(cnd)
    },
    withr::with_options(list(warn=2),
                        parse_datetime(date_col[[colnames(date_col)]], format = datetime_format))
    )
}





