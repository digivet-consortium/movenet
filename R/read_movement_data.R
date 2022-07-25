#' @title Reading and reformatting of animal movement data
#'
#' @description
#' Reformats movement data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param move_data_file Path to a delimited file with movement data (alternatively: literal data or a connection).
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
reformat_move_data <- function(move_data_file){

  if (!file.exists(move_data_file)){
    stop(paste0(move_data_file, ": no such file exists"))
  }

  min_move_keys <- c("origin_ID", "dest_ID", "move_date", "nr_pigs")

  #read in datafile (all columns), with col type initially character for all columns
  all_data <- read_delim(move_data_file,
                         delim = movenetenv$options$movedata_fileopts$separator,
                         locale = locale(date_format = movenetenv$options$movedata_fileopts$date_format,
                                         decimal_mark = movenetenv$options$movedata_fileopts$decimal,
                                         encoding = movenetenv$options$movedata_fileopts$encoding),
                         col_types = cols(.default = col_character()),
                         lazy = TRUE,
                         name_repair = asciify)

  #select columns of interest
  minvars <- movenetenv$options$movedata_cols[min_move_keys] #mandatory
  extra <- movenetenv$options$movedata_cols[is.na(match(names(movenetenv$options$movedata_cols),min_move_keys))] #optional
  #convert options with integer values (column indices) to column names
  if(any(sapply(movenetenv$options$movedata_cols,is.integer))){
    opt_w_names <- colindex2name(data = all_data, minvars = minvars, extra = extra)
    minvars <- opt_w_names[[1]]
    extra <- opt_w_names[[2]]
  }
  selected_data <- select_cols(data = all_data, minvars = minvars, extra = extra)

  #check data & change col types; or raise informative errors
  selected_data[minvars$nr_pigs] <- reformat_nrpigs(selected_data[minvars$nr_pigs])
  selected_data[minvars$move_date] <- reformat_date(selected_data[minvars$move_date])
  if (length(selected_data) > 4){
    selected_data[unlist(extra[extra %in% names(selected_data)])] <-
      suppressMessages(type_convert(selected_data[unlist(extra[extra %in% names(selected_data)])], #guess coltype of extra columns
                                    locale = locale(date_format = movenetenv$options$movedata_fileopts$date_format,
                                                    decimal_mark = movenetenv$options$movedata_fileopts$decimal)))
  }
  return(selected_data)
}

#' @export
asciify <- function(x) make.names(stringi::stri_trans_general(x, 'Latin-ASCII'), unique=TRUE)

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
  if(anyDuplicated(c(minvars,extra)) != 0){
    dupl_names <- names(c(minvars,extra))[which(c(minvars,extra) %in% c(minvars,extra)[duplicated(c(minvars,extra))])]
    stop(paste("Values for movedata_cols options must be unique. Translation of column indices to column headers identified the following options with duplicate values:", paste(dupl_names, collapse=", ")), call. = FALSE)
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
      cnd$message <- paste0("Column `",colnames(nr_pigs_col),"` must be numeric and can't contain a grouping mark.")
      cnd$call <- NULL
      stop(cnd)
    },
    withr::with_options(list(warn=2),parse_double(nr_pigs_col[[colnames(nr_pigs_col)]],
                                                  locale = locale(decimal_mark = movenetenv$options$movedata_fileopts$decimal)))
  )
}

reformat_date <- function(date_col){
  date_format <- movenetenv$options$movedata_fileopts$date_format
  tryCatch(
    error = function(cnd) {
      old_message <- cnd$message
      msg <- paste0("Can't parse column `",colnames(date_col),"` as date.\n")
      msglist <- c()
      #if a date column contains strings with no numbers (e.g. "foo")
      if(!any(grepl("[0-9]",date_col[[colnames(date_col)]]))){
         msglist <- c(msglist,paste0("Column `",colnames(date_col),"` does not contain any numbers.\nHave you identified the correct column name under the option `move_date`?"))
      }
      #Otherwise (difficult to assess what specific situation applies)
      #- other forms of "date column can simply not be a date" - e.g. 34/345/12
      #- if date format string is invalid (format does not match data) - e.g. %m%d%Y when it is %d%m%Y
      #- if date format string is missing (format is incorrectly interpreted as iso)
      #- if a date column contains some invalid dates - e.g. 30 Feb
      if(length(msglist)==0){
        msglist <- c(msglist,paste0("The date format specification given through the option `date_format` (value `",date_format,"`) and the actual format of column `",colnames(date_col),"` don't appear to match.\nAlternatively, column `",colnames(date_col),
                                    "` contains one or more invalid dates.\nSee `readr::?parse_date` for guidance on readr date format specifications.\nOriginal readr warning message:\n",old_message))
      }
      cnd$message <- paste0(msg, paste0(msglist,collapse="\nIn addition:\n"))
      cnd$call <- NULL
      stop(cnd)
    },
    withr::with_options(list(warn=2),
                        parse_date(date_col[[colnames(date_col)]], format = date_format))
    )
}





