#' @title Reading and reformatting of animal movement or holding data
#'
#' @description
#' Reformats movement or holding data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param datafile Path to a delimited file with movement or holding data.
#' @param type Data type: "movement" or "holding"
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr has_element
#' @import readr
#'
#' @return reformatted movement or holding data (selected & renamed columns)
#' @export
#'
#' @examples
#'
reformat_data <- function(datafile, type){

  if (!file.exists(datafile)){
    stop(paste0(datafile, ": no such file exists"))
  }

  if (type == "movement"){
    min_keys <- c("from", "to", "date", "weight")
    fileopts <- movenetenv$options$movedata_fileopts
    cols <- movenetenv$options$movedata_cols
  } else if (type == "holding"){
    min_keys <- c("id")
    fileopts <- movenetenv$options$holdingdata_fileopts
    cols <- movenetenv$options$holdingdata_cols
  } else {
    stop("Argument `type` must be either 'movement' or 'holding'")
  }

  #read in datafile (all columns), with col type initially character for all columns
  all_data <- read_delim(datafile,
                         delim = fileopts$separator,
                         locale = locale(date_format = ifelse("date_format" %in% names(fileopts),fileopts$date_format,"%AD"),
                                         decimal_mark = fileopts$decimal,
                                         encoding = fileopts$encoding),
                         col_types = cols(.default = col_character()),
                         lazy = TRUE,
                         name_repair = asciify)

  #select columns of interest
  minvars <- cols[min_keys] #mandatory
  extra <- cols[is.na(match(names(cols),min_keys))] #optional
  #convert options with integer values (column indices) to column names
  if(any(sapply(cols,is.integer))){
    opt_w_names <- colindex2name(data = all_data, minvars = minvars, extra = extra)
    minvars <- opt_w_names[[1]]
    extra <- opt_w_names[[2]]
  }
  selected_data <- select_cols(data = all_data, minvars = unlist(minvars), extra = unlist(extra))

  #check data & change col types; or raise informative errors
  if (type == "movement"){
    selected_data[minvars$weight] <- reformat_numeric(selected_data[minvars$weight], fileopts$decimal)
    selected_data[minvars$date] <- reformat_date(selected_data[minvars$date], fileopts$date_format)

    if (length(selected_data) > 4){ #set col types of any extra columns by using a guesser algorithm
      selected_extra <- unlist(extra[extra %in% names(selected_data)])
      selected_data[selected_extra] <-
        suppressMessages(type_convert(selected_data[selected_extra],
                                      locale = locale(date_format = ifelse("date_format" %in% names(fileopts),fileopts$date_format,"%AD"),
                                                      decimal_mark = fileopts$decimal)))
    }
  }
  else {
    if ("coord_x" %in% names(extra)){ #requires making sure that coord_x comes with coord_y and EPSG code
      selected_data[extra$coord_x] <- reformat_numeric(selected_data[extra$coord_x], fileopts$decimal)
      selected_data[extra$coord_y] <- reformat_numeric(selected_data[extra$coord_y], fileopts$decimal)
    }
    if ("herd_size" %in% names(extra)){
      selected_data[extra$herd_size] <- reformat_numeric(selected_data[extra$herd_size], fileopts$decimal)
    }
    other_extra <- !(names(extra) %in% c("coord_x","coord_y","herd_size"))
    if (any(other_extra)){ #set col types of any additional extra columns by using a guesser algorithm
      selected_data[unlist(extra[other_extra])] <-
        suppressMessages(type_convert(selected_data[unlist(extra[other_extra])],
                                      locale = locale(date_format = ifelse("date_format" %in% names(fileopts),fileopts$date_format,"%AD"),
                                                      decimal_mark = fileopts$decimal)))
    }
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
    stop(paste("Values for movedata_cols/holdingdata_cols options must be unique. Translation of column indices to column headers identified the following options with duplicate values:", paste(dupl_names, collapse=", ")), call. = FALSE)
  }
  return(list(minvars,extra))
}

select_cols <- function(data, minvars, extra){
  if (!(all(minvars %in% colnames(data)))){
    missing_minvars <- unname(minvars)[which(!(minvars %in% colnames(data)))]
    stop(sprintf("Can't find the following mandatory columns in the datafile: %s.", paste0(missing_minvars, collapse=", ")), call. = FALSE)
  }
  if (!(all(extra %in% colnames(data)))){
    missing_extra <- unname(extra)[which(!(extra %in% colnames(data)))]
    warning(sprintf("Can't find the following requested optional columns in the datafile: %s.\nProceeding without missing optional columns.",
                    paste0(missing_extra, collapse=", ")),
            call. = FALSE)
    to_extract <- unname(c(minvars,extra))[-which(unname(c(minvars,extra)) %in% missing_extra)]
  }else{
    to_extract <- unname(c(minvars,extra))
  }
  data[to_extract]
}

reformat_numeric <- function(numeric_col, decimal){
  tryCatch(
    error = function(cnd) {
      cnd$message <- paste0("Column `",colnames(numeric_col),"` must be numeric and can't contain a grouping mark.")
      cnd$call <- NULL
      stop(cnd)
    },
    withr::with_options(list(warn=2),parse_double(numeric_col[[colnames(numeric_col)]],
                                                  locale = locale(decimal_mark = decimal)))
  )
}

reformat_date <- function(date_col, date_format){
  tryCatch(
    error = function(cnd) {
      old_message <- cnd$message
      msg <- paste0("Can't parse column `",colnames(date_col),"` as date.\n")
      msglist <- c()
      #if a date column contains strings with no numbers (e.g. "foo")
      if(!any(grepl("[0-9]",date_col[[colnames(date_col)]]))){
         msglist <- c(msglist,paste0("Column `",colnames(date_col),"` does not contain any numbers.\nHave you identified the correct column name under the option `date`?"))
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



