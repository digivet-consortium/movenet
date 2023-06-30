#' @title Reading and reformatting of animal movement or holding data
#'
#' @description
#' Reformats movement or holding data from a delimited file into a common
#' intermediate format (extracts and renames selected columns)
#'
#' @param datafile Path to a delimited file with movement or holding data.
#' @param type Data type: "movement" or "holding"
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr has_element
#' @importFrom withr with_options
#' @import checkmate
#' @import readr
#'
#' @details
#'
#' @return reformatted movement or holding data (selected & renamed columns)
#'
#' @export
reformat_data <- function(datafile, type){ #Could also infer type from the data

  #######################
  ### Argument checks ###
  #######################

  assert_file_exists(datafile, access = "r")
  #could add extension = c(".csv",".tsv") or something similar, but I'd expect
  #not all suitable (delimited) files to necessarily have those extensions

  assert_choice(type, choices = c("movement","holding"))
  #Could also infer type from the data


  #########################
  ### Config file check ###
  #########################

  config_file_check(type)


  ###################################
  ### Definition of key variables ###
  ###################################

  if (type == "movement"){
    min_keys <- c("from", "to", "date", "weight")
    fileopts <- movenetenv$options$movedata_fileopts
    cols <- movenetenv$options$movedata_cols
  } else {
    min_keys <- c("id")
    fileopts <- movenetenv$options$holdingdata_fileopts
    cols <- movenetenv$options$holdingdata_cols
  }

  decimal <- fileopts$decimal
  encoding <- fileopts$encoding
  separator <- fileopts$separator
  date_format <-
    if("date_format" %in% names(fileopts)) fileopts$date_format else "%AD"

  minvars <- cols[min_keys] #list of mandatory column headers (or indices)
  extra <- cols[is.na(match(names(cols),min_keys))] #list of opt col headers/ind


  ###########################
  ### Reading in datafile ###
  ###########################

  #read in datafile (all columns), with col type set as character for all cols
  all_data <- read_delim(datafile,
                         delim = separator,
                         locale = locale(date_format = date_format,
                                         decimal_mark = decimal,
                                         encoding = encoding),
                         col_types = cols(.default = col_character()),
                         lazy = TRUE,
                         name_repair = asciify)

  #rationale for reading all data (more expensive) instead of directly selecting
  #cols of interest:
  #a separate select_cols function allows for easy checking of presence/absence
  #of mandatory or optional columns, and handling these differently, without
  #complicated tryCatch loops with custom errors.
  #Unclear how to tryCatch this while reporting all missing columns at once - as
  #read_delim's original message (can be obtained with conditionMessage) only
  #mentions the first missing column it comes across.

  #Also:
  #can set col_types directly with cols({{minvars$date}} := col_date(), etc),
  #but if something goes wrong with coltype, you get NA with a readr/vroom warning
  #message that asks you to run problems() for details.


  #############################
  ### Reformatting key data ###
  #############################

  #convert options with integer values (column indices) to column names
  if(any(sapply(cols, is.integer))){
    opt_w_names <- colindex2name(all_data, minvars, extra)
    minvars <- opt_w_names[[1]]
    extra <- opt_w_names[[2]]
    suppressWarnings(change_config(c(minvars, extra)))
  }

  #select columns of interest
  selected_data <- select_cols(all_data, unlist(minvars), unlist(extra))

  #check data & change column types; or raise informative errors
  if (type == "movement"){
    selected_data[minvars$weight] <-
      reformat_numeric(selected_data[minvars$weight], decimal)
    selected_data[minvars$date] <-
      reformat_date(selected_data[minvars$date], date_format)

    if (length(selected_data) > 4){
      #set col types of any extra columns by using a guesser algorithm
      selected_extra <- unlist(extra[extra %in% names(selected_data)])
      selected_data[selected_extra] <-
        suppressMessages(
          type_convert(selected_data[selected_extra],
                       locale = locale(date_format = date_format,
                                       decimal_mark = decimal)))
    }
  } else { #if type == "holding"
    if ("coord_x" %in% names(extra)){
      selected_data[extra$coord_x] <-
        reformat_numeric(selected_data[extra$coord_x], decimal)
      selected_data[extra$coord_y] <-
        reformat_numeric(selected_data[extra$coord_y], decimal)
    }
    if ("herd_size" %in% names(extra)){
      selected_data[extra$herd_size] <-
        reformat_numeric(selected_data[extra$herd_size], decimal)
    }
    other_extra <- !(names(extra) %in% c("coord_x","coord_y","herd_size"))
    if (any(other_extra)){
      #set col types of any additional extra cols by using a guesser algorithm
      selected_data[unlist(extra[other_extra])] <-
        suppressMessages(
          type_convert(selected_data[unlist(extra[other_extra])],
                       locale = locale(date_format = date_format,
                                       decimal_mark = decimal)))
    }
  }
  return(selected_data)
}

################################################################################

#######################################################################
### Helper: assert that the given data type matches the config file ###
#######################################################################

#' config_file_check
#'
#' Stops execution if the config file does not match the indicated type of data
#'
#' @param type character (either "movement" or "holding")

config_file_check <- function(type) {
  if ((has_element(names(movenetenv$options), "movedata_cols") &
       type == "holding") |
      (has_element(names(movenetenv$options), "holdingdata_cols") &
       type == "movement")){
    stop(paste0(
      "The loaded config file does not match the indicated type of data (",
      type, " data). Please ensure the appropriate config file is loaded."))
  }
}


################################################################################

############################################################
### Helper: make column names ascii-compliant and unique ###
############################################################

#' asciify
#'
#' @export
#' @importFrom stringi stri_trans_general
#' @param x character
asciify <- function(x){
  make.names(stringi::stri_trans_general(x, 'Latin-ASCII'),
             unique=TRUE)
}

################################################################################

################################################################################
### Helper: convert options with column indices (int) to column names (char) ###
################################################################################

colindex2name <- function(data, minvars, extra){

 # Start building warning message with overview of any int options values (col
 # indices) that have been recoded as characters (col names)

  msg <- "The following options have been changed from column indices to column
  headers within the loaded configurations:\n - "

  if (any(sapply(minvars, is.integer))){

    int_minvars <- minvars[which(sapply(minvars, is.integer))]


  # Check for required options w/ out-of-range indices, raise error if any exist

    minvars_outofrange <-
      sapply(minvars, function(x) (is.integer(x) & x > length(data)))

    if (any(minvars_outofrange)){

      outofrange_minvars <- minvars[which(minvars_outofrange)]

      stop(
      sprintf("Can't find the following mandatory columns in the datafile:
              %s.\nThese column indices exceed the number of columns in the
              datafile.",
              paste0("#",outofrange_minvars," (",names(outofrange_minvars),")",
                     collapse = ", ")),
      call. = FALSE)
    }


  # Change column indices to column headers for required options;
  # add these changes to warning message

    changes_minvar <-
      paste0(names(int_minvars),": #", int_minvars, " -> '",
             colnames(data)[unlist(int_minvars)],"'", collapse = "\n - ")

    msg <- paste0(msg, changes_minvar)

    minvars[names(int_minvars)]  <- colnames(data)[unlist(int_minvars)]
  }


  if (any(sapply(extra, is.integer))){

  # Check for extra options w/ out-of-range indices; if any exist, raise
  # (second) warning and proceed without these optional columns

    extra_outofrange <-
      sapply(extra, function(x) (is.integer(x) & x > length(data)))

    if (any(extra_outofrange)){

      outofrange_extra <- extra[which(extra_outofrange)]

      warning(
        sprintf("Can't find the following requested optional columns in the
                datafile: %s.\nThese column indices exceed the number of
                columns in the datafile.\nProceeding without missing optional
                columns.",
                paste0("#",outofrange_extra," (",names(outofrange_extra),")",
                       collapse = ", ")),
        call. = FALSE)

      extra[which(extra %in% outofrange_extra)] <- NULL
    }


  # Change column indices to column headers for extra options that are within-
  # range; add these changes to (first) warning message

    withinrange_extra <-
      extra[which(sapply(extra,
                         function(x) (is.integer(x) & x <= length(data))))]

    if(length(withinrange_extra) > 0){

      changes_extra <-
        paste0(names(withinrange_extra),": #", withinrange_extra, " -> '",
               colnames(data)[unlist(withinrange_extra)],"'",
               collapse = "\n - ")

      msg <- paste0(msg, "\n - ", changes_extra)

      extra[names(extra[which(extra %in% withinrange_extra)])] <-
        colnames(data)[unlist(withinrange_extra)]
    }
  }


 # Check for duplicate option values (column names); raise error if these exist

  if(anyDuplicated(c(minvars,extra)) != 0){

    dupl_names <-
      names(c(minvars,extra))[which(
        c(minvars,extra) %in% c(minvars,extra)[duplicated(c(minvars,extra))])]

    stop(paste("Values for movedata_cols/holdingdata_cols options must be unique
               . Translation of column indices to column headers identified the
               following options with duplicate values:",
               paste(dupl_names, collapse=", ")),
         call. = FALSE)
  }

# Raise warning message with overview of any int options values (col indices)
# that have been recoded as characters (col names)

  warning(msg, call. = FALSE)
  return(list(minvars,extra))
}

################################################################################

######################################################
### Helper: check for & select columns of interest ###
######################################################

select_cols <- function(data, minvars, extra){

  #If not all mandatory columns (minvars) are present in the data, raise error
  if (!(all(minvars %in% colnames(data)))){
    missing_minvars <- unname(minvars)[which(!(minvars %in% colnames(data)))]
    stop(
      sprintf("Can't find the following mandatory columns in the datafile: %s.",
              paste0(missing_minvars, collapse=", ")),
      call. = FALSE)
  }

  #If not all requested optional columns (extra) are present in the data, raise
  #warning and proceed without the missing optional columns
  if (!(all(extra %in% colnames(data)))){
    missing_extra <- unname(extra)[which(!(extra %in% colnames(data)))]
    warning(
      sprintf("Can't find the following requested optional columns in the
              datafile: %s.\nProceeding without missing optional columns.",
              paste0(missing_extra, collapse=", ")),
      call. = FALSE)

    to_extract <-
      unname(c(minvars,extra))[-which(
        unname(c(minvars,extra)) %in% missing_extra)]

  }else{

    to_extract <- unname(c(minvars,extra))

  }

  #Extract mandatory & requested optional columns from the data frame
  data[to_extract]
}

################################################################################

##################################################################
### Helper: check numeric columns & change col type to numeric ###
##################################################################

reformat_numeric <- function(numeric_col, decimal){

  #Normally, parse_double() raises a warning if it can't read something as
  #numeric; but with the structure below, such cases now trigger a custom
  #error message (as warnings are temporarily turned into errors)

  tryCatch(
    error = function(cnd) {
      cnd$message <-
        paste0("Column `",colnames(numeric_col),
               "` must be numeric and can't contain a grouping mark.")
      cnd$call <- NULL
      stop(cnd)
    },

    with_options(  #this changes global options temporarily, while running func
      list(warn=2),  #this turns warnings into errors
      parse_double(numeric_col[[colnames(numeric_col)]],
                   locale = locale(decimal_mark = decimal)))

  )
}

################################################################################

############################################################
### Helper: check date columns & change col type to Date ###
############################################################

reformat_date <- function(date_col, date_format){

  #Normally, parse_date() raises a warning if it can't read something as a date;
  #but with the structure below, such cases now trigger a custom error message
  #(as warnings are temporarily turned into errors)

  tryCatch(
    error = function(cnd) {
      old_message <- cnd$message

      #Start building an informative error message
      msg <- paste0("Can't parse column `",colnames(date_col),"` as date.\n")
      msglist <- c()

      #if a date column contains strings with no numbers (e.g. "foo"):
      if(!any(grepl("[0-9]",date_col[[colnames(date_col)]]))){
         msglist <-
           c(msglist, paste0(
             "Column `", colnames(date_col), "` does not contain any numbers.\n
             Have you identified the correct column name under the option `date`
             ?"))
      }

      #Otherwise (difficult to assess what specific situation applies):
      #- other forms of "date column can simply not be a date" - e.g. 34/345/12
      #- if date format string is invalid (format does not match data) -
      #    e.g. %m%d%Y when it is %d%m%Y
      #- if date format string is missing (format is incorrectly considered iso)
      #- if a date column contains some invalid dates - e.g. 30 Feb
      if(length(msglist)==0){
        msglist <-
          c(msglist,paste0(
            "The date format specification given through the option `date_format
            ` (value `", date_format, "`) and the actual format of column `",
            colnames(date_col), "` don't appear to match.\nAlternatively, column
            `", colnames(date_col), "` contains one or more invalid dates.\nSee
            `readr::?parse_date` for guidance on readr date format
            specifications.\nOriginal readr warning message:\n", old_message))
      }
      cnd$message <- paste0(msg, paste0(msglist,collapse="\nIn addition:\n"))
      cnd$call <- NULL
      stop(cnd)
    },

    with_options(  #this changes global options temporarily, while running func
      list(warn=2),  #this turns warnings into errors
      parse_date(date_col[[colnames(date_col)]],
                 format = date_format))
    )
}
