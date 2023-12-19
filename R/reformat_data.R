#' Read in livestock movement or holding data, and reshape to movenet format
#'
#' @description
#' `reformat_data()` reads in movement or holding data from a delimited `datafile`,
#' and reshapes them to a format compatible with movenet pseudonymisation,
#' network analysis, and modelling workflows.
#'
#' N.B.: To correctly interpret and process `datafile`, the function requires
#' configurations of the relevant `type` to be loaded into the movenet environment.
#'
#' `reformat_data()` processes `datafile` as follows:
#' * Columns with minimally required data, and any additional optional columns
#' as indicated in the loaded configurations, are extracted from `datafile`.
#' * Column headers are converted to unique ASCII-compliant and syntactically
#' valid names.
#' * Data formats for `date`, `weight`, `coord_x`, `coord_y`, and/or `herd_size`
#' columns are checked.
#' * For movement data files (`type == "movement"`): Dates in the `date` column
#' are converted to date format.
#' * For holding data files (`type == "holding"`): If `coord_x` and `coord_y`
#' columns are present, these are converted to a single simple feature (sf)
#' list-column named `"coordinates"`. Coordinates are converted from the `crs`
#' specified in the config file to ETRS89 (EPSG:4258).
#'
#' @details
#' If the movenet environment contains `movedata_cols` or `holdingdata_cols`
#' configurations stored as column indices (rather than column headers), calling
#' `reformat_data()` replaces these configuration values with the appropriate
#' column headers. A warning message is generated to indicate any such
#' configuration changes.
#'
#' @param datafile Path to a delimited file with movement or holding data.
#' @param type Character string representing the type of data contained in
#'   `datafile`: either `"movement"` or `"holding"`.
#'
#' @returns
#' A tibble with (a subset of) columns from `datafile`, reordered and
#' reformatted according to movenet format requirements.
#'
#' For movement data (`type == "movement"`), columns will include:
#' * `from` (character format).
#' * `to` (character format).
#' * `date` (date format).
#' * `weight` (double format).
#' * Any optional columns as indicated by the loaded `movedata_cols`
#' configurations (formats as determined by [readr::type_convert()]).
#'
#' For holding data (`type == "holding"`), columns will include:
#' * `id` (character format).
#' * Any optional columns as indicated by the loaded `holdingdata_cols`
#' configurations (formats as determined by [readr::type_convert()]).
#' * If the loaded configurations include `coord_x` and `coord_y`,
#' the returned tibble will instead include a single column named `"coordinates"`
#' (sf list-column, class sfc_POINT).
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a movement config file
#' load_config(system.file("configurations", "ScotEID.yml",
#'                         package = "movenet"))
#'
#' # Read in and reformat a movement data file
#' movement_data <-
#'   reformat_data(system.file("extdata", "fake_Scottish_movement_data.csv",
#'                             package = "movenet"),
#'                 type = "movement")
#' head(movement_data)
#'
#' # Load a holding config file
#' load_config(system.file("configurations", "fakeScotEID_holding.yml",
#'                         package = "movenet"))
#'
#' # Read in and reformat a holding data file
#' holding_data <-
#'   reformat_data(system.file("extdata", "fake_Scottish_holding_data.csv",
#'                            package = "movenet"),
#'                 type = "holding")
#' head(holding_data)
#'
#' # Clean-up: Reinstate previous configurations
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv", "movement_data", "holding_data")
#'
#' @seealso
#' * [asciify()] for the underlying ASCIIfication process.
#' * [`vignette("movenet")`] for getting started with movenet.
#' @family functions for initial data processing
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr has_element
#' @importFrom withr with_options
#' @import checkmate
#' @import readr
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

  if ((type == "movement" & !(has_element(names(movenetenv$options), "movedata_cols"))) |
      (type == "holding" & !(has_element(names(movenetenv$options), "holdingdata_cols")))){
    stop(paste0(
      "The loaded config file does not match the indicated type of data (",
      type, " data). Please ensure an appropriate config file is loaded."))
  }


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
    crs <- fileopts$coord_EPSG_code
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
    opt_prefix<-switch(type,
                       "movement" = "movedata_cols.",
                       "holding" = "holdingdata_cols.")
    #Add opt_prefix to name of options in minvar/extra for change_config to work
    opts_for_change_config <- c(minvars,extra) %>%
      purrr::set_names(paste0(opt_prefix, names(c(minvars,extra))))
    suppressWarnings(change_config(opts_for_change_config))
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
    if ("coord_x" %in% names(extra)){ #config validation ensures coord_x always comes with coord_y
      selected_data["coordinates"] <-
        reformat_coords(selected_data[extra$coord_x],
                        selected_data[extra$coord_y],
                        decimal, crs)
      selected_data[extra$coord_x] <- NULL
      selected_data[extra$coord_y] <- NULL
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

#' Convert character strings to unique, ASCII-compliant, and syntactically
#' valid column names
#'
#' `asciify()` replaces non-ASCII punctuation, symbols, and Latin letters with
#' approximate ASCII-range equivalents, and ensures that resulting character
#' strings are unique and syntactically valid column names.
#'
#' @details This helper function is used within [`load_config()`] and
#' [`reformat_data()`] to avoid potential issues with accents and other
#' non-ASCII symbols in data file column headers.
#'
#' @param x Character vector to be coerced to unique, ASCII-compliant, and
#' syntactically valid names. This is coerced to character if necessary.
#'
#' @returns Character vector of the same length as `x`, with each changed to a
#' unique, ASCII-compliant and syntactically valid name.
#'
#' @examples
#' asciify(c("Fr\u00E5nppn", "Avf\u00E4rdsdatum"))
#' asciify(c("a and b", "a-and-b"))
#'
#' @seealso
#' * [stringi::stri_trans_general()] and
#' \url{https://unicode-org.github.io/icu/userguide/transforms/general/#icu-transliterators} for the underlying process of making names ASCII-compliant.
#' * [make.names()] for the underlying process of making names syntactically valid and unique.
#' @family functions for initial data processing
#'
#' @export
#' @importFrom stringi stri_trans_general
asciify <- function(x){
  make.names(stringi::stri_trans_general(x, 'Latin-ASCII'),
             unique=TRUE)
}

################################################################################

#' Replace `cols` configurations stored as column indices with the relevant
#' column headers
#'
#' Internal helper function that replaces the values of `cols` configurations
#' that are stored as column indices within the movenet environment, with the
#' relevant column headers, using `data` as reference dataset. A warning
#' message is generated to indicate any such changes. The function also checks
#' that the provided column indices are within the column range of `data`, and
#' that the configurations do not correspond to duplicate columns.
#'
#' @param data A tibble with ALL columns of a movement or holding data file.
#' @param minvars A named list with all required `cols` configurations, as column indices.
#' @param extra A named list with all optional `cols` configurations set in the movenet environment, as column indices.
#'
#' @returns
#' A list of lists, consisting of:
#' * `minvars` (a named list with all required `cols` configurations) with values
#' converted to column headers.
#' * `extra` (a named list with all optional `cols` configurations set in the
#' movenet environment) with values converted to column headers. If any optional
#' column indices exceed `data`'s column range, these are ignored - these configuration(s)
#' will not feature in this list.
#'
#' Additionally, the function has a range of side effects:
#' * Changes are made in movenetenv, using [change_config()].
#' * A warning is generated, indicating any changes from column indices to column headers.
#' * A warning is generated if a column index for an optional configuration is out-of-range.
#' * An error is raised if a column index for a required configuration is
#' out-of-range, or if changes result in duplicate configuration values.
#'
#' @seealso `vignette("configurations")` for an explanation of the movenet config system.
#'
#' @keywords internal
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

#' Check the presence of required and additional columns, and extract from `data`
#'
#' Internal helper function that checks `data` for the presence of (required
#' and optional) columns represented in `cols` configurations. It then
#' extracts these columns from `data`.
#'
#' @param data A tibble with ALL columns of a movement or holding data file.
#' @param minvars A named list with all required `cols` configurations.
#' @param extra A named list with all optional `cols` configurations set in the movenet environment.
#'
#' @returns
#' A tibble with (a subset of) columns from `data`, reordered with required (`minvar`)
#' columns first, followed by optional (`extra`) columns.
#' If all indicated `cols` configurations are present in `data`, the overall
#' number of columns of the returned tibble is equal to `length(c(minvars, extra))`.
#' If any optional `cols` configurations are missing from `data`, these columns
#' are not represented in the returned tibble (which thus has fewer columns),
#' and a warning is generated.
#' An error is raised if any required `cols` configurations are missing from `data`.
#'
#' @keywords internal
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

#' Convert a column's data to double
#'
#' Internal helper function that parses the data in `numeric_col` as double, and
#' raises an error with an informative message if this does not work.
#'
#' @param numeric_col A one-column tibble containing data of type `character`,
#'   but assumed to be coercible to numeric.
#' @param decimal A single character representing the decimal mark.
#'
#' @returns If `numeric_col` contains numeric data, a double vector. Otherwise,
#'   an error is raised.
#'
#' @keywords internal
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

#' Convert a column's data to dates
#'
#' Internal helper function that parses the data in `date_col` as dates, using
#' the date format specification indicated by `date_format`, and raises an error
#' with an informative message if this does not work.
#'
#' @param date_col A one-column tibble containing data of type `character`,
#'   assumed to be coercible to dates.
#' @param date_format A character string with the date format specification used
#'   in `date_col`. See [readr::parse_date()] for guidance.
#'
#' @returns If `date_col` contains dates that follow the `date_format`
#'   specification, a date vector. Otherwise, an error is raised.
#'
#' @keywords internal
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

################################################################################

#' Convert two columns containing coordinates in character format to an sf geometry
#' object with CRS ETRS89
#'
#' Internal helper function that parses the data in `coord_x_col` and
#' `coord_y_col` as coordinates and converts them to an sf geometry list-column.
#' It then transforms the coordinates from the original crs to ETRS89.
#'
#' @param coord_x_col A one-column tibble containing data of type `character`,
#'   assumed to be coercible to numeric and representing a longitudinal coordinate.
#' @param coord_y_col A one-column tibble containing data of type `character`,
#'   assumed to be coercible to numeric and representing a latitudinal coordinate.
#' @param decimal A single character representing the decimal mark.
#' @param crs An integer with the numeric part of the EPSG code for the
#'   Coordinate Reference System used in `coord_x_col` and `coord_y_col`.
#'
#' @returns If `coord_x_col` and `coord_y_col` contain numeric data, a tibble
#' with a single sf geometry list-column (class sfc_POINT) with combined coordinates,
#' and attributes including the crs ETRS89. Otherwise, an error is raised.
#'
#' @importFrom sf st_as_sf st_transform
#' @importFrom tibble as_tibble
#'
#' @keywords internal
reformat_coords <- function(coord_x_col, coord_y_col, decimal, crs){

  #first reformat to numeric to ensure use of correct decimal marker
  coord_x_col[colnames(coord_x_col)] <- reformat_numeric(coord_x_col, decimal)
  coord_y_col[colnames(coord_y_col)] <- reformat_numeric(coord_y_col, decimal)

  st_as_sf(as_tibble(c(coord_x_col, coord_y_col)),
           coords = c(1,2),
           na.fail = FALSE,
           crs = crs) %>%
    st_transform(crs = 	4258)

}

