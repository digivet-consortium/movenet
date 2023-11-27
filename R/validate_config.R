#' @title Validate a movenet config file
#'
#' @description
#' `validate_config()` validates that `file` is a valid movenet config file, by checking that:
#' * `file` can be parsed as a YAML string.
#' * All required top-level, `fileopts`, and `cols` configuration keys are present in `file`.
#' * All configuration values follow data format requirements.
#'
#' @details
#' ## Config file types
#' There are three types of config files, each with slightly different requirements:
#' * Movement config files, with information on how to read movement data files. Configuration groupings in these files are prefixed with `"movedata_"`.
#' * Holding config files, with information on how to read holding data files. Configuration groupings in these files are prefixed with `"holdingdata_"`.
#' * Combined config files, containing both `"movedata_"` and `"holdingdata_"` configuration groupings. These need to fulfill criteria for both movement and holding config files.
#'
#' ## Top-level configuration key requirements
#' Config files must contain each of two types of configuration groupings:
#' * File and data options that commonly vary between datasets. Indicated by top-level key `movedata_fileopts` or `holdingdata_fileopts`.
#' * Column headers/indices for data fields that are either required or that you want to extract along for analyses. Indicated by top-level key `movedata_cols` or `holdingdata_cols`.
#'
#' ## `fileopts` configurations and data format requirements
#' Within each `fileopts` grouping, the following configurations are required, with the value taking the indicated format:
#' * `separator`: Separator (or delimiter) used in the data file. Format: Single character.
#' * `decimal`: Decimal mark used in the data file. Format: Single character.
#' * `encoding`: Encoding used in the data file. Format: Character string.
#'
#' Additionally, required for `movedata_fileopts`:
#' * `date_format`: Date format specification used in the movement data file. Format: Character string matching readr date format specifications. See [readr::parse_date()] for
#' guidance.
#'
#' Additionally, required for `holdingdata_fileopts`, if geographical coordinates are to be included in data extraction:
#' * `country_code`: Two-letter country code for the data in the holding data file. Format: Character string consisting of two upper-case letters.
#' * `coord_EPSG_code`: Numeric part of the EPSG code for the Coordinate Reference System used in the holding data file. Format: Single integer.
#'
#' ## `cols` configurations and data format requirements
#' Within `movedata_cols` groupings, the following configurations are required:
#' * `from`: Header/index of the column containing identifiers of the holdings of origin.
#' * `to`: Header/index of the column containing identifiers of the destination holdings.
#' * `date`: Header/index of the column containing movement dates.
#' * `weight`: Header/index of the column containing movement weights/quantities.
#'
#' For `holdingdata_cols`, there is only one required configuration:
#' * `id`: Header/index of the column containing holding identifiers (matching to `from`/`to` in movement data).
#'
#' Additionally, for inclusion of geographical coordinates, `holdingdata_cols` requires the following configurations:
#' * `coord_x`: Header/index of the column containing longitudinal geographical coordinates (x/longitude) of holdings
#' * `coord_y`: Header/index of the column containing latitudinal geographical coordinates (y/latitude) of holdings
#'
#' All `cols` configuration values must either be a character string or a single integer.
#' All configurations within a `cols` grouping must have unique values.
#'
#' @param file Path of the config file to validate.
#'
#' @returns
#' * If the provided config file is valid, `TRUE` (returned invisibly).
#' * If the provided config file is not valid, an error is raised, with a message
#' indicating the reason(s) validation has failed.
#'
#' @examples
#' # Validate a valid config file
#' validate_config(system.file("configurations", "fakeScotEID_holding.yml", package="movenet"))
#'
#' # Validate an invalid config file (empty template)
#' validate_config(system.file("configurations", "holdingconfig_template.yml", package="movenet"))
#'
#' @seealso
#' * `vignette("configurations")` for an explanation of the movenet config system.
#' * `list.files(system.file("configurations", package = "movenet"))` for a list of list of pre-installed templates and validated config files.
#' @family configurations-related functions
#'
#' @export
validate_config <- function(file){
  if (!file.exists(file)){
    stop(paste0(file, ": no such file exists"))
  }
  failed_validation_messages = internal_validate_config(file)
  if (is.null(failed_validation_messages)){
    invisible(TRUE)
  }
  else{
    stop(
      sprintf(paste(file,"is not a valid movenet config file\n%s"), paste0(failed_validation_messages, collapse="\n"))
    )}
  }


#' Validate a movenet config file and assemble "failed validation" messages
#'
#' Internal helper function that runs a variety of validation functions on `file`,
#' and collects and assembles any failed validation messages for [validate_config()].
#' `internal_validate_config()` validates that:
#' * `file` can be parsed as a YAML string.
#' * Any required/expected `config_type` is covered by the actual config type of `file`.
#' * All required top-level, "fileopts", and "cols" configuration keys are present in `file`.
#' * All configuration values match their data format requirements.
#'
#' @details
#' The movenet app uses this internal function instead of the external
#' [validate_config()] function, to avoid raising app-crashing errors.
#'
#' This internal function contains the `config_type` argument, whereas the
#' external [`validate_config()`] function doesn't. This is because in the movenet
#' app, the movement and holding config files are provided via different pages,
#' requiring validation that the file's actual config type corresponds to the
#' config type of the app page. No such validation is required for standard
#' usage of [`validate_config()`]via the package.
#'
#'
#' @param file Path of the config file to validate.
#' @param config_type The type of config file to expect: one of `NULL` (default,
#'   resulting in inference of config type), `c("movement", "holding")`
#'   (indicating a combined config file), `"movement"` or `"holding"`. Within the
#'   movenet app this is used to indicate the `config_type` required by the app page.
#'
#' @returns
#' * If the provided config file is valid, `NULL` (returned invisibly).
#' * If the provided config file is not valid, a character vector (returned invisibly)
#' with one or more elements that each indicate a reason why validation has failed.
#'
#' @examples
#'
#' @seealso
#' * `vignette("configurations")` for an explanation of the movenet config system.
#' * `list.files(system.file("configurations", package = "movenet"))` for a list of pre-installed, validated config files.
#'
#' @importFrom yaml yaml.load_file
#' @keywords internal
internal_validate_config <- function(file, config_type = NULL){
  if (!file.exists(file)){
    stop(paste0(file, ": no such file exists"))
  }
  if (isFALSE(validate_yaml(file)$test)){
    invisible(validate_yaml(file)$msg)
  } else {
    yamlfile <- yaml.load_file(file)
    inferred_config_type <- infer_config_type(yamlfile)
    if(is.null(config_type)){ config_type_to_use <- inferred_config_type
      } else { config_type_to_use <- config_type }
    msg <- c(
      validate_config_root(yamlfile),
      validate_config_type(config_type, inferred_config_type),
      validate_config_fileopts(yamlfile, config_type_to_use),
      validate_config_cols(yamlfile, config_type_to_use),
      validate_config_datatype(yamlfile)
    )
    invisible(msg)
  }
}



#' Validate that a file can be parsed as a YAML string
#'
#' Internal helper function that checks whether `file` has YAML format.
#'
#' @details Adapted from https://rdrr.io/cran/validate/src/R/yaml.R
#'
#' @param file Path of the file to validate.
#'
#' @returns A named list consisting of 2 elements:
#' * `test`: A boolean, whether `file` contains a valid YAML string that can be read by yaml.load_file.
#' * `msg`: Either `NULL` if `isTRUE(test)`, or a character string stating that `file` is not valid YAML format.
#'
#' @importFrom yaml yaml.load_file
#' @keywords internal
validate_yaml <- function(file){
  if (!file.exists(file)){
    stop(paste0(file, ": no such file exists"))
  }
  out <- tryCatch(yaml.load_file(file),error = function(e) FALSE)
  test <- !identical(out,FALSE)
  msg <- NULL
  if (isFALSE(test)){
    msg <- paste(file,"is not valid yaml format")
  }
  list(test = test, msg = msg) #Does this need to be invisible?
}

#' Infer the config type of a parsed yaml file
#'
#' Internal helper function that detects any occurrence of `"move"` or `"holding"`
#' among `yamlfile`'s names, and infers from this the potential config type.
#'
#' @param yamlfile Named list, assumed to be a parsed yaml-format config file.
#'
#' @returns Character vector representing config type. One of `"movement"`,
#' `"holding"`, or `c("movement", "holding")`.
#'
#' @keywords internal
#' @importFrom stringr str_sort
infer_config_type <- function(yamlfile){
  unique(regmatches(names(yamlfile), regexpr("move|holding", names(yamlfile)))) %>%
    sub(pattern = "move", replacement = "movement", x = ., fixed = TRUE) %>%
    str_sort(decreasing = TRUE)
}

#' Validate that top-level config keys are present in a parsed yaml file
#'
#' Internal helper function that checks whether all movement and/or holding
#' top-level config keys are present in `yamlfile`, and prints a character string
#' containing a failed validation message in case any such keys are missing.
#'
#' @details
#' Required top-level config keys are as follows:
#' * `movedata_fileopts` and `movedata_cols` for movement config files.
#' * `holdingdata_fileopts` and `holdingdata_cols` for holding config files.
#' * All of the above four top-level config keys for combined config files.
#'
#' @param yamlfile Named list, assumed to be a parsed yaml-format config file.
#'
#' @returns
#' Either `NULL` if all movement and/or holding top-level config keys are present,
#' or a character string stating that `file` has an unexpected structure and which
#' top-level config keys are missing.
#'
#' @keywords internal
validate_config_root <- function(yamlfile){

  root_keys_obs <- names(yamlfile)
  move_root_keys_exp <- c("movedata_fileopts", "movedata_cols")
  holding_root_keys_exp <- c("holdingdata_fileopts", "holdingdata_cols")
  missing_move_keys <- move_root_keys_exp[which(!(move_root_keys_exp %in% root_keys_obs))]
  missing_holding_keys <- holding_root_keys_exp[which(!(holding_root_keys_exp %in% root_keys_obs))]
  missing_move_keys_msg <- sprintf(" Missing top-level movement config key(s): %s.", paste0(missing_move_keys,collapse=", "))
  missing_holding_keys_msg <- sprintf(" Missing top-level holding config key(s): %s.", paste0(missing_holding_keys,collapse=", "))
  valid_move_config <- length(missing_move_keys) == 0
  valid_holding_config <- length(missing_holding_keys) == 0
  valid_move_config_msg <- " Valid top-level movement config structure."
  valid_holding_config_msg <- " Valid top-level holding config structure."
  print_missing_move_msg <- length(missing_move_keys) == 1
  print_missing_holding_msg <- length(missing_holding_keys) == 1
  root_valid <- !(length(missing_move_keys) == 2 && length(missing_holding_keys) == 2) && isFALSE(print_missing_move_msg) && isFALSE(print_missing_holding_msg)
  if (!root_valid){
    paste0("Unexpected config file structure. Top-level keys must include `movedata_fileopts` and `movedata_cols`, and/or `holdingdata_fileopts` and `holdingdata_cols`.",
          valid_move_config_msg[valid_move_config], valid_holding_config_msg[valid_holding_config],
          missing_move_keys_msg[print_missing_move_msg], missing_holding_keys_msg[print_missing_holding_msg])
  }
}

#' Validate that the required config type is covered by the inferred config type
#'
#' Internal helper function that checks whether the required config type
#' corresponds to or is a subset of the inferred config type.
#'
#' @details This is used by the movenet app to ensure that the config file uploaded
#' in the "movement data input" page contains movement configurations, and that the
#' config file uploaded in the "holding data input" page contains holding configurations.
#'
#' N.B. `validate_config_type()` does not check config file validity, only whether
#' inferred and required config types correspond.
#'
#' @param required_config_type Character vector representing the type of
#'  configurations that are required. One of `"movement"`, `"holding"`, or
#'  `c("movement", "holding")` (containing both types of configurations). Within
#'  the movenet app this is used to indicate the config type expected by the app page.
#' @param inferred_config_type Character vector representing the type of
#'  configurations inferred to be present in the config file. One of `"movement"`,
#'  `"holding"`, or `c("movement", "holding")` (containing both types of
#'  configurations).
#'
#' @returns
#' Either `NULL` if the required config type corresponds to or is a subset of the
#' inferred config type, or a character string stating that `file` has an unexpected config type.
#'
#' @keywords internal
validate_config_type <- function(required_config_type, inferred_config_type){
  if (!is.null(required_config_type) && required_config_type == "movement" && inferred_config_type == "holding") {
    return("Unexpected config file type. File must be a movement config file, not a holding config file")
  } else if (!is.null(required_config_type) && all(required_config_type == c("movement", "holding")) && inferred_config_type == "movement") {
    return("Unexpected config file type. File is a movement config file, not a combined config file")
  } else if (!is.null(required_config_type) && required_config_type == "holding" && inferred_config_type == "movement") {
    return("Unexpected config file type. File must be a holding config file, not a movement config file")
  } else if (!is.null(required_config_type) && all(required_config_type == c("movement", "holding")) && inferred_config_type == "holding") {
    return("Unexpected config file type. File is a holding config file, not a combined config file")
  } else { return(NULL) }
}

#' Validate that required "fileopts" keys are present in a parsed yaml file
#'
#' Internal helper function that checks:
#' * Whether all "fileopts" (file options) keys required for `config_type` are present in `yamlfile`.
#' * In case any geographical coordinate-related "cols" configurations (`holdingdata_cols.coord_x`
#'  and `holdingdata_cols.coord_y`) are included, that all "fileopts" keys required
#'  for geographic data are present in `yamlfile`.
#' A character string containing a failed validation message is printed in case
#' any keys are missing.
#'
#' @details
#' Required "fileopts" keys for `config_type = "movement"` are as follows:
#' * `movedata_fileopt.separator`
#' * `movedata_fileopts.decimal`
#' * `movedata_fileopts.encoding`
#' * `movedata_fileopts.date_format`
#'
#' Required "fileopts" keys for `config_type = "holding"` are as follows:
#' * `holdingdata_fileopt.separator`
#' * `holdingdata_fileopts.decimal`
#' * `holdingdata_fileopts.encoding`
#' * For the inclusion of geographical coordinates in any data extraction and
#' analysis, both `holdingdata_fileopts.country_code` and
#' `holdingdata_fileopts.coord_EPSG_code` are required.
#'
#' @param yamlfile Named list, assumed to be a parsed yaml-format config file.
#' @param config_type Character vector representing the type of configurations
#'   for which to check the "fileopts" keys: one of `c("movement", "holding")`,
#'   `"movement"` or `"holding"`.
#'
#' @returns
#' Either `NULL` if all "fileopts" (file options) keys required for `config_type`
#' are present, or a character string stating that `file` has an unexpected
#' structure and which "fileopts" keys are missing.
#'
#' @importFrom purrr has_element
#' @importFrom magrittr extract
#' @keywords internal
validate_config_fileopts <- function(yamlfile, config_type){
  if (has_element(list("movement", "holding", c("movement", "holding")), config_type)){
    msg <- NULL
    opts_keys_obs <- names(unlist(yamlfile[grep("(move|holding)data_fileopts",names(yamlfile), value = TRUE)], recursive = FALSE))
    opts_keys_exp <- list(movement = c("movedata_fileopts.separator", "movedata_fileopts.encoding", "movedata_fileopts.decimal", "movedata_fileopts.date_format"),
                          holding = c("holdingdata_fileopts.separator", "holdingdata_fileopts.encoding", "holdingdata_fileopts.decimal")) %>%
      extract(config_type) %>%
      unlist(use.names = FALSE)
    opts_notmissing <- length(opts_keys_obs) > 2 && all(opts_keys_exp %in% opts_keys_obs) #tests that required fileopts keys are present; but file may have more keys
    if (!opts_notmissing){
      msg <- append(msg, paste0("Unexpected config file structure. Missing mandatory second-level key(s): ",paste0(opts_keys_exp[!opts_keys_exp %in% opts_keys_obs],collapse=", ")))
    } #Does this need to be invisible?
    geo_opts <- c("holdingdata_fileopts.coord_EPSG_code","holdingdata_fileopts.country_code")
    geo_opts_missing <- (!geo_opts %in% opts_keys_obs)
    if("holding" %in% config_type && any(geo_opts_missing) && (any(c("coord_x","coord_y") %in% names(yamlfile[["holdingdata_cols"]])))){
      msg <- append(msg, paste0("Unexpected config file structure. Missing key(s) required for inclusion of columns with geographical coordinates: ",paste0(geo_opts[geo_opts_missing],collapse=", ")))
    }
    msg
  }
}

#' Validate that required "cols" keys are present in a parsed yaml file
#'
#' Internal helper function that checks:
#' * Whether all "cols" (column header/index) keys required for `config_type` are present in `yamlfile`.
#' * Whether all configurations within `movedata_cols` or `holdingdata_cols` have unique values.
#' * In case any geographical coordinate-related "cols" configurations (`holdingdata_cols.coord_x`
#'  and `holdingdata_cols.coord_y`) are included, that both of these are present.
#' A character string containing a failed validation message is printed in case
#' any keys are missing or any values are duplicated.
#'
#' @details
#' Required "cols" keys for `config_type = "movement"` are as follows:
#' * `movedata_cols.from`
#' * `movedata_cols.to`
#' * `movedata_cols.date`
#' * `movedata_cols.weight`
#'
#' Required "cols" keys for `config_type = "holding"` are as follows:
#' * `holdingdata_cols.id`
#' * For the inclusion of geographical coordinates in any data extraction and
#' analysis, both `holdingdata_cols.coord_x` and `holdingdata_cols.coord_y` are required.
#'
#' All configurations within `movedata_cols` or `holdingdata_cols` must have unique values.
#'
#' @param yamlfile Named list, assumed to be a parsed yaml-format config file.
#' @param config_type Character vector representing the type of configurations
#'   for which to check the "cols" keys: one of `c("movement", "holding")`,
#'   `"movement"` or `"holding"`.
#'
#' @returns
#' Either `NULL` if all "cols" (column header/index) keys required for `config_type`
#' are present, or a character string stating that `file` has an unexpected
#' structure and which "cols" keys are missing.
#'
#' @importFrom purrr has_element
#' @importFrom magrittr extract
#' @keywords internal
validate_config_cols <- function(yamlfile, config_type){
  if (has_element(list("movement", "holding", c("movement", "holding")), config_type)){
    msg <- NULL
    cols_options_obs <- unlist(yamlfile[grep("(move|holding)data_cols",names(yamlfile), value = TRUE)], recursive = FALSE)
    cols_keys_obs <- names(cols_options_obs)
    cols_keys_exp <-
      list(movement = c("movedata_cols.from", "movedata_cols.to", "movedata_cols.date", "movedata_cols.weight"),
           holding = "holdingdata_cols.id") %>%
      extract(config_type) %>%
      unlist(use.names = FALSE)
    cols_notmissing <- length(cols_keys_obs) > 0 && all(cols_keys_exp %in% cols_keys_obs) #tests that required cols keys are present; but file may have more keys
    if (!cols_notmissing){
      msg <- append(msg, paste0("Unexpected config file structure. Missing mandatory second-level key(s): ", paste0(cols_keys_exp[!cols_keys_exp %in% cols_keys_obs],collapse=", ")))
    }
    movecols_options_obs <- cols_options_obs[startsWith(names(cols_options_obs),"movedata_cols")]
    holdingcols_options_obs <- cols_options_obs[startsWith(names(cols_options_obs),"holdingdata_cols")]
    movecols_notdupl <- anyDuplicated(movecols_options_obs) == 0
    holdingcols_notdupl <- anyDuplicated(holdingcols_options_obs) == 0
    if (!movecols_notdupl){
      msg <- append(msg, paste0("Values for movedata_cols options must be unique. The following options have duplicate values: ",
                                paste0(names(movecols_options_obs)[which(movecols_options_obs %in% movecols_options_obs[duplicated(movecols_options_obs)])],collapse=", ")))
    }
    if (!holdingcols_notdupl){
      msg <- append(msg, paste0("Values for holdingdata_cols options must be unique. The following options have duplicate values: ",
                                paste0(names(holdingcols_options_obs)[which(holdingcols_options_obs %in% holdingcols_options_obs[duplicated(holdingcols_options_obs)])],collapse=", ")))
    }
    coord_present <- c("holdingdata_cols.coord_x","holdingdata_cols.coord_y") %in% cols_keys_obs
    if ("holding" %in% config_type && any(coord_present) & any(!coord_present)){
      coord_missing <- c("holdingdata_cols.coord_x","holdingdata_cols.coord_y")[which(!coord_present)]
      msg <- append(msg, paste0("Unexpected config file structure. Missing key required for inclusion of columns with geographical coordinates: ",coord_missing))
    }
    msg
  }
}

#' Validate that all configurations match their required data format
#'
#' Internal helper function that checks whether the values of all configurations
#' in `yamlfile` have their required data format, and prints a character string
#' containing a failed validation message if this is not the case.
#'
#' @details
#' Valid configuration data formats are as follows:
#' * `(move|holding)data_fileopts.separator`: A single character.
#' * `(move|holding)data_fileopts.decimal`: A single character.
#' * `(move|holding)data_fileopts.encoding`: A character string.
#' * `(move|holding)data_fileopts.date_format`: A character string matching readr date format specifications. See [readr::parse_date()] for
#' guidance.
#' * `holdingdata_fileopts.country_code`: A character string consisting of two upper-case letters.
#' * `holdingdata_fileopts.coord_EPSG_code`: A single integer.
#' * `(move|holding)data_cols` configurations: A character string or a single integer.
#'
#' @param yamlfile Named list, assumed to be a parsed yaml-format config file.
#'
#' @returns
#' Either `NULL` if all configuration values have the required data formats, or a
#' character string stating which configurations have an unexpected data format.
#'
#' @importFrom purrr has_element
#' @keywords internal
validate_config_datatype <- function(yamlfile){
  msg <- NULL
  fileopts <- unlist(yamlfile[grep("(move|holding)data_fileopts",names(yamlfile), value = TRUE)], recursive = FALSE)
  if(length(fileopts) > 0){
    opts_char_exp <- fileopts[which(names(fileopts)!="holdingdata_fileopts.coord_EPSG_code")]
    opts_char_obs <- sapply(opts_char_exp,is.character) #tests that appropriate fileopts option values are characters
    if (!all(opts_char_obs)){
      msg <- append(msg, sprintf("Data field(s) not in expected character format: %s", paste0(names(which(opts_char_obs == FALSE)),collapse=", ")))
    }
    opts_singlechar_exp <- c("movedata_fileopts.separator","holdingdata_fileopts.separator",
                             "movedata_fileopts.decimal","holdingdata_fileopts.decimal")
    if (any(sapply(opts_singlechar_exp,function(x){all(x %in% names(fileopts) && nchar(fileopts[x]) != 1)}))){
      nonsinglechar_names <- opts_singlechar_exp[which(opts_singlechar_exp %in% names(fileopts) & nchar(fileopts[opts_singlechar_exp]) != 1)]
      msg <- append(msg, sprintf("Data field(s) do not have the expected format of a single character: %s", paste(nonsinglechar_names, collapse=", ")))
    }
    if (has_element(names(fileopts),"holdingdata_fileopts.country_code") && !is_valid_country_code(fileopts["holdingdata_fileopts.country_code"])){
      msg <- append(msg, "Data field `holdingdata_fileopts.country_code` doesn't have the expected format of two uppercase letters")
    }
    opts_dateformat <- c("movedata_fileopts.date_format","holdingdata_fileopts.date_format")
    if (any(sapply(opts_dateformat,function(x){all(x %in% names(fileopts) && !is_valid_date_format(fileopts[x]))}))){
      wrongdateformat_names <- opts_dateformat[which(opts_dateformat %in% names(fileopts) & !is_valid_date_format(fileopts[opts_dateformat]))]
      msg <- append(msg, sprintf("Data field(s) do not match readr date format specifications: %s. See `?readr::parse_date` for guidance.",
                                 paste(wrongdateformat_names, collapse=", ")))
    }
    if (has_element(names(fileopts),"holdingdata_fileopts.coord_EPSG_code") && !is.integer(fileopts[["holdingdata_fileopts.coord_EPSG_code"]])){
      msg <- append(msg, "Data field `holdingdata_fileopts.coord_EPSG_code` not in expected integer format")
    }
  }
  cols_options <- unlist(yamlfile[grep("(move|holding)data_cols",names(yamlfile), value = TRUE)], recursive = FALSE)
  if(length(cols_options) > 0){
    cols_char <- sapply(cols_options, is.character) #tests that cols values are characters
    cols_int <- sapply(cols_options, is.integer) #tests that cols values are integers
    cols_charint <- (cols_char | cols_int)
    if (!all(cols_charint)){
      msg <- append(msg, sprintf("Data field(s) not in expected character or integer format: %s", paste0(names(which(cols_charint == FALSE)),collapse=", ")))
    }
  }
  msg
}


