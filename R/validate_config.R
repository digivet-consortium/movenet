#' @name validate_config
#' @title Validate a movenet config file
#'
#' @param file config file (yml format) to validate
#'
#' @return
#'
#' @importFrom purrr has_element
#' @importFrom yaml yaml.load_file
#'
#' @examples
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

#adapted from https://rdrr.io/cran/validate/src/R/yaml.R
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

infer_config_type <- function(yamlfile){
  unique(regmatches(names(yamlfile), regexpr("move|holding", names(yamlfile)))) %>%
    sub(pattern = "move", replacement = "movement", x = ., fixed = TRUE)
}

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
  valid_move_config_msg <- " Valid movement config file."
  valid_holding_config_msg <- " Valid holding config file."
  print_missing_move_msg <- length(missing_move_keys) == 1
  print_missing_holding_msg <- length(missing_holding_keys) == 1
  root_valid <- !(length(missing_move_keys) == 2 && length(missing_holding_keys) == 2) && isFALSE(print_missing_move_msg) && isFALSE(print_missing_holding_msg)
  if (!root_valid){
    paste0("Unexpected config file structure. Top-level keys must include `movedata_fileopts` and `movedata_cols`, and/or `holdingdata_fileopts` and `holdingdata_cols`.",
          valid_move_config_msg[valid_move_config], valid_holding_config_msg[valid_holding_config],
          missing_move_keys_msg[print_missing_move_msg], missing_holding_keys_msg[print_missing_holding_msg])
  }
}

#Validate_config_type() tests whether the provided config type matches the actual config type
#Doesn't explicitly check all combinations, and inferred_config_type doesn't mean it's valid - that's what validate_config_root etc. are for.
validate_config_type <- function(provided_config_type, inferred_config_type){
  if (!is.null(provided_config_type) && provided_config_type == "movement" && inferred_config_type == "holding") {
    return("Unexpected config file type. File must be a movement config file, not a holding config file")
  } else if (!is.null(provided_config_type) && provided_config_type == c("movement", "holding") && inferred_config_type == "movement") {
    return("Unexpected config file type. File is a movement config file, not a combined config file")
  } else if (!is.null(provided_config_type) && provided_config_type == "holding" && inferred_config_type == "movement") {
    return("Unexpected config file type. File must be a holding config file, not a movement config file")
  } else if (!is.null(provided_config_type) && provided_config_type == c("movement", "holding") && inferred_config_type == "holding") {
    return("Unexpected config file type. File is a holding config file, not a combined config file")
  } else { return(NULL) }
}

#' @importFrom magrittr extract
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

#' @importFrom magrittr extract
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

validate_config_datatype <- function(yamlfile){
  msg <- NULL
  fileopts <- unlist(yamlfile[grep("(move|holding)data_fileopts",names(yamlfile), value = TRUE)], recursive = FALSE)
  if(length(fileopts) > 0){
    opts_char_exp <- fileopts[which(names(fileopts)!="holdingdata_fileopts.coord_EPSG_code")]
    opts_char_obs <- sapply(opts_char_exp,is.character) #tests that appropriate fileopts option values are characters
    if (!all(opts_char_obs)){
      msg <- append(msg, sprintf("Data field(s) not in expected character format: %s", paste0(names(which(isFALSE(opts_char_obs))),collapse=", ")))
    }
    opts_singlechar_exp <- c("movedata_fileopts.separator","holdingdata_fileopts.separator",
                             "movedata_fileopts.decimal","holdingdata_fileopts.decimal")
    if (any(sapply(opts_singlechar_exp,function(x){all(x %in% names(fileopts) && nchar(fileopts[x]) != 1)}))){
      nonsinglechar_names <- opts_singlechar_exp[which(opts_singlechar_exp %in% names(fileopts) & nchar(fileopts[opts_singlechar_exp]) != 1)]
      msg <- append(msg, sprintf("Data field(s) do not have the expected format of a single character: %s", paste(nonsinglechar_names, collapse=", ")))
    }
    if (has_element(names(fileopts),"holdingdata_fileopts.country_code") && nchar(fileopts["holdingdata_fileopts.country_code"]) != 2){
      msg <- append(msg, "Data field `holdingdata_fileopts.country_code` doesn't have the expected format of two characters")
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
      msg <- append(msg, sprintf("Data field(s) not in expected character or integer format: %s", paste0(names(which(isFALSE(cols_charint))),collapse=", ")))
    }
  }
  msg
}


