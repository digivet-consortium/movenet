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


internal_validate_config <- function(file){
  if (!file.exists(file)){
    stop(paste0(file, ": no such file exists"))
  }
  if (validate_yaml(file)$test == FALSE){
    invisible(validate_yaml(file)$msg)
  }else{
    yamlfile <- yaml.load_file(file)
    config_type <- sub("data", "", regmatches(names(yamlfile),regexpr("(move|holding)data",names(yamlfile)))[1])
    msg <- c(
      validate_config_root(yamlfile, config_type),
      validate_config_fileopts(yamlfile, config_type),
      validate_config_cols(yamlfile, config_type),
      validate_config_datatype(yamlfile, config_type)
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
  if (test==FALSE){
    msg <- paste(file,"is not valid yaml format")
  }
  list(test = test, msg = msg) #Does this need to be invisible?
}

validate_config_root <- function(yamlfile, config_type){
  if (!has_element(c("move","holding"),config_type)){
    return("Unexpected config file structure. Top-level keys must include either `movedata_fileopts` and `movedata_cols`, or `holdingdata_fileopts` and `holdingdata_cols`")
  } else {
    root_keys_obs <- names(yamlfile)
    if(config_type == "move"){root_keys_exp <- c("movedata_fileopts", "movedata_cols")} else {root_keys_exp <- c("holdingdata_fileopts", "holdingdata_cols")}
    root_valid <-  length(root_keys_obs) > 0 && all(root_keys_exp %in% root_keys_obs)
    if (!root_valid){
      sprintf("Unexpected config file structure. Missing mandatory top-level key(s): %s", paste0(root_keys_exp[!root_keys_exp %in% root_keys_obs],collapse=", "))
    }
  }  #Does this need to be invisible?
}


validate_config_fileopts <- function(yamlfile, config_type){
  if (has_element(c("move","holding"),config_type)){
    opts_keys_obs <- names(yamlfile[[paste0(config_type,"data_fileopts")]])
    if(config_type == "move"){opts_keys_exp <- c("separator", "encoding", "decimal", "date_format")} else {opts_keys_exp <- c("separator", "encoding", "decimal")}
    opts_notmissing <- length(opts_keys_obs) > 2 && all(opts_keys_exp %in% opts_keys_obs) #tests that required fileopts keys are present; but file may have more keys
    if (!opts_notmissing){
      paste0("Unexpected config file structure. Missing mandatory second-level (", config_type, "data_fileopts) key(s): ",paste0(opts_keys_exp[!opts_keys_exp %in% opts_keys_obs],collapse=", "))
    } #Does this need to be invisible?
  }
}

validate_config_cols <- function(yamlfile, config_type){
  if (has_element(c("move","holding"),config_type)){
    msg <- NULL
    cols_options_obs <- yamlfile[[paste0(config_type,"data_cols")]]
    cols_keys_obs <- names(cols_options_obs)
    if(config_type == "move"){cols_keys_exp <- c("from", "to", "date", "weight")} else {cols_keys_exp <- c("id")}
    cols_notmissing <- length(cols_keys_obs) > 0 && all(cols_keys_exp %in% cols_keys_obs) #tests that required cols keys are present; but file may have more keys
    cols_notdupl <- anyDuplicated(cols_options_obs) == 0
    if (!cols_notmissing){
      msg <- append(msg, paste0("Unexpected config file structure. Missing mandatory second-level (", config_type, "data_cols) key(s): ", paste0(cols_keys_exp[!cols_keys_exp %in% cols_keys_obs],collapse=", ")))
    }
    if (!cols_notdupl){
      msg <- append(msg, paste0("Values for ", config_type, "data_cols options must be unique. The following options have duplicate values: ", paste0(cols_keys_obs[which(cols_options_obs %in% cols_options_obs[duplicated(cols_options_obs)])],collapse=", ")))
    }
    msg #Does this need to be invisible?
  }
}

validate_config_datatype <- function(yamlfile, config_type){
  msg <- NULL
  fileopts <- yamlfile[[paste0(config_type,"data_fileopts")]]
  if(length(fileopts) > 0){
    if(config_type == "move"){ opts_char_exp <- fileopts }else{ opts_char_exp <- fileopts[which(names(fileopts)!="coord_EPSG_code")]}
    opts_char_obs <- sapply(opts_char_exp,is.character) #tests that appropriate fileopts option values are characters
    if (!all(opts_char_obs)){
      msg <- append(msg, sprintf("Data field(s) not in expected character format: %s", paste0(names(which(opts_char_obs==FALSE)),collapse=", ")))
    }
    if (!nchar(fileopts[["separator"]]) == 1){
      msg <- append(msg, "Data field `separator` doesn't have the expected format of a single character")
    }
    if (!nchar(fileopts[["decimal"]]) == 1){
      msg <- append(msg, "Data field `decimal` doesn't have the expected format of a single character")
    }
    if (has_element(names(fileopts),"date_format") && !grepl("%(Y|y|AD|D|F|x|s)|^$",fileopts[["date_format"]])){
      msg <- append(msg, paste0("Data field `date_format` doesn't match readr date format specifications.\nSee `?readr::parse_date` for guidance."))
    }
    if (has_element(names(fileopts),"coord_EPSG_code") && !is.integer(fileopts[["coord_EPSG_code"]])){
      msg <- append(msg, "Data field `coord_EPSG_code` not in expected integer format")
    }
  }
  cols_options <- yamlfile[[paste0(config_type,"data_cols")]]
  if(length(cols_options) > 0){
    cols_char <- sapply(cols_options, is.character) #tests that cols values are characters
    cols_int <- sapply(cols_options, is.integer) #tests that cols values are integers
    cols_charint <- (cols_char | cols_int)
    if (!all(cols_charint)){
      msg <- append(msg, sprintf("Data field(s) not in expected character or integer format: %s", paste0(names(which(cols_charint==FALSE)),collapse=", ")))
    }
  }
  msg #Does this need to be invisible?
}


