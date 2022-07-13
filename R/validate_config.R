#' @name validate_config
#' @title Validate a movenet config file
#'
#' @param file config file (yml format) to validate
#'
#' @return
#'
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
    msg <- c(
      validate_config_root(yamlfile),
      validate_config_moveopts(yamlfile),
      validate_config_movecols(yamlfile),
     #validate_config_holding(yamlfile),
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
  if (test==FALSE){
    msg <- paste(file,"is not valid yaml format")
  }
  list(test = test, msg = msg) #Does this need to be invisible?
}

validate_config_root <- function(yamlfile){
  root_keys_obs <- names(yamlfile)
  root_keys_exp <- c("movedata_fileopts", "movedata_cols") #"holding_data"
  root_valid <-  length(root_keys_obs) > 0 && all(root_keys_exp %in% root_keys_obs)
  if (!root_valid){
    sprintf("Unexpected config file structure. Missing mandatory top-level key(s): %s", paste0(root_keys_exp[!root_keys_exp %in% root_keys_obs],collapse=", "))
  } #Does this need to be invisible?

  #validate_config_root generates the missing message, when keys are present but not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated
}

validate_config_moveopts <- function(yamlfile){
  opts_keys_obs <- names(yamlfile[["movedata_fileopts"]])
  opts_keys_exp <- c("separator", "encoding", "decimal", "date_format")
  opts_notmissing <- length(opts_keys_obs) > 3 && all(opts_keys_exp %in% opts_keys_obs) #tests that required move keys are present; but file may have more keys
  if (!opts_notmissing){
    sprintf("Unexpected config file structure. Missing mandatory second-level (movedata_fileopts) key(s): %s", paste0(opts_keys_exp[!opts_keys_exp %in% opts_keys_obs],collapse=", "))
  } #Does this need to be invisible?

  #validate_config_moveopts generates the missing message, when keys are present but not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated
}

validate_config_movecols <- function(yamlfile){
  move_keys_obs <- names(yamlfile[["movedata_cols"]])
  move_keys_exp <- c("origin_ID", "dest_ID", "move_date", "nr_pigs")
  move_notmissing <- length(move_keys_obs) > 3 && all(move_keys_exp %in% move_keys_obs) #tests that required move keys are present; but file may have more keys
  if (!move_notmissing){
    sprintf("Unexpected config file structure. Missing mandatory second-level (movedata_cols) key(s): %s", paste0(move_keys_exp[!move_keys_exp %in% move_keys_obs],collapse=", "))
  } #Does this need to be invisible?

  #validate_config_movecols generates the missing message, when keys are present but not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated
}

#validate_config_holding <- function(yamlfile){
  #holding_keys_obs <- names(yamlfile[["holding_data"]])
  #holding_keys_exp <-  # ADD MANDATORY KEYS!
  #holding_notmissing <- length(move_keys_obs) > x && all(holding_keys_exp %in% holding_keys_obs) # SUBSTITUTE X!  tests that required holding keys are present; but file may have more keys
  #if (!holding_notmissing){
  #    sprintf("Unexpected config file structure. Missing mandatory second-level (holding_data) keys: %s", paste0(holding_keys_exp[!holding_keys_exp %in% holding_keys_obs],collapse=", "))
  #}
#}

validate_config_datatype <- function(yamlfile){
  msg <- NULL
  if(length(yamlfile[["movedata_fileopts"]]) > 0){
    opts_char <- sapply(yamlfile[["movedata_fileopts"]],is.character) #tests that moveopts values are characters
    if (!all(opts_char)){
      msg <- append(msg, sprintf("Data field(s) not in expected character format: %s", paste0(names(which(opts_char==FALSE)),collapse=", ")))
    }
    if (!nchar(yamlfile[["movedata_fileopts"]][["separator"]]) == 1){
      msg <- append(msg, "Data field `separator` doesn't have the expected format of a single character")
    }
    if (!nchar(yamlfile[["movedata_fileopts"]][["decimal"]]) == 1){
      msg <- append(msg, "Data field `decimal` doesn't have the expected format of a single character")
    }
    if(!grepl("%(Y|y|AD|D|F|x|s)|^$",yamlfile[["movedata_fileopts"]][["date_format"]])){
      msg <- append(msg,paste0("Data field `date_format` doesn't match readr date format specifications.\nSee `?readr::parse_datetime` for guidance."))
    }
  }
  if(length(yamlfile[["movedata_cols"]]) > 0){
    move_char <- sapply(yamlfile[["movedata_cols"]],is.character) #tests that movecol values are characters
    move_int <- sapply(yamlfile[["movedata_cols"]],is.integer) #tests that movecol values are integers
    move_charint <- (move_char | move_int)
    if (!all(move_charint)){
      msg <- append(msg, sprintf("Data field(s) not in expected character or integer format: %s", paste0(names(which(move_charint==FALSE)),collapse=", ")))
    }
  }
  # Add checks for holding_data data types
  msg #Does this need to be invisible?
}


