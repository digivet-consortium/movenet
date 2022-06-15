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
      validate_config_move(yamlfile),
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
  root_keys_exp <- c("movement_data") #"holding_data"
  root_valid <-  length(root_keys_obs) > 0 && all(root_keys_exp %in% root_keys_obs)
  if (!root_valid){
    sprintf("Unexpected config file structure. Missing mandatory top-level keys: %s", paste0(root_keys_exp[!root_keys_exp %in% root_keys_obs],collapse=", "))
  } #Does this need to be invisible?

  #validate_config_root generates the missing message, when keys are present but not at appropriate level.
  #Can change this behaviour if needed but this gets a bit complicated
}

validate_config_move <- function(yamlfile){
  move_keys_obs <- names(yamlfile[["movement_data"]])
  move_keys_exp <- c("movenet.origin_ID", "movenet.dest_ID", "movenet.move_date", "movenet.nr_pigs")
  move_notmissing <- length(move_keys_obs) > 3 && all(move_keys_exp %in% move_keys_obs) #tests that required move keys are present; but file may have more keys
  if (!move_notmissing){
    sprintf("Unexpected config file structure. Missing mandatory second-level (movement_data) keys: %s", paste0(move_keys_exp[!move_keys_exp %in% move_keys_obs],collapse=", "))
  } #Does this need to be invisible?

  #validate_config_move generates the missing message, when keys are present but not at appropriate level.
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
  move_keys_obs <- names(yamlfile[["movement_data"]])
  move_keys_exp <- c("movenet.origin_ID", "movenet.dest_ID", "movenet.move_date", "movenet.nr_pigs")
  move_char <- sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.character) #tests that required & non-missing move values are characters
  move_int <- sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.integer) #tests that required & non-missing move values are integers
  move_charint <- (move_char | move_int)
  msg <- NULL
  if (!all(move_charint)){
    msg <- append(msg, sprintf("Data fields not in expected character or integer format: %s", paste0(names(which(move_charint==FALSE)),collapse=", ")))
  }
  # Add checks for holding_data data types
  msg #Does this need to be invisible?
}


