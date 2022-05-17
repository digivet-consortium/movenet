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
  failed_validation_messages = suppressMessages(internal_validate_config(file))
  if (is.null(failed_validation_messages)){
    invisible(TRUE)
  }
  else{
    stop(
      sprintf(paste(file,"is not a valid movenet config file\n%s"), paste0(failed_validation_messages, collapse="")),
      call. = FALSE
    )}
  }


internal_validate_config <- function(file){
  msg <- NULL
  suppressMessages(
    withCallingHandlers({
      if (validate_yaml(file) == TRUE){
        yamlfile <- yaml.load_file(file)
        validate_config_root(yamlfile)
        validate_config_move(yamlfile)
        #validate_config_holding(yamlfile)
      }},
      message = function(m) {
        msg <<- append(msg, conditionMessage(m))
        }
  ))
  invisible(msg)
}

#adapted from https://rdrr.io/cran/validate/src/R/yaml.R
validate_yaml <- function(file){
  out <- tryCatch(yaml.load_file(file),error = function(e) FALSE)
  test<-!identical(out,FALSE)
  if(test==FALSE){
    message(paste(file,"is not valid yaml format"))
  }
  invisible(test)
}

validate_config_root <- function(yamlfile){
  root_keys_obs <- names(yamlfile)
  root_keys_exp <- c("movement_data","holding_data")
  root_valid <-  length(root_keys_obs) > 1 && all(root_keys_exp %in% root_keys_obs)
  if (!root_valid){
    message(
      sprintf("Missing mandatory top-level keys: %s", paste0(root_keys_exp[!root_keys_exp %in% root_keys_obs],collapse=", "))
    )
  }
}

validate_config_move <- function(yamlfile){
  move_keys_obs <- names(yamlfile[["movement_data"]])
  move_keys_exp <- c("move_ID", "origin_ID", "dest_ID", "move_date", "nr_pigs")
  move_notmissing <- length(move_keys_obs) > 4 && all(move_keys_exp %in% move_keys_obs) #tests that required move keys are present; but file may have more keys
  move_allchar <- all(sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.character)) #tests that required & non-missing move values are all characters
  move_valid <- all(move_notmissing && move_allchar)
  if (!move_notmissing){
    message(
      sprintf("Missing mandatory movement_data keys: %s", paste0(move_keys_exp[!move_keys_exp %in% move_keys_obs],collapse=", "))
    )
  }
  if (!move_allchar){ #Needs adapting when holding data is incorporated
    message(
      sprintf("Data fields not in expected character format: %s", paste0(names(which(!sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.character))),collapse=", "))
    )
  }
}

#validate_config_holding <- function(yamlfile){
  #holding_keys_obs <- names(yamlfile[["holding_data"]])
  #holding_keys_exp <-  # ADD MANDATORY KEYS!
  #holding_notmissing <- length(move_keys_obs) > x && all(holding_keys_exp %in% holding_keys_obs) # SUBSTITUTE X!  tests that required holding keys are present; but file may have more keys

  #if (!holding_notmissing){
  #  message(
  #    sprintf("Missing mandatory holding_data keys: %s", paste0(holding_keys_exp[!holding_keys_exp %in% holding_keys_obs],collapse=", "))
  #  )
  #}
#}




