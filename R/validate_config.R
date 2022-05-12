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
  if (is_yaml_file(file)==FALSE){
    return(FALSE)
  }
  else{
    test_config_structure(file)
  }
}

test_config_structure <- function(file){
  yamlfile <- yaml.load_file(file) #requires yaml package
  root_keys_obs <- names(yamlfile)
  root_keys_exp <- c("movement_data","holding_data")
  root_valid <-  length(root_keys_obs) > 1 && all(root_keys_exp %in% root_keys_obs) #tests move & holding data in yaml headings, file may have more

  if (!root_valid){
    warning(
      sprintf("Missing top-level keys: %s\n", paste0(root_keys_exp[!root_keys_exp %in% root_keys_obs],collapse=", "))
    )
  }

  if (root_valid){

    move_keys_obs <- names(yamlfile[["movement_data"]])
    move_keys_exp <- c("move_ID", "origin_ID", "dest_ID", "dep_date", "arr_date", "nr_pigs")
    move_notmissing <- length(move_keys_obs) > 5 && all(move_keys_exp %in% move_keys_obs) #tests that required move keys are present; but file may have more keys
    move_allchar <- all(sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.character)) #tests that required & non-missing move values are all characters (may need adapting for DK dates))
    move_valid <- all(move_notmissing && move_allchar)

    #node_keys_obs <- names(yamlfile[["holding_data"]])
    #add tests here once clear what we want with holding data

    if (!move_notmissing){
      warning(
        sprintf("Missing movement_data keys: %s\n", paste0(move_keys_exp[!move_keys_exp %in% move_keys_obs],collapse=", "))
      )
    }
    #if (!node_notmissing){
    #  warning(
    #    sprintf("Missing holding_data keys: %s\n", paste0(node_keys_exp[!node_keys_exp %in% node_keys_obs],collapse=", "))
    #  )
    #}
    if (!move_allchar){ #Needs adapting when holding data is incorporated
      warning(
        sprintf("Data fields not in expected character format: %s\n", paste0(names(which(!sapply(yamlfile[["movement_data"]][move_keys_obs %in% move_keys_exp],is.character))),collapse=", "))
      )
    }

  }

  valid <- all(root_valid && move_valid) #add test for holding data here too

  return(valid)
}

#adapted from https://rdrr.io/cran/validate/src/R/yaml.R
is_yaml_file <- function(file){
  out <- tryCatch(yaml.load_file(file),error = function(e) FALSE)
  test<-!identical(out,FALSE)
  if(test==FALSE){
    warning("Input file not valid yaml format", call.=FALSE)
  }
  return(test)
}
#Accepts lots of formats that can be read as yaml (e.g. .md or .R files)
#When faced with e.g. pdf, it prints warnings that aren't helpful. How do I silence these?