#' ---
#' title: testing yaml files
#' author: Carlijn Bogaardt
#' output: html_document
#' ---
#'
#'
# Wed 11 May 2022

#+ setup, message=FALSE
devtools::load_all()

infile<-system.file("configurations", paste0("ScotEID", ".yml"),package="movenet")
testempty<-system.file("configurations", paste0("ScotEID_testempty", ".yml"),package="movenet")

#adapted from https://rdrr.io/cran/validate/src/R/yaml.R
valid_yaml <- function(infile){
  yamlfile <- yaml.load_file(infile) #requires yaml package
  root_keys_obs <- names(yamlfile)
  root_keys_exp <- c("movement_data","holding_data")
  root_valid <-  length(root_keys_obs) > 1 && all(root_keys_exp %in% root_keys_obs) #tests move & holding data in yaml headings, file may have more

  move_keys_obs <- names(yamlfile[["movement_data"]])
  move_keys_exp <- c("move_ID", "origin_ID", "dest_ID", "dep_date", "arr_date", "nr_pigs")
  move_valid <- length(move_keys_obs) > 5 &&
    all(move_keys_exp %in% move_keys_obs) && #tests that required move keys are present; but file may have more keys
    #!(list(NULL) %in% yamlfile[["movement_data"]][move_keys_exp]) && #tests that required move values are not NULL (may need adapting for DK dates)
    all(sapply(yamlfile[["movement_data"]][move_keys_exp],is.character)) #tests that required move values are all characters (may need adapting for DK dates))

  #node_keys_obs <- names(yamlfile[["holding_data"]])
  #add tests here once clear what we want with holding data

  valid <- all(root_valid && move_valid) #add test for holding data here too

  return(valid)
}

#from https://rdrr.io/cran/validate/src/R/yaml.R
is_yaml_file <- function(infile){
  out <- tryCatch(yaml::yaml.load_file(infile),error = function(e) FALSE)
  !identical(out,FALSE)
}
