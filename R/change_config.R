#' @name change_config
#' @title Change configurations
#'
#' @param country some description
#'
#' @importFrom yaml yaml.load_file


#' @rdname change_config
#' @export
change_config <- function(surveillance_system){

  yamlfile <- system.file("configurations", paste0(surveillance_system, ".yml"), package="movenet")
  if(yamlfile=="") stop("Specified surveillance system config file not found")

  # Change to contents of yaml file:
  save_config(yaml.load_file(yamlfile))
}


#' @rdname new_config
#' @export
new_config <- function(){

  # Option to save the template for editing:
  file.copy(system.file("configurations", "template.yaml", package="movenet"), getwd())

}


#' @rdname get_config
#' @export
get_config <- function(parameter){

  # TODO: use pmatch
  stopifnot(parameter %in% names(movenetenv$config))

  return(movenetenv$config[[parameter]])
}


save_config <- function(config){
  movenetenv$config <- config
}
movenetenv <- new.env()
