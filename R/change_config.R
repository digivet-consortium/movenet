#' @name change_config
#' @title Change configurations
#'
#' @param country some description


#' @rdname change_config
#' @export
change_config <- function(country){

  yamlfile <- system.file("configurations", paste0(country, ".yaml"), package="movenet")
  if(yamlfile=="") stop("Specified country config not found")

  # Change to contents of yaml file:
  save_config(list(hello=1, date_time="%Y-%m-%d"))
}


#' @rdname change_config
#' @export
new_config <- function(){

  # Option to save the template for editing:
  file.copy(system.file("configurations", "template.yaml", package="movenet"), getwd())

}


#' @rdname change_config
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
