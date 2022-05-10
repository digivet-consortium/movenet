#' @name change_config
#' @title Change configurations
#'
#' @param surveillance_system some description
#'
#' @importFrom yaml yaml.load_file
#' @importFrom purrr flatten


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
  file.copy(system.file("configurations", "template.yml", package="movenet"), getwd())

}


#' @rdname get_config
#' @export
get_config <- function(...){
  #This is a mixture of runjags.options and runjags.getOption,
  #Allows for querying of multiple options simultaneously
  #Works with get_config("option1","option2") but not get_config(c("option1","option2"))

  opts <- list(...)
  if(length(opts)>0){
    recognised <- pmatch(opts, names(movenetenv$config))
    if(any(is.na(recognised))){
      warning(paste("Igoring unmatched or ambiguous option(s): ", paste(opts[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
    }
    return(movenetenv$config[recognised[!is.na(recognised)]])
  }

  #if no specific options given, return full set of options
  else{
    return(movenetenv$config)
  }

}


save_config <- function(config){
  movenetenv$config <- flatten(config) %>% #can use unlist instead (-> named chr)
    `names<-`(sub(pattern = ".+\\.(.+)", replacement = "\\1", x = names(.)))
}
movenetenv <- new.env()
