#' @name change_config
#' @title Change configurations
#'
#' @param surveillance_system some description
#'
#' @importFrom yaml yaml.load_file
#' @importFrom purrr flatten


#' @rdname change_config
#' @export
load_config <- function(configname){

  yamlfile <- system.file("configurations", paste0(configname, ".yml"), package="movenet")
    if(yamlfile=="") stop(paste("Specified config file", yamlfile,"not found"))

  # Suggestions to allow reading of any configfile (but not worry about config path):
  # 1) two arguments: name & path
  # 2) guess whether name or path from form
  # See https://github.com/digivet-consortium/movenet/issues/9

  # Change to contents of yaml file:
  config2options(yaml.load_file(yamlfile))
}

#' @rdname change_config
#' @export
#save_config <- function(name){
#
#  template <- system.file("configurations", "template.yml", package="movenet")
#  outfile <- system.file("configurations", paste0(name, ".yml"), package="movenet")
#  file.copy(template_path, outfile)
#
#  as.yaml(movenetenv$options)
#}

# Idea: argument save_to_configurations_dir = TRUE/FALSE (or similar), to save to directory with pre-installed config files rather than working directory


#' @rdname change_config
#' @export
new_config <- function(){

  # Option to save the template for editing:
  file.copy(system.file("configurations", "template.yml", package="movenet"), getwd())

}


#' @rdname change_config
#' @export
get_config <- function(...){
  #This is a mixture of runjags.options and runjags.getOption,
  #Allows for querying of multiple options simultaneously
  #Works with get_config("option1","option2") but not get_config(c("option1","option2"))

  opts <- list(...)
  if(length(opts)>0){
    options <- flatten(movenetenv$options)
    recognised <- pmatch(opts, names(options))
    if(any(is.na(recognised))){
      warning(paste("Igoring unmatched or ambiguous option(s): ", paste(names(opts)[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
    }
    return(options[recognised[!is.na(recognised)]])
  }

  #if no specific options given, return full set of options
  else{
    return(options)
  }

}

#' @rdname change_config
#' @export
movenet.options <- function(...){
  #doesnt work with named list (allowed in R options())
  #invisibly returns newly set values rather than previous values (as R options() does)
  #doesn't print options when called without argument (returns values invisibly)
  opts <- list(...)

  if(length(opts)>0){
    options_w_structure <- movenetenv$options
    options_no_structure <- flatten(movenetenv$options)
    recognised <- pmatch(names(opts), names(options_no_structure))
    if(any(is.na(recognised))){
      warning(paste("Igoring unmatched or ambiguous option(s): ", paste(names(opts)[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
    }
    optnames <- names(options_no_structure)[recognised[!is.na(recognised)]]
    if(length(optnames)>0) for(i in 1:length(optnames)){
      options_no_structure[optnames[i]] <- opts[[i]]
    }
    assign("options",relist(unlist(options_no_structure),options_w_structure),envir=movenetenv)
  }

  #Here one can plug in checks for valid option setting

  invisible(options) #returns newly set values rather than previous values
  }


#' @rdname change_config
#' @export
movenet.getOption <- function(name){
  if(length(name)!=1) stop("Only 1 option can be retrieved at a time")
  options <- flatten(movenetenv$options)
  opt <- pmatch(name,names(options))
  if(is.na(opt)) stop(paste("Unmatched or ambiguous option '", name, "'", sep=""))
  return(options[[opt]])
}


config2options <- function(config){
  movenetenv$options <- config
}
movenetenv <- new.env()

#options2config <- function(options){
#
#}

