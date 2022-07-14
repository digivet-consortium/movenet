#' @name change_config
#' @title Change configurations
#'
#' @param configname some description
#'
#' @importFrom yaml yaml.load_file write_yaml
#' @importFrom purrr flatten modify


#' @rdname change_config
#' @export
load_config <- function(configname){

  yamlfile <- system.file("configurations", paste0(configname, ".yml"), package="movenet")
  if(yamlfile=="") stop(paste("Specified config file not found:", configname))

  # Suggestions to allow reading of any configfile (but not worry about config path):
  # 1) two arguments: name & path
  # 2) guess whether name or path from form
  # See https://github.com/digivet-consortium/movenet/issues/9

  # Should this also run validate_config.R (or somehow refer to some validation result)?

  # Change options to contents of yaml file:
  movenetenv$options <- yaml.load_file(yamlfile)

  message(paste("Successfully loaded config file:", configname))
}
movenetenv <- new.env()

#' @rdname change_config
#' @export
save_config <- function(configname){

  if(configname=="") stop('"" is not a valid configname')
  outfile <- paste0(system.file("configurations", package = "movenet"),"/",configname,".yml")

  write_yaml(x = movenetenv$options, file = outfile)

  message(paste("Successfully saved config file:", configname,"\nIt can be found at:", outfile))
}

# This leaves strings/fields unquoted, but that should be fine.

# Idea: argument save_to_configurations_dir = TRUE/FALSE (or similar), to save to directory with pre-installed config files rather than working directory


#' @rdname change_config
#' @export
new_config <- function(){

  # Option to save the template for editing:
  file.copy(system.file("configurations", "template.yml", package = "movenet"), getwd())

  message(paste0("Successfully saved config template to working directory. It can be found at: ",getwd(),"/template.yml"))

}


#' @rdname change_config
#' @export
get_config <- function(...){
  #This is a mixture of runjags.options and runjags.getOption,
  #Allows for querying of multiple options simultaneously
  #Works with get_config("option1","option2"), get_config(c("option1","option2")), and get_config(list("option1","option2"))

  opts <- flatten(list(...))
  options <- flatten(movenetenv$options)
  if(length(opts)>0){
    recognised <- pmatch(opts, names(options))
    if(any(is.na(recognised))){
      warning(paste("Ignoring unmatched or ambiguous option(s):", paste(opts[is.na(recognised)],collapse=", ")))
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
  #doesn't print options when called without argument
  old_opts <- movenetenv$options
  options_no_structure <- flatten(movenetenv$options)
  opts <- flatten(list(...))
  if(has_element(list(...), NULL)){
    #The following should really only be for mandatory options. Should somehow allow functionality to remove optional options with NULL
    #But the assign/relist statement in line 111 doesnt deal well with NULLs -> ??
    warning(paste("Option values can't be NULL. Ignoring option(s) with value NULL:", paste(names(list(...))[which(sapply(list(...),is.null))],collapse=", ")), call. = FALSE)
  }
  if(length(opts)>0){
    recognised <- pmatch(names(opts), names(options_no_structure))
    if(any(is.na(recognised))){
      warning(paste("Ignoring unmatched or ambiguous option(s):", paste(names(opts)[is.na(recognised)],collapse=", ")), call. = FALSE)
      opts <- opts[!is.na(recognised)]
    }
    optnames <- names(options_no_structure)[recognised[!is.na(recognised)]]

    char_opts <- opts[which(optnames %in% c("separator","encoding","decimal","date_format"))]
    if(any(!sapply(char_opts,is.character))){
      char_names <- optnames[which(optnames %in% c("separator","encoding","decimal","date_format"))]
      nonchar_names <- char_names[which(!sapply(char_opts, is.character))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (character):", paste(nonchar_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% nonchar_names)]
      optnames <- optnames[-which(optnames %in% nonchar_names)]
    }
    singlechar_opts <- opts[which(optnames %in% c("separator","decimal"))]
    if(any(!sapply(singlechar_opts,function(x){nchar(x) == 1}))){
      singlechar_names <- optnames[which(optnames %in% c("separator","decimal"))]
      nonsinglechar_names <- singlechar_names[which(!sapply(singlechar_opts,function(x){nchar(x) == 1}))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (single character):", paste(nonsinglechar_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% nonsinglechar_names)]
      optnames <- optnames[-which(optnames %in% nonsinglechar_names)]
    }
    if(("date_format" %in% optnames) & (!grepl("%(Y|y|AD|D|F|x|s)|^$",opts["date_format"]))){
      warning("Ignoring option `date_format`, as the value doesn't appear to match readr date format specifications. See `?readr::parse_datetime` for guidance.", call. = FALSE)
      opts <- opts[-which(optnames=="date_format")]
      optnames <- optnames[-which(optnames=="date_format")]
    }
    charint_opts <- opts[which(optnames %in% c("origin_ID","dest_ID","move_date","nr_pigs"))]
    if(any(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))){
      charint_names <- optnames[which(optnames %in% c("origin_ID","dest_ID","move_date","nr_pigs"))]
      noncharint_names <- charint_names[which(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (character or integer):", paste(noncharint_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% noncharint_names)]
      optnames <- optnames[-which(optnames %in% noncharint_names)]
    }

    if(length(optnames)>0) for(i in 1:length(optnames)){
      options_no_structure[optnames[i]] <- opts[[i]]
    }
    assign("options",modify(relist(options_no_structure,old_opts),flatten),envir=movenetenv) #this doesn't deal well with NULLs
    invisible(old_opts) #invisibly returns previous values (want this with or without structure?)
  }
  #if no specific options given, return full set of options (without structure)
  else{
    return(options_no_structure)
  }
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



