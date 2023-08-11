#' @name change_config
#' @title Change configurations
#'
#' @param configname some description
#'
#' @importFrom yaml yaml.load_file write_yaml
#' @importFrom purrr flatten modify


#' @rdname change_config
#' @export
load_config <- function(configfile){
  if(missing(configfile)) stop("Argument `configfile` is missing. Please provide either the name of a preinstalled config file, or the path of the config file you wish to load.", call. = FALSE)
  if(file.exists(configfile)){
    yamlfile <- configfile
  } else {
    if(grepl(".yml",configfile)==TRUE){
      yamlfile <- system.file("configurations", configfile, package="movenet")
    } else {
      yamlfile <- system.file("configurations", paste0(configfile, ".yml"), package="movenet")
    }
    if(yamlfile=="") stop(paste("Specified config file not found:", configfile), call. = FALSE)
  }

  if(validate_config(yamlfile) == TRUE){
    loaded_config <- yaml.load_file(yamlfile)
    movenetenv$options[names(loaded_config)] <- loaded_config # Changes (move|holding)data_fileopts & _cols to contents of yaml file
    message(paste("Successfully loaded config file:", configfile))
  }

}
movenetenv <- new.env()

#' @rdname change_config
#' @export
save_config <- function(outfile, config_type = c("movement", "holding")){
  if(missing(outfile)) stop("Argument `outfile` is missing. Please provide a path to which to save the config file to.", call. = FALSE)
  if(outfile=="") stop('"" is not a valid value for `outfile`. Please provide a path to which to save the config file to.', call. = FALSE)
  if(!(all(config_type %in% c("movement", "holding")))) stop("Argument `config_type` must be one of 'movement', 'holding', or c('movement','holding')")

  options_to_write <- lapply(config_type, function(x) {
    switch(
      x,
      "movement" = list(movedata_fileopts = movenetenv$options$movedata_fileopts,
                        movedata_cols = movenetenv$options$movedata_cols),
      "holding" = list(holdingdata_fileopts = movenetenv$options$holdingdata_fileopts,
                       holdingdata_cols = movenetenv$options$holdingdata_cols)
    )}) %>%
    unlist(recursive = FALSE)

  write_yaml(x = options_to_write, file = outfile)

  message(paste("Successfully saved", paste(config_type, collapse = " and "),"configurations to:", outfile))
}

# This leaves strings/fields unquoted, but that should be fine.


#' @rdname change_config
#' @export
new_config <- function(config_type){

  if (!has_element(c("move","holding"),config_type)){
    stop("Argument `config_type` must be either 'move' or 'holding'")
  }

  # Option to save the template for editing:
  file.copy(system.file("configurations", paste0(config_type,"config_template.yml"), package = "movenet"), getwd())

  message(paste0("Successfully saved ", config_type, " config template to working directory. It can be found at: ", getwd(), "/", config_type, "config_template.yml"))

}


#' @rdname change_config
#' @export
get_config <- function(...){
  #Set alias to get_option, get_options?
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
#' @importFrom utils relist
change_config <- function(...){
  #set aliases: set_config, set_option, set_options?
  #remove behaviour to invisibly return old options [no longer trying to copy base R options system]?

  old_opts <- movenetenv$options
  options_no_structure <- flatten(movenetenv$options)
  opts <- flatten(list(...))
  if(has_element(list(...), NULL)){
    #The following should really only be for mandatory options. Should somehow allow functionality to remove optional options with NULL
    #But the assign/relist statement in line 178 doesnt deal well with NULLs -> ??
    warning(paste("Option values can't be NULL. Ignoring option(s) with value NULL:", paste(names(list(...))[which(sapply(list(...),is.null))],collapse=", ")), call. = FALSE)
  }
  if(length(opts)>0){
    recognised <- pmatch(names(opts), names(options_no_structure))
    if(any(is.na(recognised))){
      warning(paste("Ignoring unmatched or ambiguous option(s):", paste(names(opts)[is.na(recognised)],collapse=", ")), call. = FALSE)
      opts <- opts[!is.na(recognised)]
    }
    optnames <- names(options_no_structure)[recognised[!is.na(recognised)]]

    if(any(opts %in% options_no_structure)){
      valinoldopts_opts <- opts[which(opts %in% options_no_structure)]
      valinoldopts_names <- optnames[which(opts %in% options_no_structure)]
      unchanged_names <- valinoldopts_names[which(sapply(valinoldopts_opts,function(x){
        if(names(options_no_structure)[which(options_no_structure == x)] %in% optnames){
          opts[which(optnames == names(options_no_structure)[which(options_no_structure == x)])] == x
        } else {FALSE}
      }))]
      if(length(unchanged_names) > 0){
        warning(paste("Ignoring unchanged option(s):", paste(unchanged_names, collapse=", ")), call. = FALSE)
        opts <- opts[-which(optnames %in% unchanged_names)]
        optnames <- optnames[-which(optnames %in% unchanged_names)]
      }
    }

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
    if(("country_code" %in% optnames) & (nchar(opts["country_code"]) != 2)){
      warning("Ignoring option `country_code`, as the value doesn't have the required format (two characters)", call. = FALSE)
      opts <- opts[-which(optnames=="country_code")]
      optnames <- optnames[-which(optnames=="country_code")]
    }
    if(("date_format" %in% optnames) & (!grepl("%(Y|y|AD|D|F|x|s)|^$",opts["date_format"]))){
      warning("Ignoring option `date_format`, as the value doesn't appear to match readr date format specifications. See `?readr::parse_date` for guidance.", call. = FALSE)
      opts <- opts[-which(optnames=="date_format")]
      optnames <- optnames[-which(optnames=="date_format")]
    }
    if(("coord_EPSG_code" %in% optnames) & (!is.integer(opts["coord_EPSG_code"]))){
      warning("Ignoring option `coord_EPSG_code`, as the value doesn't have the required format (integer)", call. = FALSE)
      opts <- opts[-which(optnames=="coord_EPSG_code")]
      optnames <- optnames[-which(optnames=="coord_EPSG_code")]
    }
    charint_opts <- opts[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
    if(any(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))){
      charint_names <- optnames[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
      noncharint_names <- charint_names[which(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (character or integer):", paste(noncharint_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% noncharint_names)]
      optnames <- optnames[-which(optnames %in% noncharint_names)]
    }
    col_opts <- opts[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
    if(anyDuplicated(col_opts) != 0){
      col_names <- optnames[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
      dupl_names <- col_names[which(col_opts %in% col_opts[duplicated(col_opts)])]
      warning(paste("Values for movedata_cols / holdingdata_cols options must be unique. Ignoring options with duplicate values:", paste(dupl_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% dupl_names)]
      optnames <- optnames[-which(optnames %in% dupl_names)]
    }
    col_opts <- opts[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
    if(any(col_opts %in% c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols))){
      col_names <- optnames[which(optnames %in% names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols)))]
      valinoldopts_opts <- col_opts[which(col_opts %in% c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols))]
      valinoldopts_names <- col_names[which(col_opts %in% c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols))]
      #disallow any col_opts with values that are also in movenetenv$options$movedata_cols or movenetenv$options$holdingdata_cols (resulting in duplicates)
      #except in cases of "option values being switched": where the name of the potential duplicate option in movenetenv$.. is also in opts where it is associated with a DIFFERENT value
      notallowed_names <- valinoldopts_names[which(!sapply(valinoldopts_opts,function(x){
        if(names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols))[which(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols) == x)] %in% col_names){
          col_opts[which(col_names == names(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols))[which(c(movenetenv$options$movedata_cols, movenetenv$options$holdingdata_cols) == x)])] != x
        } else {FALSE}
      }))]
      if(length(notallowed_names) > 0){
        warning(paste("Values for movedata_cols / holdingdata_cols options must be unique. Ignoring options that would result in duplicate values:", paste(notallowed_names, collapse=", ")), call. = FALSE)
        opts <- opts[-which(optnames %in% notallowed_names)]
        optnames <- optnames[-which(optnames %in% notallowed_names)]
      }
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
