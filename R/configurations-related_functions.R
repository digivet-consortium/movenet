#' Load configurations from a config file into the movenet environment
#'
#' `load_config()` checks that the provided `configfile` is valid, and then loads
#' its content into the movenet environment. This lets the movenet package know
#' how to read and interpret upcoming input data files.
#'
#' @details Values of configurations representing column headers or indices (in
#'   the config file `fileopts` section) are converted to ASCII-compliant and
#'   syntactically valid names before being loaded into in the environment.
#'
#' @param configfile Either the path to a YAML-format movenet config file, or the name of a
#'   pre-installed config file in the movenet package's `configurations` folder.
#'
#' @returns No return value, but `load_config()` has the effect of copying the
#'   provided configurations into the movenet environment. If this is
#'   successful, a message is printed.
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a config file using a path
#' load_config(system.file("configurations", "fakeScotEID_holding.yml", package="movenet"))
#' get_config() #Examine configurations
#'
#' # Alternatively, load a pre-installed config file by using just its name
#' load_config("ScotEID.yml")
#' get_config() #Examine configurations
#'
#' # Clean-up: Reinstate previous configurations
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv")
#'
#' @seealso
#' * [validate_config()] for the underlying config file validation process.
#' * [asciify()] for the underlying ASCIIfication process.
#' * `vignette("configurations")` for an explanation of the movenet config system.
#' * `list.files(system.file("configurations", package = "movenet"))` for a list of pre-installed templates and validated config files.
#' @family configurations-related functions
#' @family functions for initial data processing
#'
#' @importFrom yaml yaml.load_file
#'
#' @export
load_config <- function(configfile){
  if(missing(configfile)) stop("Argument `configfile` is missing. Please provide either the name of a preinstalled config file, or the path of the config file you wish to load.", call. = FALSE)
  if(file.exists(configfile)){
    yamlfile <- configfile
  } else {
    if(isTRUE(grepl(".yml",configfile))){
      yamlfile <- system.file("configurations", configfile, package="movenet")
    } else {
      yamlfile <- system.file("configurations", paste0(configfile, ".yml"), package="movenet")
    }
    if(yamlfile=="") stop(paste("Specified config file not found:", configfile), call. = FALSE)
  }

  if(isTRUE(validate_config(yamlfile))){
    loaded_config <- yaml.load_file(yamlfile)
    # Change (move|holding)data_fileopts to contents of yaml file
    movenetenv$options[names(loaded_config[grep("fileopts",names(loaded_config))])] <-
      loaded_config[grep("fileopts",names(loaded_config))]
    # Change (move|holding)data_cols to contents of yaml file, but asciify column headers first
    list_of_lists <- loaded_config[grep("cols",names(loaded_config))]
    lapply(seq_along(list_of_lists),
           function(x) {
             list_x <- lapply(seq_along(list_of_lists[[x]]),
                              function(y) movenet::asciify(list_of_lists[[x]][y]))
             names(list_x) <- names(list_of_lists[[x]])
             movenetenv$options[names(list_of_lists)[x]] <- list(list_x)
           })

    message(paste("Successfully loaded config file:", configfile))
  }

}
movenetenv <- new.env()

#' Save current configurations to a config file
#'
#' `save_config()` saves the configurations currently set in the movenet
#' environment to a new config file of the type indicated by `config_type`.
#'
#' @details N.B.: The resulting config file is not automatically validated.
#'
#' @param outfile The name and path of the file to which the configurations will
#'   be saved.
#' @param config_type Character vector representing the type of configurations
#'   to save: one of `c("movement", "holding")` (default, resulting in a
#'   combined config file), `"movement"` or `"holding"`.
#'
#' @returns No return value, but `save_config()` has the effect of writing a
#'   YAML-format config file containing (a subset of) the configurations found
#'   in the movenet environment. If this is successful, a message is printed.
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a config file
#' load_config(system.file("configurations", "ScotEID.yml", package="movenet"))
#'
#' # Save the currently loaded movement configurations to a new config file in `tempdir()`
#' save_config(paste0(tempdir(),"\\saved_movement_config.yml"), "movement")
#'
#' #Examine file contents
#' xfun::file_string(paste0(tempdir(),"\\saved_movement_config.yml"))
#'
#' # Clean-up: Remove the saved file and reinstate previous configurations
#' file.remove(paste0(tempdir(),"\\saved_movement_config.yml"))
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv")
#'
#'
#' @seealso `vignette("configurations")` for an explanation of the movenet
#'   config system.
#' @family configurations-related functions
#'
#' @importFrom yaml write_yaml
#' @importFrom xfun file_string
#'
#' @export
save_config <- function(outfile, config_type = c("movement", "holding")){
  if(missing(outfile)) stop("Argument `outfile` is missing. Please provide a path to which to save the config file to.", call. = FALSE)
  if(outfile=="") stop('"" is not a valid value for `outfile`. Please provide a path to which to save the config file to.', call. = FALSE)
  if(is.null(config_type) || !(all(config_type %in% c("movement", "holding")))) stop("Argument `config_type` must be one of 'movement', 'holding', or c('movement','holding')")

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

#' Copy a config file template to your working directory
#'
#' `new_config()` copies an empty config file template to the current working
#' directory.
#'
#' @param config_type Character vector representing the type of config file for
#'   which to copy the template: one of `c("movement", "holding")` (default,
#'   indicating the combined config file template), `"movement"` or `"holding"`.
#'
#' @returns No return value, but `new_config()` has the effect of writing a
#'   YAML-format config file template to the current working directory. If this
#'   is successful, a message is printed.
#'
#' @examples
#' # Copy the movement config file template to your working directory
#' new_config(config_type = "movement")
#'
#' # Examine file contents
#' xfun::file_string(paste0(getwd(),"/movementconfig_template.yml"))
#'
#' # Clean-up: Remove the saved file
#' file.remove(paste0(getwd(),"/movementconfig_template.yml"))
#'
#' @family configurations-related functions
#' @seealso `vignette("configurations")` for an explanation of the movenet
#'   config system.
#'
#' @export
new_config <- function(config_type = c("movement", "holding")){

  if(is.null(config_type) || !(all(config_type %in% c("movement", "holding")))) stop("Argument `config_type` must be one of 'movement', 'holding', or c('movement','holding')")

  # Save the template for editing:
  if(all(c("movement", "holding") %in% config_type)){
    template_name <- "allconfig_template.yml"
  } else {
    template_name <- paste0(config_type,"config_template.yml")
  }
  file.copy(system.file("configurations", template_name, package = "movenet"), getwd())

  message(paste0("Saved ", template_name, " to working directory. It can be found at: ", getwd(), "/", template_name))
}

#' Get the values of configurations currently set in the movenet environment
#'
#' `get_config()` allows the user to examine the values of one or more
#' configurations as currently set in the movenet environment.
#'
#' @details ## Configuration names
#'
#' Each configuration name must follow the `"datatype_configtype.configname"`
#' format, consisting of the following elements:
#' * `datatype`: Type of data the configuration applies to. Either `movedata` or `holdingdata`.
#' * `configtype`: Type of configuration. Either `fileopts` (file options) or `cols` (column headers/indices).
#' * `configname`: Name of the specific configuration.
#'
#' For example: `"movedata_fileopts.separator"`, `"holdingdata_cols.id"`
#'
#' `get_config()` uses partial matching for configuration names. If a
#' configuration name is ambiguous or unrecognised (e.g. if the configuration is
#' unset), a warning is displayed.
#'
#' @param ... One or more configuration names, in the form of individual
#'   character strings, a character vector, or a list.
#'
#' @returns
#' * If no arguments are provided, an unstructured named list of all configurations
#' as currently set in the movenet environment.
#' * If one or more configuration names are provided, an unstructured named list
#' with the current value of these configurations, or an empty named list and a
#' warning if the configurations are unset or ambiguous.
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a config file
#' load_config(system.file("configurations", "fakeScotEID_holding.yml", package="movenet"))
#' get_config() #Examine configurations
#'
#' # Query the values of all configurations
#' get_config()
#'
#' # Query the values of specific configurations
#' get_config("holdingdata_fileopts.separator", "holdingdata_cols.id")
#'
#' # Clean-up: Reinstate previous configurations
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv")
#'
#' @family configurations-related functions
#' @seealso `vignette("configurations")` for an explanation of the movenet
#'   config system.
#'
#' @importFrom purrr flatten
#' @export
get_config <- function(...){
  #requires options to be plugged in w/names like "movedata_fileopts.separator".
  #Partial matching only works for initial matches
  #Set alias to get_option, get_options?
  #This is a mixture of runjags.options and runjags.getOption,
  #Allows for querying of multiple options simultaneously
  #Works with get_config("option1","option2"), get_config(c("option1","option2")), and get_config(list("option1","option2"))

  opts <- flatten(list(...))
  options <- unlist(movenetenv$options, recursive = FALSE) #results in list w/names e.g. "movedata_fileopts.separator"
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

#' Change individual configurations in the movenet environment
#'
#' `change_config()` allows the user to set the value of one or more
#' configurations in the movenet environment. Any requested changes are
#' validated before being made.
#'
#' @details ## Configuration names
#'
#' Each configuration name must follow the `"datatype_configtype.configname"`
#' format, consisting of the following elements:
#' * `datatype`: Type of data the configuration applies to. Either `movedata` or `holdingdata`.
#' * `configtype`: Type of configuration. Either `fileopts` (file options) or `cols` (column headers/indices).
#' * `configname`: Name of the specific configuration.
#'
#' For example: `"movedata_fileopts.separator"`, `"holdingdata_cols.id"`
#'
#' `change_config()` uses partial matching for configuration names. If a
#' configuration name is ambiguous or unrecognised (e.g. if the configuration is
#' unset), the requested change is ignored and a warning is displayed.
#'
#' ## Configuration values
#' Before configuration values are set, their data format is validated.
#' Valid configuration data formats are as follows:
#' * `(move|holding)data_fileopts.separator`: A single character.
#' * `(move|holding)data_fileopts.decimal`: A single character.
#' * `(move|holding)data_fileopts.encoding`: A character string.
#' * `(move|holding)data_fileopts.date_format`: A character string matching readr date format specifications. See [readr::parse_date()] for
#' guidance.
#' * `holdingdata_fileopts.country_code`: A character string consisting of two upper-case letters.
#' * `holdingdata_fileopts.coord_EPSG_code`: A single integer.
#' * `(move|holding)data_cols` configurations: A character string or a single integer.
#'  All configurations within `movedata_cols` or `holdingdata_cols` must have unique values.
#'
#' Configurations can't be set to `NULL`.
#'
#' If a configuration value is invalid, the requested change is ignored and a
#' warning is displayed.
#'
#' @param ... One or more configurations to change, using `name = value` format.
#'   These can be provided in the form of individual name-value pairs, as a
#'   named character vector, or as a named list.
#'
#' @returns
#' * If one or more configuration name-value pairs are provided, a structured
#' named list of the values of all configurations in the movenet environment,
#' as they were before the function call (returned invisibly).
#' As side effect, any provided configurations are changed in the movenet environment.
#' If a configuration name is ambiguous or unrecognised, or if a requested change
#' would result in an invalid configuration value, the requested configuration
#' change is ignored and a warning is displayed.
#' * If no arguments are provided, an unstructured named list of all configurations as currently set in the movenet environment.
#'
#' @examples
#' # Set-up: Save movenet environment with current configurations
#' movenetenv <- movenet:::movenetenv
#' old_config <- movenetenv$options
#'
#' # Load a config file and examine current configurations
#' load_config(system.file("configurations", "fakeScotEID_holding.yml", package="movenet"))
#' get_config("holdingdata_fileopts.separator", "holdingdata_cols.id") #Examine configurations
#'
#' # Change configurations
#' change_config(holdingdata_fileopts.sep = ".", holdingdata_cols.id = "identifier")
#' get_config("holdingdata_fileopts.separator", "holdingdata_cols.id") #Examine changed configurations
#'
#' # Clean-up: Reinstate previous configurations
#' movenetenv$options <- old_config
#' rm("old_config", "movenetenv")
#'
#' @family configurations-related functions
#' @seealso
#' * [readr::parse_date()] for guidance on date format specification strings.
#' * `vignette("configurations")` for an explanation of the movenet
#' config system.
#'
#' @importFrom purrr modify
#' @importFrom utils relist
#' @export
change_config <- function(...){
  #set aliases: set_config, set_option, set_options?
  #remove behaviour to invisibly return old options [no longer trying to copy base R options system]?

  old_opts <- movenetenv$options
  #options_no_structure <- flatten(movenetenv$options)
  options_no_structure <- unlist(movenetenv$options, recursive = FALSE)
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
        if(any(names(options_no_structure)[which(options_no_structure == x)] %in% optnames)){
          opts[which(optnames == names(options_no_structure)[which(options_no_structure == x)])] == x
        } else {FALSE}
      }))]
      if(length(unchanged_names) > 0){
        warning(paste("Ignoring unchanged option(s):", paste(unchanged_names, collapse=", ")), call. = FALSE)
        opts <- opts[-which(optnames %in% unchanged_names)]
        optnames <- optnames[-which(optnames %in% unchanged_names)]
      }
    }

    char_idx <- which(optnames %in% c("movedata_fileopts.separator","holdingdata_fileopts.separator",
                                            "movedata_fileopts.encoding","holdingdata_fileopts.encoding",
                                            "movedata_fileopts.decimal","holdingdata_fileopts.decimal",
                                            "movedata_fileopts.date_format","holdingdata_fileopts.date_format"))
    if(any(!sapply(opts[char_idx],is.character))){
      char_names <- optnames[char_idx]
      nonchar_names <- char_names[which(!sapply(opts[char_idx], is.character))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (character):", paste(nonchar_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% nonchar_names)]
      optnames <- optnames[-which(optnames %in% nonchar_names)]
    }
    singlechar_idx <- which(optnames %in% c("movedata_fileopts.separator","holdingdata_fileopts.separator",
                                            "movedata_fileopts.decimal","holdingdata_fileopts.decimal"))
    singlechar_opts <- opts[singlechar_idx]
    if(any(!sapply(singlechar_opts,function(x){nchar(x) == 1}))){
      singlechar_names <- optnames[singlechar_idx]
      nonsinglechar_names <- singlechar_names[which(!sapply(singlechar_opts,function(x){nchar(x) == 1}))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (single character):", paste(nonsinglechar_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% nonsinglechar_names)]
      optnames <- optnames[-which(optnames %in% nonsinglechar_names)]
    }
    if(("holdingdata_fileopts.country_code" %in% optnames) & (!is_valid_country_code(opts["holdingdata_fileopts.country_code"]))){
      warning("Ignoring option `holdingdata_fileopts.country_code`, as the value doesn't have the required format (two uppercase letters)", call. = FALSE)
      opts <- opts[-which(optnames=="holdingdata_fileopts.country_code")]
      optnames <- optnames[-which(optnames=="holdingdata_fileopts.country_code")]
    }
    if(("movedata_fileopts.date_format" %in% optnames) & (!is_valid_date_format(opts["movedata_fileopts.date_format"]))){
      warning("Ignoring option `movedata_fileopts.date_format`, as the value doesn't appear to match readr date format specifications. See `?readr::parse_date` for guidance.", call. = FALSE)
      opts <- opts[-which(optnames=="movedata_fileopts.date_format")]
      optnames <- optnames[-which(optnames=="movedata_fileopts.date_format")]
    }
    if(("holdingdata_fileopts.date_format" %in% optnames) & (!is_valid_date_format(opts["movedata_fileopts.date_format"]))){
      warning("Ignoring option `holdingdata_fileopts.date_format`, as the value doesn't appear to match readr date format specifications. See `?readr::parse_date` for guidance.", call. = FALSE)
      opts <- opts[-which(optnames=="holdingdata_fileopts.date_format")]
      optnames <- optnames[-which(optnames=="holdingdata_fileopts.date_format")]
    }
    if(("holdingdata_fileopts.coord_EPSG_code" %in% optnames) & (!is.integer(opts["holdingdata_fileopts.coord_EPSG_code"]))){
      warning("Ignoring option `holdingdata_fileopts.coord_EPSG_code`, as the value doesn't have the required format (integer)", call. = FALSE)
      opts <- opts[-which(optnames=="holdingdata_fileopts.coord_EPSG_code")]
      optnames <- optnames[-which(optnames=="holdingdata_fileopts.coord_EPSG_code")]
    }
    charint_idx <- which(optnames %in% grep("(move|holding)data_cols",names(options_no_structure), value = TRUE))
    charint_opts <- opts[charint_idx]
    if(any(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))){
      charint_names <- optnames[charint_idx]
      noncharint_names <- charint_names[which(!sapply(charint_opts,function(x){is.character(x) | is.integer(x)}))]
      warning(paste("Ignoring option(s) of which the value(s) do(es) not have the required format (character or integer):", paste(noncharint_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% noncharint_names)]
      optnames <- optnames[-which(optnames %in% noncharint_names)]
    }
    movecol_idx <- which(optnames %in% grep("movedata_cols",names(options_no_structure), fixed = TRUE, value = TRUE))
    movecol_opts <- opts[movecol_idx]
    if(anyDuplicated(movecol_opts) != 0){
      movecol_names <- optnames[movecol_idx]
      movedupl_names <- movecol_names[which(movecol_opts %in% movecol_opts[duplicated(movecol_opts)])] #this looks like redundant code but isn't.
      warning(paste("Values for movedata_cols options must be unique. Ignoring options with duplicate values:", paste(movedupl_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% movedupl_names)]
      optnames <- optnames[-which(optnames %in% movedupl_names)]
    }
    holdingcol_idx <- which(optnames %in% grep("holdingdata_cols",names(options_no_structure), fixed = TRUE, value = TRUE))
    holdingcol_opts <- opts[holdingcol_idx]
    if(anyDuplicated(holdingcol_opts) != 0){
      holdingcol_names <- optnames[holdingcol_idx]
      holdingdupl_names <- holdingcol_names[which(holdingcol_opts %in% holdingcol_opts[duplicated(holdingcol_opts)])] #this looks like redundant code but isn't.
      warning(paste("Values for holdingdata_cols options must be unique. Ignoring options with duplicate values:", paste(holdingdupl_names, collapse=", ")), call. = FALSE)
      opts <- opts[-which(optnames %in% holdingdupl_names)]
      optnames <- optnames[-which(optnames %in% holdingdupl_names)]
    }
    movecol_idx <- which(optnames %in% grep("movedata_cols",names(options_no_structure), fixed = TRUE, value = TRUE))
    movecol_opts <- opts[movecol_idx]
    if(any(movecol_opts %in% movenetenv$options$movedata_cols)){
      movecol_names <- optnames[movecol_idx]
      valinoldopts_opts <- movecol_opts[which(movecol_opts %in% movenetenv$options$movedata_cols)]
      valinoldopts_names <- movecol_names[which(movecol_opts %in% movenetenv$options$movedata_cols)]
      #disallow any col_opts with values that are also in movenetenv$options$movedata_cols (resulting in duplicates)
      #except in cases of "option values being switched": where the name of the potential duplicate option in movenetenv$.. is also in opts where it is associated with a DIFFERENT value
      notallowed_names <- valinoldopts_names[which(!sapply(valinoldopts_opts,function(x){
        if(names(movenetenv$options$movedata_cols)[which(movenetenv$options$movedata_cols == x)] %in% movecol_names){
          movecol_opts[which(movecol_names == names(movenetenv$options$movedata_cols)[which(movenetenv$options$movedata_cols == x)])] != x
        } else {FALSE}
      }))]
      if(length(notallowed_names) > 0){
        warning(paste("Values for movedata_cols options must be unique. Ignoring options that would result in duplicate values:", paste(notallowed_names, collapse=", ")), call. = FALSE)
        opts <- opts[-which(optnames %in% notallowed_names)]
        optnames <- optnames[-which(optnames %in% notallowed_names)]
      }
    }
    holdingcol_idx <- which(optnames %in% grep("holdingdata_cols",names(options_no_structure), fixed = TRUE, value = TRUE))
    holdingcol_opts <- opts[holdingcol_idx]
    if(any(holdingcol_opts %in% movenetenv$options$holdingdata_cols)){
      holdingcol_names <- optnames[holdingcol_idx]
      valinoldopts_opts <- holdingcol_opts[which(holdingcol_opts %in% movenetenv$options$holdingdata_cols)]
      valinoldopts_names <- holdingcol_names[which(holdingcol_opts %in% movenetenv$options$holdingdata_cols)]
      #disallow any col_opts with values that are also in movenetenv$options$holdingdata_cols (resulting in duplicates)
      #except in cases of "option values being switched": where the name of the potential duplicate option in movenetenv$.. is also in opts where it is associated with a DIFFERENT value
      notallowed_names <- valinoldopts_names[which(!sapply(valinoldopts_opts,function(x){
        if(names(movenetenv$options$holdingdata_cols)[which(movenetenv$options$holdingdata_cols == x)] %in% holdingcol_names){
          holdingcol_opts[which(holdingcol_names == names(movenetenv$options$holdingdata_cols)[which(movenetenv$options$holdingdata_cols == x)])] != x
        } else {FALSE}
      }))]
      if(length(notallowed_names) > 0){
        warning(paste("Values for holdingdata_cols options must be unique. Ignoring options that would result in duplicate values:", paste(notallowed_names, collapse=", ")), call. = FALSE)
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

is_valid_date_format <- function(char) grepl("%(Y|y|m|b|B|AD|D|F|x|s)|^$", char)
is_valid_country_code <- function(char) grepl("^[A-Z]{2}$", char)
