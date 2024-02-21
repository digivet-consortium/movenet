#' Create a dynamic network representation of movenet-format movement data
#'
#' `movedata2networkDynamic()` converts movenet-format movement and (optional)
#' holding data tibbles into a networkDynamic temporal network representation.
#' This assumes the network is directed with no loops, no hyperedges, and no
#' multiplex edges.
#'
#' @details
#' For compatibility with the networkDynamic package, node identifiers
#' need to be consecutive integers between 1 and the number of nodes in the
#' network (total number of holdings if `incl_nonactive_holdings == TRUE`, or
#' number of active holdings if `incl_nonactive_holdings == FALSE`). If holding
#' identifiers do not meet this condition, new integer identifiers are assigned,
#' in a random order. Original holding identifiers are stored as node attribute
#' `true_id` and set as [`persistent ids`][networkDynamic::persistent.ids].
#'
#' Any `movement_data` columns beyond `from`, `to` and `date` are set as dynamic
#' edge attributes in the network, and any `holding_data` columns beyond `id` are
#' set as unchanging node attributes.
#'
#' Nodes and edges are set as active only on each movement date associated with
#' the node or edge. Each activity spell is considered instantaneous: the
#' activity occurs on the movement date, but has duration 0.
#'
#' @param movement_data A movenet-format movement data tibble.
#' @param holding_data A movenet-format holding data tibble (optional).
#' @param incl_nonactive_holdings A logical that indicates whether to include
#'   holdings from `holding_data` that are not present in `movement_data`.
#'   Default is `FALSE`. If set to `TRUE`, holdings that don't trade within the
#'   period covered by `movement_data` are included in the network but set as
#'   non-active throughout the observation period.
#'
#' @returns
#' A networkDynamic object consisting of a directed temporal network,
#' with nodes representing holdings, and edges representing connections between
#' holdings. Moves are represented as edge spells (times at which edges are
#' considered active).
#' Additionally, information is printed out about the assumptions made while
#' constructing the network; this output is passed on from [networkDynamic::networkDynamic()].
#'
#' @examples
#' movement_data <- head(example_movement_data, 20)
#' holding_data <- head(example_holding_data, 20)
#'
#' # Create a network using only movement_data
#' movedata2networkDynamic(movement_data)
#'
#' # Create a network using movement_data and holding_data, including only
#' # active holdings
#' movedata2networkDynamic(movement_data, holding_data,
#'                         incl_nonactive_holdings = FALSE)
#'
#' # Create a network using movement_data and holding_data, including both
#' # active and non-active holdings
#' movedata2networkDynamic(movement_data, holding_data,
#'                         incl_nonactive_holdings = TRUE)
#'
#' rm(movement_data, holding_data)
#'
#' @seealso
#' * [networkDynamic::networkDynamic()] for the underlying network-generating process.
#' * `vignette("network-analysis")` for an overview of the movenet network analysis workflow.
#' @family network-related functions
#'
#' @importFrom dplyr filter select
#' @import network
#' @import networkDynamic
#' @importFrom tibble add_row
#'
#' @export
movedata2networkDynamic <- function(movement_data, holding_data = NULL,
                                    incl_nonactive_holdings = FALSE){

  #######################
  ### Argument checks ###
  #######################

  assert_data_frame(movement_data, min.cols = 4, null.ok = FALSE)
  assert_character(movement_data[[1]], any.missing = FALSE)
  assert_character(movement_data[[2]], any.missing = FALSE)
  assert_date(movement_data[[3]], any.missing = FALSE)
  assert_numeric(movement_data[[4]], any.missing = TRUE)

  assert_data_frame(holding_data, min.cols = 1, null.ok = TRUE)
  if(!is.null(holding_data)){
    #check that holding_ids are unique - should this be done in "reformat_data"?!
    assert_character(holding_data[[1]], unique = TRUE, any.missing = FALSE)

    holding_ids <- holding_data[[1]]
    }

  assert_logical(incl_nonactive_holdings, len = 1, any.missing = FALSE,
                 null.ok = FALSE)

  ############################################################
  ### Ensure consistency between movement and holding data ###
  ############################################################

  active_holding_ids <- unique(c(movement_data[[1]],movement_data[[2]]))
  all_holding_ids <- active_holding_ids

  if(!is.null(holding_data)){

    #If there are any holding ids present in movement_data, but missing from
    #holding_data, add these ids to holding_data with NAs for other columns
    missing_holding_ids <- !(active_holding_ids %in% holding_ids)
    if(any(missing_holding_ids)){
      holding_data <-
        holding_data %>%
        add_row("{names(holding_data)[1]}" :=
                  active_holding_ids[which(missing_holding_ids)])
      holding_ids <- holding_data[[1]]
    }

    #If there are any holding ids present in holding_data but missing from
    #movement_data, and if incl_nonactive_holdings == FALSE, delete these ids.
    additional_holding_ids <- !(holding_ids %in% active_holding_ids)
    if(any(additional_holding_ids) && isFALSE(incl_nonactive_holdings)){
        holding_data <-
          holding_data %>%
          dplyr::filter(.data[[names(holding_data)[1]]] %in% active_holding_ids)
    }

    all_holding_ids <- holding_data[[1]]
  }

  #############################################
  ### Ensure correct node identifier format ###
  #############################################

  #networkDynamic needs vertex.ids to be consecutive integers from 1 to n_nodes.
  #Check if holding ids are consecutive "integers" (in character format is ok),
  #and if this is not the case, renumber and save the key.
  #The key is later used to generate a "true_id" vertex attribute, and to set
  #vertex.pids (persistent identifiers).

  if(isFALSE(are_ids_consec_intchars_from_1(all_holding_ids))){

    output <- holdingids2consecints(movement_data, holding_data,
                                    incl_nonactive_holdings = TRUE)
    movement_data <- output$movement_data
    holding_data <- output$holding_data
    holding_id_key <- output$key

  }

  ########################################
  ### Reformat data and create network ###
  ########################################

  #Reformat data to integer vertex.ids and dates required by networkDynamic
  movement_data[1:3] <- movement_data[1:3] %>% lapply(as.numeric)

  #Create activity spells for edges and nodes
  edge_spells <- create_edge_spells(movement_data)
  vertex_spells <- create_vertex_spells(movement_data)

  #Identify correct n of nodes to create network with
  n_nodes <- ifelse(!is.null(holding_data), nrow(holding_data), nrow(vertex_spells))

  #Create the network
  net <- networkDynamic(base.net = network.initialize(n_nodes),
                        edge.spells = edge_spells,
                        vertex.spells = vertex_spells,
                        verbose = FALSE, create.TEAs = TRUE,
                        edge.TEA.names = names(edge_spells)[-c(1:4)])
  #Allow multiplex graphs? (Default = FALSE)
  #This may cause trouble with certain measures. Edge spells over time will
  #cover most cases, but what if multiple moves betw same farms on 1 day?
  #Allow loops? (Default = FALSE)

  #Set the weight column header as a network attribute for easy reference in
  #analysis workflow (so that config file isn't required)
  set.network.attribute(net, "weight", names(movement_data)[4])

  ###########################
  ### Set node attributes ###
  ###########################

  if(!is.null(holding_data)){

    #Set non-active holdings to "inactive"
    inactive_vertices <-
      as.integer(holding_data[[1]][which(!(holding_data[[1]] %in% vertex_spells[[3]]))])
    deactivate.vertices(net, v = inactive_vertices)

    #Set holding_data columns as attributes
    holding_data <- holding_data[order(as.integer(holding_data[[1]])),]
    set.vertex.attribute(net, names(holding_data)[-1], holding_data[-1])
  }

  #######################################
  ### Set node persistent identifiers ###
  #######################################

  #vertex.pids (persistent identifiers) are needed to reliably identify nodes
  #when extracting subnetworks, as vertex.ids are re-numbered to match the
  #network size (i.e. vertex.ids are always 1:n_nodes; whereas vertex.pids
  #remain the same throughout extractions and can be non-int/non-consecutive).

  #if have key, add names (original holding ids) as vertex attrib "true_id"
  if(exists("holding_id_key", where = environment(), inherits = FALSE)){
    set.vertex.attribute(net, 'true_id', names(holding_id_key))
    warning(str_wrap("Node identifiers (vertex.id) have been changed to
    consecutive integers. Original identifiers have been set as persistent
    identifiers (vertex.pid) and can be identified for each node by running
    `get.vertex.pid(network_name, vertex.id)`."))
  } else {
  #else, convert vertex.names (original holding ids if consecutive ints) to
  #character and set these as vertex attrib "true_id" [for consistency]
    set.vertex.attribute(net, 'true_id',
                         as.character(get.vertex.attribute(net,'vertex.names')))
  }
  #set true_id attribute as vertex.pid
  set.network.attribute(net,'vertex.pid','true_id')

  ######################
  ### Return network ###
  ######################

  #return network w/ true_id and vertex.pid containing original holding ids in
  #character format
  return(net)
}

#' Create a networkDynamic edge.spells data frame from a movement data tibble
#'
#' `create_edge_spells()` is an internal helper function that converts a
#' modified movenet-format movement data tibble into a networkDynamic-compliant
#' edge activity (`edge.spells`) data frame. It requires integer node
#' identifiers, and dates converted to numeric format.
#'
#' @param movement_data A movement data tibble with integer node identifiers and
#' dates in numeric format.
#'
#' @returns A networkDynamic-compliant `edge.spells` data frame, with 1 column
#' more than `movement_data`. The first 4 columns of the returned data frame
#' correspond to `date`, `date` again, `from`, and `to`; any additional
#' `movement_data` columns are located thereafter.
#'
#' @seealso [networkDynamic::networkDynamic()] for `edge.spells` requirements
#' and how this data frame is used.
#'
#' @keywords internal
create_edge_spells <- function(movement_data){
  edge_spells <-
    movement_data[,c(3,3,1,2,4:length(movement_data))] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    sort()
  return(edge_spells)
}

#' Create a networkDynamic vertex.spells data frame from a movement data tibble
#'
#' `create_vertex_spells()` is an internal helper function that converts a
#' modified movenet-format movement data tibble into a networkDynamic-compliant
#' node activity (`vertex.spells`) data frame. It requires integer node
#' identifiers, and dates converted to numeric format.
#'
#' @param movement_data A movement data tibble with integer node identifiers and
#' dates in numeric format.
#'
#' @returns A networkDynamic-compliant `vertex.spells` data frame, consisting of
#' 3 columns indicating node activity onset (`date` from `movement_data`),
#' node activity terminus (again `date` from `movement_data`), and node
#' identifier (all identifiers in `from` and `to` in `movement_data`).
#'
#' @seealso [networkDynamic::networkDynamic()] for `vertex.spells` requirements
#' and how this data frame is used.
#'
#' @keywords internal
create_vertex_spells <- function(movement_data){
  vertex_spells <- movement_data[,c(3,3,1)] %>%
    `colnames<-`(colnames(movement_data[,c(3,3,2)]))
  vertex_spells <- rbind(vertex_spells, movement_data[,c(3,3,2)]) %>%
    data.frame(stringsAsFactors = FALSE) %>% sort() %>% unique()
  return(vertex_spells)
}

#' Are holding identifiers consecutive integer numbers, starting from 1?
#'
#' `are_ids_consec_intchars_from_1()` is an internal helper function that checks
#' whether holding identifiers are consecutive "integers" (in character format),
#' ranging from 1 to the total number of holding identifiers.
#' This is to facilitate conversion of movenet data tibbles into formats
#' compatible with [networkDynamic::networkDynamic()] and [SimInf::SimInf],
#' which require node identifiers to be consecutive integers starting from 1.
#'
#' @param holding_ids A character vector containing holding identifiers.
#'
#' @returns A single boolean: `TRUE` if the identifiers are consecutive integer
#' numbers starting from 1; `FALSE` otherwise.
#'
#' @keywords internal
are_ids_consec_intchars_from_1 <- function(holding_ids){
  return(
    all(grepl("^\\d+$", holding_ids)) && #char strings consisting of only digits...
      identical(sort(as.integer(holding_ids)), 1:length(holding_ids))
      #...which are consecutive integers from 1 to the total number of ids
  )
}

#' Replace holding identifiers with consecutive "integers" (in character format)
#' starting from 1, and harmonise movement and holding data
#'
#' @description
#' `holdingids2consecints()` is an internal helper function that replaces
#' holding identifiers in `movement_data` and (optional) `holding_data`
#' with consecutive "integers" (in character format), ranging from 1 to the
#' total number of node identifiers.
#'
#' Additionally, if `holding_data` is provided, it checks whether the included
#' holding identifiers correspond to those in `movement_data`, and it harmonises
#' both data tibbles:
#' * If `movement_data` contains additional holdings, entries for these are
#' added to `holding_data`, with any columns beyond `id` being assigned
#' the value `NA`.
#' * If `holding_data` contains any entries for holdings that are not in
#' `movement_data` (i.e. entries for non-active holdings), these are removed if
#' `incl_nonactive_holdings == FALSE` or kept otherwise.
#'
#' This function facilitates conversion of movenet data tibbles into formats
#' compatible with [networkDynamic::networkDynamic()] and [SimInf::SimInf],
#' which require node identifiers to be consecutive integers starting from 1.
#'
#' @param incl_nonactive_holdings A logical that indicates whether to keep entries in
#' `holding_data` for holdings that are not present in `movement_data`. Default
#' is `FALSE`.
#' @inheritParams movedata2networkDynamic
#'
#' @returns A named list with 3 elements:
#' * `key`: A named character vector representing the identifier replacement key.
#' Names are original identifiers, values are new integer identifiers.
#' * `movement_data`: `movement_data` with holding identifiers replaced with
#' integers.
#' * `holding_data`: Either `holding_data` with holding identifiers replaced
#' with integers, and potentially some additional or removed rows; or `NULL`
#' if no `holding_data` was provided.
#'
#' @keywords internal
holdingids2consecints <- function(movement_data, holding_data = NULL,
                                  incl_nonactive_holdings = FALSE){

  #Define holding_ids dependent on whether to only include active holdings (present
  #in movement_data) or to also include non-active holdings (present in
  #holding_data but not movement_data)
  if(isFALSE(incl_nonactive_holdings)){
    holding_ids <- unique(c(movement_data[[1]], movement_data[[2]]))
  } else if(isTRUE(incl_nonactive_holdings)){
    holding_ids <-
      unique(c(movement_data[[1]], movement_data[[2]], holding_data[[1]]))
    }


  key <- generate_anonymisation_key(holding_ids, prefix = "", n_start = 1)
  #using this instead of 'anonymise' avoids the loaded config file requirement

  movement_data <- replace_ids_w_key(movement_data, c(1,2), key)

  # replace_ids_w_key() does not have "as.character" as in the original below.
  # Needs testing.
  # movement_data[c(1,2)] <-
  #   lapply(c(1,2), function(x){unname(key[as.character(movement_data[[x]])])})

  if(!is.null(holding_data)){
    holding_data <-
      holding_data %>%
      filter_holding_data(holding_ids) %>%
      add_rows_to_holding_data(holding_ids) %>%
      replace_ids_w_key(1, key)
    # replace_ids_w_key() does not have "as.character" as in the original below.
    # Needs testing.
    # holding_data[1] <- unname(key[as.character(holding_data[[1]])])
  }

  return(list(key = key,
              movement_data = movement_data,
              holding_data = holding_data))
}

#' Filter a movenet holding data tibble for a set of holding identifiers
#'
#' `filter_holding_data()` is an internal helper function that removes
#' `holding_data` entries of which the holding identifiers do not appear in
#' `holding_ids`.
#'
#' @inheritParams movedata2networkDynamic
#' @param holding_ids A character vector with holding identifiers to keep.
#'
#' @returns A tibble like `holding_data`, but possibly with fewer rows. A warning
#' message is printed which lists any entries that have been removed.
#'
#' @keywords internal
filter_holding_data <- function(holding_data, holding_ids){
  if(any(!(holding_data[[1]] %in% holding_ids))){
    holding_ids_to_remove <- holding_data[[1]][which(!(holding_data[[1]] %in% holding_ids))]
    holding_data <-
      holding_data %>%
      dplyr::filter(.data[[names(holding_data)[1]]] %in% holding_ids)
    warning(paste0("The following holdings have been removed from holding_data: ",
                   paste0(holding_ids_to_remove, collapse = ", "),
                   "."),
            call. = FALSE)
  }
  return(holding_data)
}

#' Add a set of holding identifiers to a movenet holding data tibble
#'
#' `add_rows_to_holding_data()` is an internal helper function that adds entries
#' to `holding_data`, using `holding_ids` as identifiers, and `NA` for any other
#' columns.
#'
#' @inheritParams movedata2networkDynamic
#' @param holding_ids A character vector with holding identifiers to add.
#'
#' @returns A tibble like `holding_data`, but possibly with more rows. A warning
#' message is printed which lists any entries that have been added.
#'
#' @keywords internal
add_rows_to_holding_data <- function(holding_data, holding_ids){
  missing_holding_ids <- !(holding_ids %in% holding_data[[1]])
  if(any(missing_holding_ids)){
    holding_data <-
      holding_data %>%
      add_row("{names(holding_data)[1]}" :=
                holding_ids[which(missing_holding_ids)])
    warning(paste0("The following holding identifiers have been added to holding_data: ",
                   paste0(holding_ids[which(missing_holding_ids)], collapse = ", "),
                   ". Any additional data fields have been set to NA for these holdings."),
      call. = FALSE)
  }
  return(holding_data)
}
