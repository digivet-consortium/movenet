#' Create a dynamic network representation of movenet-format movement data
#'
#' `movedata2networkDynamic()` converts movenet-format movement and (optional)
#' holding data tibbles into a networkDynamic temporal network representation.
#' This assumes the network is directed with no loops, no hyperedges, and no
#' multiplex edges.
#'
#' @details
#' ## Node and holding identifiers
#' If nodes identifiers are not consecutive integers:
#' For compatibility with networkDynamic, active nodes (holdings) are assigned
#' identifiers consisting of an integer between 1 and the number of active holdings in
#' the network, in random order.
#' If `incl_nonactive_holdings == TRUE`, non-active holdings are added to the network
#' with identifiers ranging from the number of active holdings+1 to the total number of holdings.
#' #' Original holding identifiers are stored as vertex attribute `true_id`
#' and set as [<`persistent identifiers`>][networkDynamic::persistent.ids].
#'
#'
#' @param movement_data A movenet-format movement data tibble
#' @param holding_data A movenet-format holding data tibble (optional)
#' @param incl_nonactive_holdings A logical that indicates whether to include
#'   holdings from `holding_data` that are not present in `movement_data`.
#'   Default is `FALSE`. If set to `TRUE`, holdings that don't trade within the
#'   period covered by `movement_data` are added to the network but set as
#'   non-active.
#'
#' @returns
#' A networkDynamic object consisting of a directed temporal network,
#' with nodes representing holdings and edges representing connections between
#' holdings. Moves are represented as edge spells (periods over which edges are
#' considered active), with a duration of 0 each. Nodes are set to be active
#' only during such edge spells.
#'
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

  node_ids <- unique(c(movement_data[[1]],movement_data[[2]]))

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

  if(!is.null(holding_data)){

    #If there are any holding ids present in movement_data, but missing from
    #holding_data, add these ids to holding_data with NAs for other columns
    missing_holding_ids <- !(node_ids %in% holding_ids)
    if(any(missing_holding_ids)){
      holding_data <-
        holding_data %>%
        add_row("{names(holding_data)[1]}" :=
                  node_ids[which(missing_holding_ids)])
      holding_ids <- holding_data[[1]]
    }

    #If there are any holding ids present in holding_data but missing from
    #movement_data, either delete these ids (if incl_nonactive_holdings == FALSE),
    #or split these to form a new additional_holding_data dataframe, to be
    #added as (non-active) vertices after network creation (if
    #incl_nonactive_holdings == TRUE)
    additional_holding_ids <- !(holding_ids %in% node_ids)
    if(any(additional_holding_ids)){
      if(isTRUE(incl_nonactive_holdings)){
        additional_holding_data <-
          holding_data %>%
          dplyr::filter(!(.data[[names(holding_data)[1]]] %in% node_ids))
      }
      holding_data <-
        holding_data %>%
        dplyr::filter(.data[[names(holding_data)[1]]] %in% node_ids)
    }
  }

  #############################################
  ### Ensure correct node identifier format ###
  #############################################

  #networkDynamic needs vertex.ids to be consecutive integers from 1 to n_nodes.
  #Check if holding ids are consecutive "integers" (in character format is ok),
  #and if this is not the case, renumber and save the key.
  #(The key is later used to generate a "true_id" vertex attribute, and to set
  #vertex.pids (persistent identifiers))

  if(isFALSE(are_ids_consec_intchars_from_1(node_ids))){

    output <- holdingids2consecints(movement_data, holding_data,
                                    incl_nonactive_holdings = FALSE)
    movement_data <- output$movement_data
    holding_data <- output$holding_data
    key <- output$key

  }

  ########################################
  ### Reformat data and create network ###
  ########################################

  #Reformat data to the specific column order, and integer vertex.ids and dates,
  #required by networkDynamic

  movement_data[1:3] <- movement_data[1:3] |> lapply(as.numeric)
  movement_data <-
    movement_data[,c(3,3,1,2,4:length(movement_data))] |>
    data.frame(stringsAsFactors = FALSE)

  #Create activity spells for nodes, to avoid very slow reconcile.vertex.activity
  vertex_spells <- create_vertex_spells(movement_data)

  #Create the network
  net <- networkDynamic(edge.spells = movement_data,
                        vertex.spells = vertex_spells,
                        verbose = FALSE, create.TEAs = TRUE,
                        edge.TEA.names = names(movement_data)[-c(1:4)])
  #Allow multiplex graphs? (Default = FALSE)
  #This may cause trouble with certain measures. Edge spells over time will
  #cover most cases, but what if multiple moves betw same farms on 1 day?
  #Allow loops? (Default = FALSE)

  #######################################
  ### Set node persistent identifiers ###
  #######################################

  #vertex.pids (persistent identifiers) are needed to reliably identify nodes
  #when extracting subnetworks, as vertex.ids are re-numbered to match the
  #network size (i.e. vertex.ids are always 1:n_nodes; whereas vertex.pids
  #remain the same throughout extractions and can be non-int/non-consecutive).

  #if have key, add names (original holding ids) as vertex attrib "true_id"
  if(exists("key", where = environment(), inherits = FALSE)){
    set.vertex.attribute(net, 'true_id', names(key))
    warning(str_wrap("Node identifiers (vertex.id) have been changed to
    consecutive integers. Original identifiers have been set as persistent
    identifiers (vertex.pid) and can be identified for each node by running
    `get.vertex.pid(network_name, vertex.id(s))`."))
  } else {
  #else, convert vertex.names (original holding ids if consecutive ints) to
  #character and set these as vertex attrib "true_id" [for consistency]
    set.vertex.attribute(net, 'true_id',
                         as.character(get.vertex.attribute(net,'vertex.names')))
  }
  #set true_id attribute as vertex.pid
  set.network.attribute(net,'vertex.pid','true_id')

  ###########################
  ### Set node attributes ###
  ###########################

  if(!is.null(holding_data)){
    holding_data <- holding_data[order(as.integer(holding_data[[1]])),]
    set.vertex.attribute(net, names(holding_data)[-1], holding_data[-1])
  }

  ###########################################
  ### Add any non-active nodes to network ###
  ###########################################

  #If incl_nonactive_holdings == TRUE, add any non-active holdings to network,
  #creating (automatic) new numeric identifiers, setting their original
  #identifiers as true_id vertex.attribute and as vertex.pids, and adding
  #other vertex.attributes from additional_holding_data

  if(exists("additional_holding_data", where = environment(),
            inherits = FALSE)){
    names(additional_holding_data)[1] <- "true_id"
    additional_holding_data$vertex.names <-
      nrow(holding_data)+(1:nrow(additional_holding_data))
    add.vertices(net, nv = sum(additional_holding_ids),
                 vattr = lapply(split(additional_holding_data,
                                      1:nrow(additional_holding_data)),
                                as.list),
                 vertex.pid = additional_holding_data[[1]])
    deactivate.vertices(net, v = c((nrow(holding_data)+1):network.size(net)))
  }

  ######################
  ### Return network ###
  ######################

  #return network w/ true_id and vertex.pid containing original holding ids in
  #character format
  return(net)
}

#' Helper function: Create activity spells for nodes, to avoid
#' very slow reconcile.vertex.activity
#'
#' @param movement_data The movement data tibble
#'
#' @returns the vertex spells ready to be passed into networkDynamic()
#'
#' @keywords internal
create_vertex_spells <- function(movement_data){
  vertex_spells <- movement_data[,c(1,2,3)] %>%
    `colnames<-`(colnames(movement_data[,c(1,2,4)]))
  vertex_spells <- rbind(vertex_spells, movement_data[,c(1,2,4)]) %>%
    sort() %>% unique()
  return(vertex_spells)
}

#' Are node ids consecutive "integers" (in character format), starting from 1?
#'
#' Checks whether node identifiers are consecutive "integers" (in character
#' format), ranging from 1 to the total number of node ids.
#' This is to facilitate conversion of movenet data tibbles into formats
#' required by networkDynamic and SimInf, which need node identifiers to be
#' consecutive integers starting from 1.
#'
#' @param node_ids vector of node IDs (character format)
#'
#' @returns a boolean - TRUE if the IDs need to be replaced, false otherwise
#'
#' @keywords internal
are_ids_consec_intchars_from_1 <- function(node_ids){
  return(
    all(grepl("^\\d+$", node_ids)) && #char strings consisting of only digits...
      identical(sort(as.integer(node_ids)), 1:length(node_ids))
      #...which are consecutive integers from 1 to the total number of ids
  )
}

#' Replaces node ids with consecutive "integers" (in character format), starting
#' from 1.
#'
#' Replaces node ids with consecutive "integers" (in character format), ranging
#' from 1 to the total number of node ids. This facilitates further conversion
#' of movenet data tibbles into formats required by networkDynamic and SimInf,
#' which need node identifiers to be consecutive integers starting from 1.
#' Also returns a "key" that links original ids and new "integer" ids, so that
#' original ids can be added as node attributes to networkDynamic and SimInf
#' outputs.
#'
#' @param movement_data Movenet format movement data tibble
#' @param holding_data Movenet format holding data tibble (optional)
#' @param incl_nonactive_holdings Whether to include holdings from
#'   `holding_data` that are not present in `movement_data`. Default is `FALSE`.
#'
#' @returns a named list with `key`, the modified `movement_data` tibble
#' `movement_data` tibble) and `holding_data` (the modified `holding_data`
#'  tibble if present, or `NULL` otherwise)
#'
#' @keywords internal
holdingids2consecints <- function(movement_data, holding_data = NULL,
                                  incl_nonactive_holdings = FALSE){

  #Define node_ids dependent on whether to only include active holdings (present
  #in movement_data) or to also include non-active holdings (present in
  #holding_data but not movement_data)
  if(isFALSE(incl_nonactive_holdings)){
    node_ids <- unique(c(movement_data[[1]], movement_data[[2]]))
  } else if(isTRUE(incl_nonactive_holdings)){
    node_ids <-
      unique(c(movement_data[[1]], movement_data[[2]], holding_data[[1]]))
    }


  key <- generate_anonymisation_key(node_ids, prefix = "", n_start = 1)
  #using this instead of 'anonymise' avoids the loaded config file requirement

  movement_data <- replace_ids_w_key(movement_data, c(1,2), key)

  # replace_ids_w_key() does not have "as.character" as in the original below.
  # Needs testing.
  # movement_data[c(1,2)] <-
  #   lapply(c(1,2), function(x){unname(key[as.character(movement_data[[x]])])})

  if(!is.null(holding_data)){
    holding_data <-
      holding_data %>%
      filter_holding_data(node_ids) %>%
      add_rows_to_holding_data(node_ids) %>%
      replace_ids_w_key(1, key)
    # replace_ids_w_key() does not have "as.character" as in the original below.
    # Needs testing.
    # holding_data[1] <- unname(key[as.character(holding_data[[1]])])
  }

  return(list(key = key,
              movement_data = movement_data,
              holding_data = holding_data))
}

#If there are any holding ids present in holding_data but missing from node_ids,
#delete these ids
#' @keywords internal
filter_holding_data <- function(holding_data, node_ids){
  if(any(!(holding_data[[1]] %in% node_ids))){
    holding_ids_to_remove <- holding_data[[1]][which(!(holding_data[[1]] %in% node_ids))]
    holding_data <-
      holding_data %>%
      dplyr::filter(.data[[names(holding_data)[1]]] %in% node_ids)
    warning(paste0("The following non-active holdings have been removed from holding_data: ",
                   paste0(holding_ids_to_remove, collapse = ", "),
                   "."),
            call. = FALSE)
  }
  return(holding_data)
}

#If there are any holding ids present in node_ids, but missing from
#holding_data, add these ids to holding_data with NAs for other columns
#' @keywords internal
add_rows_to_holding_data <- function(holding_data, node_ids){
  missing_holding_ids <- !(node_ids %in% holding_data[[1]])
  if(any(missing_holding_ids)){
    holding_data <-
      holding_data %>%
      add_row("{names(holding_data)[1]}" :=
                node_ids[which(missing_holding_ids)])
    warning(paste0("The following holding identifiers have been added to holding_data: ",
                   paste0(node_ids[which(missing_holding_ids)], collapse = ", "),
                   ". Any additional data fields have been set to NA for these holdings."),
      call. = FALSE)
  }
  return(holding_data)
}


#' Extract max reachabilities in parallel
#'
#' @param networks list of movement networks
#' @param n_threads
#'
#' @return
#'
#' @importFrom parallel makeCluster clusterEvalQ stopCluster
#' @importFrom pbapply pbsapply
#' @importFrom tsna tReach
#'
#' @export
parallel_max_reachabilities <- function(networks, n_threads){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
  })

  max_reachabilities <-
    pbsapply(networks,
              function(x){max(tReach(x, graph.step.time = 1))}, cl=cl)
  return(max_reachabilities)
}
# N.B. This step is super-slow without parallel processing.
# See https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html#building-a-socket-cluster
# Using a socket cluster, as mclapply doesn't work on Windows

#' Extract max reachabilities and associated node persistent identifiers, in parallel
#'
#' @param networks list of movement networks
#' @param n_threads
#'
#' @return list of lists: for each network, a list with (1) max reachability,
#' (2) the persistent identifiers of the node(s) with max reachability.
#'
#' @importFrom networkDynamic get.vertex.pid
#' @importFrom parallel makeCluster clusterEvalQ stopCluster
#' @importFrom pbapply pblapply
#' @importFrom tsna tReach
#'
#' @export
parallel_max_reachabilities_with_id <- function(networks, n_threads){
  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
    library("networkDynamic")
  })

  max_reachabilities_w_ids <-
    pblapply(networks,
             function(x){
               reachability <- tReach(x, graph.step.time = 1)
               max_reachability <- max(reachability)
               id_max_reachability <-
                 get.vertex.pid(x, which(reachability == max_reachability))
               list(max_reachability, id_max_reachability)
               },
             cl=cl)
  return(max_reachabilities_w_ids)
}

#' Extract summary stats for temporal network node properties, in parallel
#'
#' @param networks list of movement networks
#' @param n_threads number of threads over which to parallelise
#' @param node_property node property: one of c("forward reachability",
#'  "temporal degree", "temporal indegree", "temporal outdegree")
#' @param statistics list with summary function(s) to calculate, e.g.
#'  list(median, max = max).
#' @param identify_nodes whether you want to identify the nodes with maximal
#'  values for `node_property` in each network. Default is `FALSE`. Requires
#'  that max is included as a named function within `statistics`.
#'
#' @return named list of lists: for each network, a list consisting of:
#'  (1) a named list `summary_statistics`, with the selected node property
#'  summary statistic(s),
#'  (2 - only if `identify_nodes` is set as `TRUE`) a named character vector
#'  `node_pid_with_max_value`, with the persistent identifier(s) of the node(s)
#'  with maximal value(s) for the selected node property.
#'
#' @importFrom networkDynamic get.vertex.pid
#' @importFrom parallel makeCluster clusterEvalQ stopCluster
#' @importFrom pbapply pblapply
#' @importFrom tsna tReach tDegree
#'
#' @export
parallel_summarise_temporal_node_properties <-
  function(networks, n_threads, node_property, statistics,
           identify_nodes = FALSE){
    cl <- makeCluster(n_threads)
    on.exit(stopCluster(cl))

    clusterExport(cl, c("node_property", "statistics", "identify_nodes"),
                  environment())
    clusterEvalQ(cl, {
      library("tsna")
      library("networkDynamic")
    })

    ### NB need to add arg checks ###

    node_summary_stats <-
      pblapply(networks,
               function(x){
                 # Determine property for all nodes
                 property <- {
                   if(node_property == "forward reachability"){
                     tReach(x, graph.step.time = 1)
                   }else if(node_property == "temporal degree"){
                     colSums(tDegree(x, cmode = "freeman"), na.rm = TRUE)
                   }else if(node_property == "temporal indegree"){
                     colSums(tDegree(x, cmode = "indegree"), na.rm = TRUE)
                   }else if(node_property == "temporal outdegree"){
                     colSums(tDegree(x, cmode = "outdegree"), na.rm = TRUE)}}
                 # Calculate requested summary statistic(s)
                 summary_stats <- lapply(statistics, function(f)(f(property)))
                 # (Optionally) identify nodes with max/min values
                 if(isTRUE(identify_nodes)){
                   id_max_value <-
                     get.vertex.pid(x, which(property == summary_stats$max))
                 # Return summary stats, and optional node identifiers
                   return(list(summary_statistics = summary_stats,
                               node_pid_with_max_value = id_max_value))
                 }else{
                   return(list(summary_statistics = summary_stats))
                 }},
               cl=cl)
    return(node_summary_stats)
  }

#' Extract time periods covered in movement dataset
#'
#' @param data Date column of movement data
#' @param period time period for which to extract dates (n days, week, n weeks,
#'    month, n months, year, n years)
#'
#' @importFrom lubridate floor_date period
#'
#' @return List of c(start date, end date) for each period covered in the data,
#'    with dates in int format
#'
#' @details For periods of days or weeks, the first date in the data is used
#'    as starting date; for periods of months or years, the first day of the
#'    month, n months, year or n years is used as starting date. (OR make
#'    starting date an argument?)
#' @export
extract_periods <- function(data, period){
  if (isTRUE(grepl("(\\d+\\sdays)|((\\d+\\s)?week(\\s)?)",period))){
    start_dates <- seq(min(data), max(data), by = period)
  } else {
    start_dates <- seq(floor_date(min(data), period),
                       max(data),
                       by = period)
  }
  end_dates <- as.integer(start_dates + period(period))
  start_dates <- as.integer(start_dates)
  Map(c,start_dates,end_dates)
}

#' Extract periodic subnetworks from movement networks
#'
#' @param networks a named list of movement networks
#' @param n_threads
#' @param periods_in_data a list of start and end dates (in int format) for all periods in the data
#'
#' @return
#'
#' @importFrom networkDynamic network.extract
#' @importFrom parallel makeCluster clusterExport stopCluster
#' @importFrom pbapply pblapply
#'
#' @details Note from network.extract: Note that only active vertices are
#'    included by default (retain.all.vertices=FALSE). As a result, the size of
#'    the extracted network may be smaller than the original. Vertex and edge
#'    ids will be translated, but may not correspond to their original values.
#'    If it is necessary to maintain the identities of vertices, see
#'    persistent.ids. (Make this an argument? Add ... to pass on arguments?)
#'
#' @export
extract_periodic_subnetworks <- function(networks, n_threads, periods_in_data){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("network.extract","periods_in_data"), envir = environment())
  periodic_networks <-
    pblapply(networks,
              function(nw){
                lapply(periods_in_data, function(p) {
                  network.extract(nw, onset = p[[1]], terminus = p[[2]],
                                  rule = "any",
                                  #retain.all.vertices = TRUE,
                                  trim.spells = TRUE)})}, cl=cl)
  names(periodic_networks) <- names(networks)
  return(periodic_networks)
}



#' Draw violin plot of distributions of monthly values of a selected network measure, for various jittered & rounded networks
#'
#' @param monthly_data tibble with measure values, for monthly networks
#' @param measure_name measure name (to refer to in y axis)
#'
#' @return
#'
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @import ggplot2
#'
#' @export
violinplot_monthly_measures <- function(monthly_data, measure_name){

  #Reformat to long tibble for plotting
  monthly_measures <-
    monthly_data |>
    pivot_longer(everything(),
                 names_to = "network",
                 values_to = measure_name)

  #Set network as a factor with specific order (to avoid default alphabetic order)
  monthly_measures$network <-
    factor(monthly_measures$network,
           levels = unique(names(monthly_data)))

  p <-
    ggplot(data = monthly_measures,
           aes(x = network, y = .data[[measure_name]])) +
    xlab("Movement network") +
    ylab(paste("Monthly", measure_name)) +
    ylim(0, NA) +
    scale_x_discrete(labels = function(x) str_wrap(as.character(x), width = 9))+
    geom_violin(trim = TRUE) +
    theme_bw()

  p <- p + geom_boxplot(width = 0.1)

  plot(p)
}

#' Plot values of a measure for a varying range of jitter or rounding
#'
#' @param data tibble with measure values for range of jitter or rounding
#' @param measure_name measure name (to refer to in y axis)
#' @param anonymisation "jitter" or "round(ing)"
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
plot_measure_over_anonymisation_gradient <-
  function(data, measure_name, anonymisation){
    datacols <- colnames(data)
    anon_amount <- colnames(data)[1]
    measure <- colnames(data)[2]
    p <-
      ggplot(data = data,
             aes(x = .data[[anon_amount]], y = .data[[measure]])) +
      xlab(ifelse(anonymisation == "jitter", "Jitter (days)",
                  "Rounding unit equivalent (days)")) +
      ylab(measure_name) +
      ylim(0, NA) +
      theme_bw()

    if(anonymisation == "jitter"){
      p <- p + geom_boxplot(aes(group = .data[[anon_amount]]))
    } else {
      p <- p + geom_point()}

    plot(p)
  }
