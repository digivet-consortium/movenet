### To do:
# Make data2contactmatrix output key
# arg checks - where to allow missing values?
# diagonal set to 0 for movement_spread, local_spread, and overall matrices (but not additional matrices)
# Remove matrix filtering for additional tm prob matrices?
# Separate out matrix processing for additional tm prob matrices into function?
# Make saved local_spread tibble default for local_spread_probabilities?

#' Title
#'
#' @param movement_data a movenet-format movement data tibble
#' @param holding_data a movenet-format holding data tibble (optional)
#' @param incl_nonactive_holdings whether to include non-active holdings
#'   (default `FALSE`)
#' @param weight_unit_transmission_probability transmission probability per
#'   unit weight moved (default 1)
#' @param whole_months whether the movement data covers full months (default
#'   `TRUE`). This affects calculation of daily average weights, and thus of
#'   movement-based transmission probabilities. See @details
#' @param local_spread_probabilities dataframe/tibble of tiered local spread
#'   probabilities. 3 columns: lower_boundary (in metres), upper_boundary (in
#'   metres), probability
#' @param additional_transmission_prob_matrices named list with any additional
#'   transmission probability matrices to include
#'
#' @return a matrix of size number of holdings x number of holdings with overall
#' transmission probabilities, based on movements, (optionally) local spread,
#' and (optionally) any other user-provided transmission probabilities.
#'
#' @details
#'
#' @importFrom sf st_is_empty
#' @export
data2contactmatrix <- function(movement_data, holding_data = NULL,
                               #general options
                               incl_nonactive_holdings = TRUE,
                               accept_missing_coordinates = FALSE,
                               #options regarding movement spread matrix
                               weight_unit_transmission_probability = 1,
                               whole_months = TRUE,
                               #options regarding local spread matrix
                               local_spread_transmission_probabilities = NULL, #change default to saved file in inst/extdata?
                               #option to allow additional matrices
                               additional_transmission_prob_matrices = NULL){

  #######################
  ### Argument checks ###
  #######################

  assert_tibble(movement_data, all.missing = FALSE, min.cols = 4)
  assert(
    check_character(movement_data[[1]], any.missing = FALSE, all.missing = FALSE),
    check_character(movement_data[[2]], any.missing = FALSE, all.missing = FALSE),
    check_date(movement_data[[3]], any.missing = FALSE, all.missing = FALSE),
    check_numeric(movement_data[[4]], lower = 0, finite = TRUE,
                  any.missing = FALSE, all.missing = FALSE),
    combine = "and"
  )
  assert_tibble(holding_data, all.missing = FALSE, min.cols = 2, null.ok = TRUE)
  if(!is.null(holding_data)){
    assert(
      check_character(holding_data[[1]], any.missing = FALSE, all.missing = FALSE,
                      unique = TRUE), #Must these be unique? And otherwise what?
      check_names(names(holding_data), must.include = "coordinates"),
      check_data_frame(local_spread_transmission_probabilities), #i.e. this can't be null
      check_tibble(holding_data["coordinates"], types = "sfc_POINT"),
      combine = "and")
    if(all(st_is_empty(holding_data$coordinates))){ #replaces the all.missing assertion, as missing coordinates are not coded as NAs but as empty geometries
      stop("Assertion on 'holding_data[\"coordinates\"]' failed: Contains only empty geometries (missing coordinates).", call. = FALSE)
    }
    else if(isFALSE(accept_missing_coordinates) && any(st_is_empty(holding_data$coordinates))){
      stop(paste0("Assertion on 'holding_data[\"coordinates\"]' failed: Contains",
                  " empty geometries (missing coordinates).\nTo proceed with missing",
                  " coordinates, use `accept_missing_coordinates = TRUE`; this will",
                  " set local spread transmission probabilities to/from holdings",
                  " with missing coordinates to 0.\nCoordinates missing for the ",
                  "following holding(s): ",
                  paste0(holding_data[[1]][which(st_is_empty(holding_data$coordinates))], collapse = ", "),
                  "."),
           call. = FALSE)
    }
  }
  assert_logical(incl_nonactive_holdings, any.missing = FALSE,
                 all.missing = FALSE, len = 1)
  assert_logical(accept_missing_coordinates, any.missing = FALSE,
                 all.missing = FALSE, len = 1)
  assert_double(weight_unit_transmission_probability, lower = 0, upper = 1,
                finite = TRUE, any.missing = FALSE, all.missing = FALSE, len = 1)
  assert_logical(whole_months, any.missing = FALSE, all.missing = FALSE, len = 1)
  assert_data_frame(local_spread_transmission_probabilities, any.missing = FALSE,
                    all.missing = FALSE, min.cols = 3, null.ok = TRUE)
  if(!is.null(local_spread_transmission_probabilities)){
    # How about using ASF local spread tiers as default?
    assert(
      check_names(names(local_spread_transmission_probabilities), must.include =
                    c("lower_boundary", "upper_boundary", "probability")),
      check_integerish(local_spread_transmission_probabilities[["lower_boundary"]],
                       lower = 0, any.missing = FALSE, all.missing = FALSE),
      check_numeric(local_spread_transmission_probabilities[["upper_boundary"]],
                    lower = 0, any.missing = FALSE, all.missing = FALSE), #contains Inf -> not integerish
      check_numeric(local_spread_transmission_probabilities[["probability"]],
                    lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE),
      check_tibble(holding_data), #i.e. this can't be null
      combine = "and")
  }
  assert_list(additional_transmission_prob_matrices, types = "matrix",
              any.missing = FALSE, all.missing = FALSE, names = "named", null.ok = TRUE)
  if(!is.null(additional_transmission_prob_matrices)){
    invisible(
      lapply(seq_along(additional_transmission_prob_matrices), function(x){
        assert_double(additional_transmission_prob_matrices[[x]], lower = 0,
                      upper = 1, all.missing = FALSE, #any.missing = FALSE?
                      .var.name = names(additional_transmission_prob_matrices)[[x]])}))
    #check dimnames for each matrix - that they are included in known holding ids (from movement/holding data)?
  }

  ######################################################################
  ### Replace holding_ids with consecutive ints, in character format ###
  ######################################################################

  outputs <- holdingids2consecints(movement_data, holding_data,
                                   incl_nonactive_holdings)
  movement_data_intchar <- outputs$movement_data
  holding_data_intchar <- outputs$holding_data
  key <- outputs$key

  #replace dimnames of any additional transmission prob matrices
  if(!is.null(additional_transmission_prob_matrices)){
    lapply(additional_transmission_prob_matrices, function(m){
      dimnames(m) <- replace_ids_w_key(dimnames(m), c(1,2), key)
      #This results in dimnames containing NA for any holding_ids not in key.
      #Hence, subset matrix to relevant holding_ids only.
      #If don't want to exclude additional holding_ids, remove the below line,
      #and run add_rows_to_holding_data before holdingids2consecints.
      m <- m[na.omit(rownames(m)), na.omit(colnames(m)), drop = FALSE]})
  }

  ################################################
  ### Create transmission probability matrices ###
  ################################################

  movement_spread_matrix <-
    create_movement_spread_matrix(movement_data_intchar,
                                  weight_unit_transmission_probability,
                                  whole_months)

  if (!is.null(holding_data_intchar)){
    local_spread_matrix <- create_local_spread_matrix(holding_data_intchar,
                                                      local_spread_transmission_probabilities,
                                                      accept_missing_coordinates)
  } else {
    local_spread_matrix <- NULL
  }

  #ASF probability tiers saved under "inst/extdata/local_spread_probabilities_ASF_Halasa_et_al_2016.Rdata"
  # - how to use this by default if holding_data is optional?!

  overall_transmission_matrix <-
    combine_transmission_matrices(append(additional_transmission_prob_matrices,
                                         list(movement_spread_matrix,
                                              local_spread_matrix)))

  return(overall_transmission_matrix)
}


create_movement_spread_matrix <- function(movement_data,
                                          weight_unit_transmission_probability = 1,
                                          whole_months = TRUE){

  node_ids <- unique(c(movement_data[[1]], movement_data[[2]]))
  n_nodes <- length(node_ids)

  #0-matrix w all possible combinations of holdings in movement_data
  movement_matrix <-
    matrix(0, n_nodes, n_nodes, dimnames = list(node_ids, node_ids))

  average_movement_data <-
    average_daily_weights(movement_data, whole_months = TRUE)

  movement_matrix[cbind(average_movement_data[[1]], #from
                        average_movement_data[[2]])] <- #to
    average_movement_data[[3]] #average_daily_weight

  movement_spread_matrix <-
    1 - (1 - weight_unit_transmission_probability)^movement_matrix
  # probability of infection through movement = 1 - probability of no infection through any moved unit
  # !NOT: movement_spread_matrix <- movement_matrix * weight_unit_transmission_probability !

  movement_spread_matrix <-
    movement_spread_matrix[order(as.numeric(rownames(movement_spread_matrix))),
                           order(as.numeric(colnames(movement_spread_matrix)))]
  #This doesn't work for non-numeric holding ids

  diag(movement_spread_matrix) <- 0 #probability of infecting oneself through movement = 0

  return(movement_spread_matrix) #n holdings in movement_data x n holdings in movement_data
}

#' @importFrom dplyr group_by summarise
#' @importFrom lubridate ceiling_date floor_date
average_daily_weights <- function(movement_data, whole_months = TRUE){

  #doesn't assume or require from/to to be in integer/"integer"  format, i.e.
  #accepts any movenet format movement data

  #set timeframe of movement data
  if(isTRUE(whole_months)){ #assume movement_data covers whole months
    n_days <- as.numeric(ceiling_date(max(movement_data[[3]]),"month") -
                           floor_date(min(movement_data[[3]]),"month"))
  } else if(isFALSE(whole_months)) { #count days from first to last movement
    n_days <- as.numeric(max(movement_data[[3]]) -
                           min(movement_data[[3]])) + 1
  }

  #average number of pigs transported per day between each farm
  #(NB this NAMES the new column average_daily_weight)
  #(NB this drops other columns - output is just from, to, average_daily_weight)
  movement_data %>%
    group_by(.[,1], .[,2]) %>%
    summarise(
      average_daily_weight = sum(eval(sym(names(movement_data)[[4]])))/n_days)

  #output is summarised movement tibble
}

#' @importFrom units drop_units
create_local_spread_matrix <- function(holding_data,
                                       local_spread_probability_tiers,
                                       accept_missing_coordinates){

  if(isFALSE(accept_missing_coordinates) && any(st_is_empty(holding_data$coordinates))){
    stop(paste0("Assertion on 'holding_data[\"coordinates\"]' failed: Contains",
                " empty geometries (missing coordinates).\nTo proceed with missing",
                " coordinates, use `accept_missing_coordinates = TRUE`; this will",
                " set local spread transmission probabilities to/from holdings",
                " with missing coordinates to 0."),
         call. = FALSE)
  }

  #Create distance matrix without units
  local_spread_matrix <-
    holding_data %>%
    create_distance_matrix %>%
    drop_units()

  #Replace distances with probabilities from look-up table
  apply(local_spread_probability_tiers, 1, function(tier){
    local_spread_matrix[local_spread_matrix >= tier['lower_boundary'] & #define columns by header or index?
                          local_spread_matrix < tier['upper_boundary']] <<-
      tier['probability']
  })

  local_spread_matrix <-
    local_spread_matrix[order(as.numeric(rownames(local_spread_matrix))),
                           order(as.numeric(colnames(local_spread_matrix)))]
  #This doesn't work for non-numeric holding ids

  diag(local_spread_matrix) <- 0 #probability of infecting oneself through local spread = 0

  if(isTRUE(accept_missing_coordinates) && sum(is.na(local_spread_matrix)) > 0){
    local_spread_matrix[which(is.na(local_spread_matrix))] <- 0
    warning(paste("Local spread transmission probabilities could not be determined",
                  "for all pairs of holdings, due to some missing coordinates.",
                  "Local spread transmission probabilities to/from holdings with",
                  "missing coordinates have been set to 0."),
            call. = FALSE)
  }

  return(local_spread_matrix) #n holdings in holding_data x n holdings in holding_data
}


#' @importFrom sf st_as_sf st_distance
create_distance_matrix <- function(holding_data){

  #distance matrix between farms
  distance_matrix <-
    holding_data %>%
    st_as_sf(sf_column_name = "coordinates") %>% #the whole tibble needs to be converted to sf for st_distance to work
    st_distance() #dimnames are index numbers, not holding_ids

  dimnames(distance_matrix) <- list(holding_data[[1]],holding_data[[1]])

  return(distance_matrix) #n holdings in holding_data x n holdings in holding_data
}

combine_transmission_matrices <- function(matrices){

  ## Ensure matrix dimensions (n_nodes) and rows/col ordering are consistent ##

  #movement_spread_matrix has active nodes only
  #local_spread_matrix has all nodes in holding data
  #additional matrices have only (but not all) nodes that are in holding_data

  #identifies node_ids from dimnames of all matrices -> all nodes in holding_data
  node_ids <- unique(unlist(lapply(matrices, function(x){dimnames(x)})))
  n_nodes <- length(node_ids)

  #create 0-matrix with dimensions n_nodes x n_nodes
  m0 <- matrix(0, n_nodes, n_nodes, dimnames = list(node_ids, node_ids))

  matrices <- lapply(matrices, function(x){
    #superimpose matrices onto 0-matrix to make dimensions consistent;
    #assumes transmission from movement or from additional routes is 0 if not
    #included in these matrices
    m0[rownames(x), colnames(x)] <- x
    #Ensure matrix rows and columns are ordered consistently
    m0[order(as.numeric(rownames(m0))), order(as.numeric(colnames(m0)))]
  })

  ## Add up probabilities ##

  #NOT simply the sum of various matrices!!
  #Probability of ANY of the infectious pressures = 1 - Probability of none
  #PRobability of NONE of the infectious pressures = product of (1-Prob) for all processes

  #Transform matrices from probability to 1-probability
  complementary_matrices <- lapply(matrices, function(x){1-x})
  #Multiply matrices and transform back from 1-probability to probability
  #Overall probability = 1 - (1-Pr(A))*(1-Pr(B))*(1-Pr(C))*etc.
  overall_matrix <- 1 - Reduce("*", complementary_matrices)

  overall_matrix <-
    overall_matrix[order(as.numeric(rownames(overall_matrix))),
                        order(as.numeric(colnames(overall_matrix)))]

  diag(overall_matrix) <- 0 #probability of infecting oneself = 0

  return(overall_matrix)

}

