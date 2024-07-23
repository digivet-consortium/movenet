# Currently if there are contact chains for some but not all roots, the function prints a message and prints
# a maps with the contact chains and with all root holdings.
# But if there are no contact chains for any roots, no map is produced.
# Is a map with just the root holding markers but no contact chains still valuable?

# The standard EpiContactTrace output is quite informative, with a summary of dates and the numbers of contact chains,
# but it also prints all the chains which can get long.
# Is there value to printing the contact_trace_object (either full or summary-only) on screen but not "returning" it?
# It's perhaps a bit complicated when the output is a list of the contact_trace_object and the leaflet widget.


# Test for multiple roots, tEnd, days // inBegin etc.
# Test if root not in movement_data
# Test if root not in holding_data
# Test if not all movement_data holdings in holding_data or not all have coordinates
# Test with the same holding being both root and in, root and out, in and out
# Test with the same holding occurring multiple times along a single contact chain
# Test with the same movement being part of ingoing and outgoing contact chains
# Test with there being only ingoing or only outgoing contact chains, with and without map
#   trace_contact_chains(example_movement_data, example_holding_data, "95/216/1100", "2019-05-01", 15)
#   The above only has ingoing contact chains, no outgoing

# Need to make sure that all identified holdings have coordinates... or what?
#
# Test that ContactTrace2movedata() and ContactTrace2holdingdata() work when
# contact_tracing_results includes a holding or connection multiple times, as both "in" and "out".
# -> to create movement_data for testing this:
# structure(list(departure_cph = c("95/216/1100", "95/216/1100",
# "69/196/5890", "52/577/5349", "39/103/5541", "41/788/6464"),
# dest_cph = c("69/196/5890", "52/577/5349", "39/103/5541",
#              "39/103/5541", "41/788/6464", "69/196/5890"),
# departure_date = structure(c(17897, 17898, 17899, 17899, 17897, 17897),
#                            class = "Date"),
# qty_pigs = c(1, 2, 3, 4, 5, 6)), row.names = c(NA, -6L),
# class = c("tbl_df", "tbl", "data.frame"))
# This should not result in Error.

# Test:
# trace_contact_chains(head(example_movement_data, 10), head(holding_data, 10),"95/216/110",2019-02-08,100)
# results in Error - Assertion on 'tEnd' failed: Must be of class 'Date', not 'double'.
# (but actually it can be of class character as well. Lines 205-9)
#---
# trace_contact_chains(head(example_movement_data, 10), head(holding_data, 10),"95/216/110","2019-02-08",100)
# results in Error in build_tree2(.)- identical(length(root), 1L) is not TRUE
# (this is because I mistyped the root holding id, should be "95/216/1100" not "95/216/110")
# Error occurs within ContactTrace2holdingdata(). ContactTrace2movedata() gives an empty tibble.
# What should happen if root not in movement_data or not in holding_data?
#---
# trace_contact_chains(head(example_movement_data, 10), head(holding_data, 10),"95/216/1100","2019-02-08",100)
# results in Error in pointData.default(sanitize_sf(obj)) :
# Don't know how to get location data from object of class sfc_GEOMETRYCOLLECTION,sfc
#---

# I use snake_case in my functions -but in trace_contact_chains I have copied
# across several arguments from EpiContactTrace, which uses camelCase. Adapt to
# snake_case or leave as they are?

# Would actually like holding_data as optional argument, using EpiContactTrace
# positioning of nodes, but this currently is buggy...


#' Trace ingoing and outgoing contact chains and visualise data on a Leaflet map
#'
#' `trace_contact_chains()` determines ingoing and outgoing contact chains for
#' one or more `root` holdings over a specified time period, and then plots
#' these on an interactive Leaflet map. If a map with administrative areas is
#' provided, contact chain data are aggregated geographically, and the resulting
#' map can be coloured by the total weight or number of ingoing movements from
#' or outgoing movements to each area.
#'
#' @param movement_data A movenet-format movement tibble.
#' @param holding_data A movenet-format holding tibble that includes a
#'   "coordinates" column.
#' @param root A vector of root holdings to perform contact tracing for.
#' @inheritParams EpiContactTrace::Trace
#' @param admin_areas_map An sf dataframe with administrative area boundaries.
#' @param colour_domain_weight_in A numeric vector representing the range (i.e.
#'   `c(min, max)`) of possible aggregate ingoing weight values to colour
#'   administrative areas by. If `NULL` (default), the range will be determined
#'   from the data.
#' @param colour_domain_weight_out A numeric vector representing the range (i.e.
#'   `c(min, max)`) of possible aggregate outgoing weight values to colour
#'   administrative areas by.  If `NULL` (default), the range will be determined
#'   from the data.
#' @param colour_domain_moves_in A numeric vector representing the range (i.e.
#'   `c(min, max)`) of possible aggregate numbers of ingoing moves to colour
#'   administrative areas by. If `NULL` (default), the range will be determined
#'   from the data.
#' @param colour_domain_moves_out A numeric vector representing the range (i.e.
#'   `c(min, max)`) of possible aggregate numbers of outgoing moves to colour
#'   administrative areas by. If `NULL` (default), the range will be determined
#'   from the data.
#'
#' @details The time period can either be specified using `tEnd` and `days`, or
#' using `inBegin`, `inEnd`, `outBegin` and `outEnd`. If using `tEnd` and
#' `days`, the time period for ingoing and outgoing contacts ends at `tEnd` and
#' starts at `days` prior to `tEnd`. The tracing will be performed for each
#' combination of `root`, `tEnd` and `days`. An alternative way is to use
#' `inBegin`, `inEnd`, `outBegin` and `outEnd`. The time period for ingoing
#' contacts starts at `inBegin` and ends at `inEnd`. For outgoing contacts the
#' time period starts at `outBegin` and ends at `outEnd`. The vectors `root`,
#' `inBegin`, `inEnd`, `outBegin`, and `outEnd` must have the same lengths and
#' the tracing will be performed for each index of them.
#'
#' If `admin_areas_map` is provided, administrative area boundaries are added to
#' the Leaflet map. Additionally, contact chain data are summarised
#' geographically and the user can choose to colour the map by the total weight
#' or number of ingoing movements from or outgoing movements to each
#' administrative area. By default, the colour scales for these geographical
#' layers are determined by the ranges of the respective values in the data,
#' however they can be set via the `colour_domain_weight_in`,
#' `colour_domain_weight_out`, `colour_domain_moves_in`, and/or
#' `colour_domain_moves_out` arguments in order to make colours comparable
#' across maps.
#'
#' @returns A Leaflet map (HTML widget object) with graphics layers for holdings
#'   and movements that are part of ingoing and outgoing contact chains, and for
#'   administrative area data if `admin_areas_map` is provided. Further layers
#'   can be added using `%>%`.
#'
#' @export
#'
#' @examples
#'
#' @seealso
#' * [EpiContactTrace::Trace()] for the underlying contact chain tracing functionality.
#' * [leaflet::leaflet()] for the underlying mapping functionality.
#'
trace_contact_chains <- function(movement_data, holding_data,
                                 root,
                                 tEnd = NULL,
                                 days = NULL,
                                 inBegin = NULL,
                                 inEnd = NULL,
                                 outBegin = NULL,
                                 outEnd = NULL,
                                 maxDistance = NULL,
                                 admin_areas_map = NULL,
                                 colour_domain_weight_in = NULL,
                                 colour_domain_weight_out = NULL,
                                 colour_domain_moves_in = NULL,
                                 colour_domain_moves_out = NULL){

# Argument checks ---------------------------------------------------------

  assert_data_frame(movement_data, min.cols = 4, null.ok = FALSE)
  assert_character(movement_data[[1]], any.missing = FALSE)
  assert_character(movement_data[[2]], any.missing = FALSE)
  assert_date(movement_data[[3]], any.missing = FALSE)
  assert_numeric(movement_data[[4]], any.missing = TRUE)

  assert_data_frame(holding_data, min.cols = 1, null.ok = FALSE)
  #check that holding_ids are unique
  assert_character(holding_data[[1]], unique = TRUE, any.missing = FALSE)
  #check that holding_data has a coordinates column, of class sfc
  assert_names(names(holding_data),
               must.include = "coordinates",
               .var.name = "holding_data")
  assert_class(holding_data$coordinates, c("sfc_POINT","sfc"))

  assert_class(admin_areas_map, c("sf","data.frame"), null.ok = TRUE)

  #check that colour_domain_* arguments are numeric vectors of length 2 [c(min,
  # max)] or NULL
  assert_numeric(colour_domain_weight_in, lower = 0, finite = TRUE,
                 any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  assert_numeric(colour_domain_weight_out, lower = 0, finite = TRUE,
                 any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  assert_numeric(colour_domain_moves_in, lower = 0, finite = TRUE,
                 any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  assert_numeric(colour_domain_moves_out, lower = 0, finite = TRUE,
                 any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  #if any colour_domain_* arguments are given (i.e. not NULL), but admn_areas_map
  #is NULL, print message that these arguments will be ignored
  if(is.null(admin_areas_map) &
     any(!is.null(colour_domain_weight_in),
         !is.null(colour_domain_weight_out),
         !is.null(colour_domain_moves_in),
         !is.null(colour_domain_moves_out))) {
    warning("Arguments 'colour_domain_weight_in', 'colour_domain_weight_out', ",
            "'colour_domain_moves_in', and 'colour_domain_moves_out' will be ",
            "ignored as no 'admin_areas_map' is provided.")
  }

  # The lines below mostly paraphrase EpiContactTrace::Trace's arg checks,
  # except for the check for vector length that is not included here, and I have
  # added some checks and warnings for duplicate value.
  # Argument checks copied to (1) remove the reference to "Trace" in
  # custom_error_message below, and (2) to have consistent checkmate error
  # messages throughout (most of) movenet.

  # EpiContactTrace does movement_data <- unique(movement_data) , so need to ensure
  # all movements are unique.
  if(any(duplicated(movement_data))){
    warning(paste0("'movement_data' contains duplicated rows (movements), but ",
                   "these must be unique. Duplicate rows will be removed."))
  }

  ## root is supposed to be a character or integer identifier , NAs not allowed
  if (is.factor(root)) {
    assert_character(as.character(root), any.missing = FALSE, null.ok = FALSE)
  } else {
  assert(
    check_character(root, any.missing = FALSE, null.ok = FALSE),
    check_integerish(root, any.missing = FALSE, null.ok = FALSE)
  )}

  # Check if we are using the combination of tEnd and days or specifying inBegin,
  # inEnd, outBegin and outEnd
  custom_error_msg <- paste0("Use either 'tEnd' and 'days', or 'inBegin', ",
                             "'inEnd', 'outBegin' and 'outEnd'.")
  if (all(!is.null(tEnd), !is.null(days))) {  #if both tEnd and days are given...

    msg_collection = makeAssertCollection()
    # ...inBegin, inEnd, outBegin, outEnd should be null
    assert_null(inBegin, add = msg_collection)
    assert_null(inEnd, add = msg_collection)
    assert_null(outBegin, add = msg_collection)
    assert_null(outEnd, add = msg_collection)
    if(isFALSE(msg_collection$isEmpty())){
      msg_collection$push(custom_error_msg)
      reportAssertions(msg_collection)
    }

    if (any(is.character(tEnd), is.factor(tEnd))) {
      assert_date(as.Date(tEnd), any.missing = FALSE, null.ok = FALSE)
    } else {
      assert_date(tEnd, any.missing = FALSE, null.ok = FALSE) #actually this results in unhelpful error message, as date can be given as a character
    }

    if(any(duplicated(tEnd))){
      warning(paste0("'tEnd' contains duplicated values, but these must be unique. ",
                     "Duplicate values will be removed."))
    }

    assert_integerish(days, any.missing = FALSE, null.ok = FALSE, lower = 0)
    if(any(duplicated(days))){
      warning(paste0("'days' contains duplicated values, but these must be unique. ",
                     "Duplicate values will be removed."))
    }

    if(any(duplicated(root))){
      warning(paste0("'root' contains duplicated values, but these must be unique. ",
                     "Duplicate values will be removed."))
    }
  } else if (all(!is.null(inBegin), !is.null(inEnd),  #if inBegin etc. are given...
                 !is.null(outBegin), !is.null(outEnd))) {

    msg_collection = makeAssertCollection()
    # ...tEnd and days should be null
    assert_null(tEnd, add = msg_collection)
    assert_null(days, add = msg_collection)
    if(isFALSE(msg_collection$isEmpty())){
      msg_collection$push(custom_error_msg)
      reportAssertions(msg_collection)
    }

    if (any(is.character(inBegin), is.factor(inBegin))) {
      assert_date(as.Date(inBegin), any.missing = FALSE, null.ok = TRUE)
    } else {
      assert_date(inBegin, any.missing = FALSE, null.ok = TRUE)
    }

    if (any(is.character(inEnd), is.factor(inEnd))) {
      assert_date(as.Date(inEnd), any.missing = FALSE, null.ok = TRUE)
    } else {
      assert_date(inEnd, any.missing = FALSE, null.ok = TRUE)
    }

    if (any(is.character(outBegin), is.factor(outBegin))) {
      assert_date(as.Date(outBegin), any.missing = FALSE, null.ok = TRUE)
    } else {
      assert_date(outBegin, any.missing = FALSE, null.ok = TRUE)
    }

    if (any(is.character(outEnd), is.factor(outEnd))) {
      assert_date(as.Date(outEnd), any.missing = FALSE, null.ok = TRUE)
    } else {
      assert_date(outEnd, any.missing = FALSE, null.ok = TRUE)
    }

  } else {
    msg_collection = makeAssertCollection()
    msg_collection$push(custom_error_msg)
    reportAssertions(msg_collection)
  }

  assert_count(maxDistance, null.ok = TRUE)

# Trace contact chains with EpiContactTrace --------------------------

  # Process movement data for compatibility (select columns, change headers)
  movedata_for_ECT <- movedata2EpiContactTrace(movement_data)

  # Determine contact chains
  contact_tracing_results <-
    EpiContactTrace::Trace(movedata_for_ECT,
                           root, tEnd, days, inBegin, inEnd, outBegin, outEnd,
                           maxDistance)

  # Print message if there are no ingoing or outgoing contact chains.
  if(length(contact_tracing_results) > 1){
    n_contact_chains <-
      sapply(contact_tracing_results, function(x) {
        sum(length(x@ingoingContacts@source), length(x@outgoingContacts@source))})
  } else {
    n_contact_chains <-
      sum(length(contact_tracing_results@ingoingContacts@source),
          length(contact_tracing_results@outgoingContacts@source))
  }
  if(any(n_contact_chains == 0)){
    root_no_cc <- names(which(n_contact_chains == 0))
    message(paste0("No ingoing or outgoing contact chains for root(s) ",
                   paste0(root_no_cc, collapse = ", "),
                   " during the search period."))
  }

  root_cc <- names(which(n_contact_chains != 0))
  if(length(root_cc) == 0){
    message("No contact chains to plot.")
  } else {
    message(paste0("Creating map with contact chains for root(s) ", paste0(root_cc, collapse = ", "), "."))

    # Get attributes from original data tibbles and convert back to movenet format
    cc_movement_data <- ContactTrace2movedata(contact_tracing_results, movement_data)
    cc_holding_data <- ContactTrace2holdingdata(contact_tracing_results, holding_data)

# Plot contact chains on Leaflet map --------------------------------------
    contactchains2leaflet(cc_movement_data, cc_holding_data, admin_areas_map,
                          colour_domain_weight_in, colour_domain_weight_out,
                          colour_domain_moves_in, colour_domain_moves_out)
  }
}

#' Make movement data compatible with EpiContactTrace's `Trace()` function
#'
#' Internal helper function that processes movement data from a movenet-format
#' tibble to a format compatible with EpiContactTrace's `Trace()` function.
#' It selects the minimally required columns, renames them, and then converts
#' the tibble to a data.frame.
#'
#' @param movement_data A movenet-format movement data tibble.
#'
#' @returns A data.frame with columns "source" (= from), "destination" (= to),
#'   "t" (= date), and "n" (= weight).
#'
#' @importFrom dplyr select
#'
#' @seealso [EpiContactTrace::Trace()]
#'
#' @keywords internal
movedata2EpiContactTrace <- function(movement_data){
  colnames <- names(movement_data)
  movement_data %>%
    select(source = colnames[[1]],
           destination = colnames[[2]],
           t = colnames[[3]],
           n = colnames[[4]]) %>%
    as.data.frame()
}

#' Convert ContactTrace object into movenet-format movement data
#'
#' Internal helper function that processes contact tracing results from
#' EpiContactTrace's `Trace()` function back into movenet format, and adds in
#' any movement attributes from the original movenet-format movement data tibble.
#'
#' @details This requires for "repeated movements on one day" to be aggregated,
#' otherwise it's complicated to ensure a correct join.
#'
#' @param contact_trace_object A ContactTrace object returned by
#'  EpiContactTrace's `Trace()` function.
#' @param original_movement_data The movenet-format movement data tibble that was
#'  used as input to `Trace()`.
#'
#' @returns A movenet-format movement data tibble, with as rows only the
#'  movements that are part of the contact chains in `contact_trace_object`, and
#'  as columns all original attribute columns from `original_movement_data` and
#'  additionally "direction" ("in" for moves that are part of ingoing contact
#'  chains, or "out" for moves that are part of outgoing contact chains).
#'
#' @seealso [EpiContactTrace::Trace()]
#'
#' @importFrom dplyr select right_join
#'
#' @keywords internal
ContactTrace2movedata <- function(contact_trace_object, original_movement_data){

  colname_from <- names(original_movement_data)[1]
  colname_to <- names(original_movement_data)[2]
  colname_date <- names(original_movement_data)[3]
  colname_weight <- names(original_movement_data)[4]

  if(length(contact_trace_object) > 1){ #if there are multiple roots and contact_trace_object is a list
    contact_trace_object %>%
      lapply(function(x) {
        x %>%
          as("data.frame") %>%
          select(source, destination, t, n, direction) %>%
          dplyr::right_join(original_movement_data, .,  #requires this set of keys to be unique
                            by = setNames(c("source", "destination", "t", "n"),
                                          c(colname_from, colname_to, colname_date,
                                            colname_weight)),
                            relationship = "one-to-many") #one row in contact_trace_object can match to at most one row in original_movement_data
      }) %>% purrr::reduce(rbind) #this binds the tibbles together into one
    } else {
      contact_trace_object %>%
        as("data.frame") %>%
        select(source, destination, t, n, direction) %>%
        dplyr::right_join(original_movement_data, .,  #requires this set of keys to be unique
                          by = setNames(c("source", "destination", "t", "n"),
                                        c(colname_from, colname_to, colname_date,
                                          colname_weight)),
                          relationship = "one-to-many") #one row in contact_trace_object can match to at most one row in original_movement_data
    }
  }


#' Convert ContactTrace object into movenet-format holding data
#'
#' Internal helper function that processes contact tracing results from
#' EpiContactTrace's `Trace()` function back into movenet format, and adds in
#' any holding attributes from the original movenet-format holding data tibble.
#'
#' @param contact_trace_object A ContactTrace object returned by
#'  EpiContactTrace's `Trace()` function.
#' @param original_holding_data An (optional) movenet-format holding data tibble
#'  with additional data on the holdings in `contact_trace_object`.
#'
#' @returns A movenet-format holding data tibble, with as rows only the
#'  holdings that are part of the contact chains in `contact_trace_object`, and
#'  as columns all original attribute columns from `original_holding_data` and
#'  additionally "direction" ("root" for root holdings, "in" for holdings that
#'  are part of ingoing contact chains, or "out" for moves that are part of
#'  outgoing contact chains). If `original_holding_data` is `NULL`, a
#'  tibble with only the columns "holding_id" and "direction" is returned.
#'
#' @importFrom dplyr right_join
#'
#' @keywords internal
ContactTrace2holdingdata <- function(contact_trace_object,
                                     original_holding_data = NULL){

  colname_id <- names(original_holding_data)[1]

  # Create data.frame with holding data for root nodes, and nodes in ingoing and
  # outgoing contact chains.
  # Need to make sure this works for contact_trace_object being a list of
  # ContactTrace objects, in case of multiple roots, as well as a single vector.
  if(length(contact_trace_object) > 1){
    roots <- sapply(contact_trace_object, function(x) x@root)
    ins <- sapply(contact_trace_object, function(x) {
      c(x@ingoingContacts@source,
        x@ingoingContacts@destination[which(x@ingoingContacts@distance != 1)])}) %>%
        unlist(use.names = FALSE) %>% # concatenate results from multiple roots into one vector
        unique()
    outs <- sapply(contact_trace_object, function(x) {
      c(x@outgoingContacts@source[which(x@outgoingContacts@distance != 1)],
        x@outgoingContacts@destination)}) %>%
        unlist(use.names = FALSE) %>% # concatenate results from multiple roots into one vector
        unique()
  } else {
    roots <- contact_trace_object@root
    ins <- c(contact_trace_object@ingoingContacts@source,
             contact_trace_object@ingoingContacts@destination) %>% unique()
    outs <- c(contact_trace_object@outgoingContacts@source,
              contact_trace_object@outgoingContacts@destination) %>% unique()
  }
  roots_df <- tibble(holding_id=roots, direction="root")
  ins_df <- tibble(holding_id=ins, direction="in")
  outs_df <- tibble(holding_id=outs, direction="out")
  holdings_df <- rbind(roots_df, ins_df, outs_df)

  # Add original holding attributes
  holdings_df %>%
    unique() %>%
    { if(!is.null(original_holding_data)){
      dplyr::right_join(original_holding_data, .,
                        by = setNames("holding_id", colname_id),
                        relationship = "one-to-many")
    } else {.} }
}

#' Create Leaflet map with ingoing and outgoing contact chains
#'
#' Internal helper function that plots ingoing and outgoing contact chains onto
#' a Leaflet map with default OpenStreetMap background.
#'
#' @param movement_data A movenet-format movement data tibble, with movements
#'   that are part of ingoing and outgoing contact chains.
#' @param holding_data A movenet-format holding data tibble, with holdings that
#'   are part of ingoing and outgoing contact chains.
#' @param admin_areas_map An sf object containing multipolygons for relevant
#'   administrative areas.
#' @param colour_domain_weight_in
#' @param colour_domain_weight_out
#' @param colour_domain_moves_in
#' @param colour_domain_moves_out
#'
#' @returns A Leaflet map (HTML widget object) with graphics layers for holdings
#'   and moves that are part of ingoing and outgoing contact chains, and for the
#'   provided administrative area boundaries. Further layers can be added using
#'   `%>%`.
#'
#' @importFrom dplyr left_join arrange group_by summarize
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @import leaflet
#' @importFrom leaflegend addLegendLine
#' @importFrom sf st_cast st_as_sf st_join st_transform st_drop_geometry st_geometry<-
#'
#' @keywords internal
contactchains2leaflet <- function(movement_data, holding_data,
                                  admin_areas_map = NULL,
                                  colour_domain_weight_in = NULL,
                                  colour_domain_weight_out = NULL,
                                  colour_domain_moves_in = NULL,
                                  colour_domain_moves_out = NULL){

  colname_from <- names(movement_data)[1]
  colname_to <- names(movement_data)[2]
  colname_date <- names(movement_data)[3]
  colname_weight <- names(movement_data)[4]
  colname_id <- names(holding_data)[1]

# Transform coordinates to WGS84 for compatibility with Leaflet -----------

  holding_data <-
    holding_data %>%
    st_as_sf() %>%
    st_transform(4326)

  if(!is.null(admin_areas_map)){
    admin_areas_map <- st_transform(admin_areas_map, 4326)

# Join admin areas to holding data ----------------------------------------

    # This requires duplicating the admin_areas_map geometry column, because
    # st_join only keeps the active geometry from x (holding_data) and deletes
    # that of y (admin_areas_map)

    admin_areas_map %>%
      mutate(adm_area_coords = geometry) %>% #duplicate the geometry column
      st_join(x = holding_data, y = .) -> holding_data
  }

# Add linestring coordinates to movement data -----------------------------

  movement_data <-
    movement_data %>%

    # First add point coordinates and admin area coordinates for origin and destination holdings.
    left_join(y = {holding_data %>% select(all_of(colname_id), coordinates, any_of("adm_area_coords")) %>% unique},
              by = setNames(colname_id, colname_from),
              relationship = "many-to-one") %>%
    rename(coords_from = coordinates) %>%
    #if adm_area_coords exist, rename to adm_area_from
    { if("adm_area_coords" %in% names(.)){
      rename(., adm_area_from = adm_area_coords) } else {.} } %>%
    left_join(y = {holding_data %>% select(all_of(colname_id), coordinates, any_of("adm_area_coords")) %>% unique},
              by = setNames(colname_id, colname_to),
              relationship = "many-to-one") %>%
    rename(coords_to = coordinates) %>%
    { if("adm_area_coords" %in% names(.)){
      rename(., adm_area_to = adm_area_coords) } else {.} } %>%

    # Then transform point coordinates to linestring: to do this, need to convert
    # the tibble to long format, add a "line_id", and then summarise by "line_id".
    mutate(line_id = row_number()) %>%
    pivot_longer(cols=c("coords_from","coords_to"),
                 names_to = "coord_type",
                 values_to = "coordinates") %>%
    st_as_sf(sf_column_name = "coordinates") %>%
    { coords_long <- .
      coords_long_linestring <-
        group_by(.data = ., line_id) %>%
        summarize(do_union=FALSE) %>%
        st_cast("LINESTRING") %>%
        rename(geometry = coordinates)
      left_join(x=coords_long, y = as.data.frame(coords_long_linestring), by = c("line_id"))
    } %>%  #need to convert back to sf object
    `st_geometry<-`("geometry") %>%

    # Convert back to wide format and remove "line_id".
    tidyr::pivot_wider(names_from = "coord_type", values_from = "coordinates") %>%
    select(-line_id)

# Summarise movement data by admin area -----------------------------------

  # Total up (i) nr of moves and (ii) overall movement weight coming from or
  # going to each admin area.
  # First filter out within-admin-area moves (to avoid double counting weight).
  #   Is this the right thing to do?! - County where root farm is, is never coloured in (even if there's a within-county move).

  summarise_movedata_by_adm_area <- function(movement_data, admin_areas_map, direction_subset){
    coords <- switch(direction_subset, "in" = "coords_from", "out" = "coords_to")
    adm_area <- switch(direction_subset, "in" = "adm_area_from", "out" = "adm_area_to")
    colname_weight <- names(movement_data)[4]

    movement_data %>%
      filter(direction == direction_subset) %>%
      { if(nrow(.) == 0) { . # use this construction to prevent error when . is empty (0 rows)
      } else { filter(., adm_area_from != adm_area_to) }} %>% #filtering out within-admin-area moves
      group_by(.data[[adm_area]]) %>%
      summarise(total_weight := sum(.data[[colname_weight]]),
                n_moves = dplyr::n()) %>%
      left_join(x = admin_areas_map, y = as.data.frame(.),
                by = setNames(adm_area, "geometry"),
                relationship = "one-to-one") %>%
      replace_na(list(total_weight = 0, n_moves = 0))
  }

  if(!is.null(admin_areas_map)){
    adm_area_ingoing_data <-
      summarise_movedata_by_adm_area(movement_data, admin_areas_map, "in")
    adm_area_outgoing_data <-
      summarise_movedata_by_adm_area(movement_data, admin_areas_map, "out")
  }


# Add columns for parallel plotting of repeated moves ---------------------

  # To plot repeated moves in parallel rather than on top of each other, each
  # move on a connection is given a different offset

  # To facilitate calculating these offsets, we add an extra edge_id column:
  # For each connection, each move (edge) on this connection is given a
  # consecutive integer edge_id from 1. Thus, repeated moves are numbered 1, 2,
  # ..., n.

  movement_data <-
    movement_data %>%
    arrange(.data[[colname_from]], .data[[colname_to]],
            direction, .data[[colname_date]]) %>%
    mutate(edge_id_on_connection = row_number(),
           .by = all_of(c(colname_from, colname_to)))

  # Additionally, to have width of lines/arrows depend on weight, create an
  # edge_width column, and then calculate each offset based on cumulative sum of
  # variable edge_widths and a fixed betw_width between the lines.
  betw_width = 1
  movement_data <-
    movement_data %>%
    mutate(edge_width = 2^floor(log10(.data[[colname_weight]]))) %>%
    mutate(offset = -(cumsum(edge_width + betw_width) - (edge_width + betw_width)/2),
           .by = all_of(c(colname_from, colname_to)))

  # Create a dynamically set "breaks" vector for the edge_width legend in the map
  values <- sort(unique(movement_data$edge_width))
  starts <- 10^log2(values) #starts of the weight bins
  ends <- (10^(log2(values)+1))-1 #ends of the weight bins
  breaks <- setNames(values,
                     paste(starts, ends, sep = " - ")) #create "start - end" label for all bins

# Convert movement data to GeoJSON for JS Leaflet plugins -----------------

  movement_geojson_in <-
    movement_data[which(movement_data$direction == "in"),] %>%
    select(-coords_from, -coords_to, -any_of(c("adm_area_from", "adm_area_to"))) %>%
    geojsonsf::sf_geojson()

  movement_geojson_out <-
    movement_data[which(movement_data$direction == "out"),] %>%
    select(-coords_from, -coords_to, -any_of(c("adm_area_from", "adm_area_to"))) %>%
    geojsonsf::sf_geojson()

# Register JS Leaflet plugins ---------------------------------------------

  # This uses several Leaflet plugins that have not been integrated into
  # R Leaflet yet, so needs to include some Javascript code.

  # This tells htmlwidgets about plugin names, version, where to find scripts.
  offset_plugin <-
    htmltools::htmlDependency("leaflet.PolylineOffset", "1.1.1",
                              src = c(href="https://cdn.jsdelivr.net/gh/bbecquet/Leaflet.PolylineOffset@1.1.1"),
                              script = "leaflet.polylineoffset.js"
  )
  ctrlGrouped_plugin <-
    htmltools::htmlDependency("ctrlGrouped", "0.6.1",
                              src = c(href="https://cdn.jsdelivr.net/gh/ismyrnow/leaflet-groupedlayercontrol@0.6.1/dist/"),
                              script = "leaflet.groupedlayercontrol.min.js",
                              stylesheet = "leaflet.groupedlayercontrol.min.css"
  )
  geometryutil_plugin <-
    htmltools::htmlDependency("leaflet-geometryutil", "0.10.3",
                              src = c(href="https://cdn.jsdelivr.net/gh/makinacorpus/Leaflet.GeometryUtil@0.10.3/src/"),
                              script = "leaflet.geometryutil.js"
  )
  arrowheads_plugin <-
    htmltools::htmlDependency("leaflet-arrowheads", "1.4.0",
                              src = c(href="https://cdn.jsdelivr.net/gh/slutske22/leaflet-arrowheads@.1.4.0/src/"),
                              script = "leaflet-arrowheads.js"
  )
  fontawesome_plugin <-
    htmltools::htmlDependency("fontawesome", "6.5.2",
                              src = c(href="https://use.fontawesome.com/releases/v6.5.2/css/"),
                              stylesheet = "all.css"
    )
  sidebar_plugin <-
    htmltools::htmlDependency("leaflet-sidebar", "0.2.4",
                              src = c(href="https://cdn.jsdelivr.net/gh/Turbo87/leaflet-sidebar@0.2.4/src/"),
                              script = "L.Control.Sidebar.js",
                              stylesheet = "L.Control.Sidebar.css"
    )

  # A function that takes a plugin htmlDependency object and adds
  # it to the map. This ensures that however or whenever the map
  # gets rendered, the plugin will be loaded into the browser.
  register_plugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }

# Define colour palettes for leaflet map ----------------------------------

  # Define colour palettes for total weight and number of moves.
  # This uses optional arguments "colour_domain_*" to allow user to set fixed
  # colour domains e.g. for comparison across maps (allowing for the same amount
  # of "total weight" or "n moves" to have the same colour intensity across maps)

  if(!is.null(admin_areas_map)){

    if(is.null(colour_domain_weight_in)) {colour_domain_weight_in <- adm_area_ingoing_data$total_weight}
    if(is.null(colour_domain_weight_out)) {colour_domain_weight_out <- adm_area_outgoing_data$total_weight}
    if(is.null(colour_domain_moves_in)) {colour_domain_moves_in <- adm_area_ingoing_data$n_moves}
    if(is.null(colour_domain_moves_out)) {colour_domain_moves_out <- adm_area_outgoing_data$n_moves}

    pal_ingoing_weight <-
      colorNumeric("Blues", colour_domain_weight_in, na.color = "grey")
    pal_ingoing_moves <-
      colorNumeric("Blues", colour_domain_moves_in, na.color = "grey")
    pal_outgoing_weight <-
      colorNumeric("Reds", colour_domain_weight_out, na.color = "grey")
    pal_outgoing_moves <-
      colorNumeric("Reds", colour_domain_moves_out, na.color = "grey")

  }

  # Helper function to apply colour palette, with custom warning message, and
  # collection of warning messages from multiple calls into a list (warning_messages).
  # This list is later returned as a single error message.
  # The function is called from within the leaflet map construction.

    warning_messages <- list() #variable to store warning messages in

    apply_palette <- function(palette_name, values) {

      withCallingHandlers({
        colour_palette <- eval(as.name(palette_name))
        result <- colour_palette(values) # Apply the colour palette to the values
        return(result)
      }, warning = function(w) { # Turn the specific warning below into an error
        if (grepl("Some values were outside the color scale and will be treated as NA", w$message)) {
          domain_name <- switch(palette_name,
                                pal_ingoing_weight = "colour_domain_weight_in",
                                pal_outgoing_weight = "colour_domain_weight_out",
                                pal_ingoing_moves = "colour_domain_moves_in",
                                pal_outgoing_moves = "colour_domain_moves_out")
          weight_or_moves <- switch(palette_name,
                                    pal_ingoing_weight = "ingoing movement weights",
                                    pal_outgoing_weight = "outgoing movement weights",
                                    pal_ingoing_moves = "ingoing moves",
                                    pal_outgoing_moves = "outgoing moves")
          warning_message <- paste0("The colour scale provided by `",
                                    domain_name,
                                    "` does not include the full range of aggregated ",
                                    weight_or_moves,
                                    ".\n   Please ensure `",
                                    domain_name,
                                    "` covers at least the following range: ",
                                    min(values, na.rm = TRUE),
                                    " to ",
                                    max(values, na.rm = TRUE), ".")
          warning_messages <<- append(warning_messages, list(warning_message)) #store warning message
          w$message <- warning_message
          invokeRestart("muffleWarning")
        }
      })
  }

# Define pop-up texts for holdings ----------------------------------------

  # Function to generate popup content
  generate_popup <- function(data) {
    properties_to_exclude <- c(names(admin_areas_map), "adm_area_coords")
    properties_to_exclude <- properties_to_exclude[which(properties_to_exclude %in% names(data))]
    data <- st_drop_geometry(data) %>% select(-{{properties_to_exclude}})
    apply(data, 1, function(row) {
      paste0(
        "<b>", ifelse(row["direction"] == "root", "Root holding",
                      ifelse(row["direction"] == "in", "Originating holding", "Destination holding")), "</b><br>",
        paste0("<b>",names(data)[which(names(data) != "direction")], ":</b> ", row[which(names(data) != "direction")], collapse = "<br>")
      )
    })
  }

  # Filtered data subsets
  holding_data_root <- holding_data[holding_data$direction == "root",]
  holding_data_in <- holding_data[holding_data$direction == "in",]
  holding_data_out <- holding_data[holding_data$direction == "out",]

  # Generate popups for each subset
  popups_root <- generate_popup(holding_data_root)
  popups_in <- generate_popup(holding_data_in)
  popups_out <- generate_popup(holding_data_out)

# Construct Leaflet map ---------------------------------------------------
  leaflet_map <- leaflet() %>%

    # Use default OSM tiles
    addTiles() %>%

    # Register JS plugins for customised code
    register_plugin(offset_plugin) %>%
    register_plugin(ctrlGrouped_plugin) %>%
    register_plugin(geometryutil_plugin) %>%
    register_plugin(arrowheads_plugin) %>%
    register_plugin(fontawesome_plugin) %>%
    register_plugin(sidebar_plugin)

    # If provided, add admin_areas_map and derivatives as layers.
    # Code is wrapped in withCallingHandlers to catch all potential warning
    # messages from apply_palette() and return these as a single error message.
  if(!is.null(admin_areas_map)){
    leaflet_map <- withCallingHandlers({
      leaflet_map %>%
        addPolygons(data = admin_areas_map,
                    color = "black",
                    weight = 2,
                    fillColor = "transparent",
                    group = "Administrative areas") %>%
        addPolygons(data = adm_area_ingoing_data,
                    color = "black",
                    weight = 2,
                    fillColor = ~apply_palette("pal_ingoing_weight", total_weight),
                    fillOpacity = 0.5,
                    group = "Ingoing weight") %>%
        addPolygons(data = adm_area_ingoing_data,
                    color = "black",
                    weight = 2,
                    fillColor = ~apply_palette("pal_ingoing_moves", n_moves),
                    fillOpacity = 0.5,
                    group = "Ingoing moves") %>%
        addPolygons(data = adm_area_outgoing_data,
                    color = "black",
                    weight = 2,
                    fillColor = ~apply_palette("pal_outgoing_weight", total_weight),
                    fillOpacity = 0.5,
                    group = "Outgoing weight") %>%
        addPolygons(data = adm_area_outgoing_data,
                    color = "black",
                    weight = 2,
                    fillColor = ~apply_palette("pal_outgoing_moves", n_moves),
                    fillOpacity = 0.5,
                    group = "Outgoing moves") %>%

        # Add colour legends, but suppress color scale related warnings (these are
        # redundant with custom error messages thrown by apply_palette)
        {withCallingHandlers({
          addLegend(map = .,
                    data = adm_area_ingoing_data,
                    title = "Ingoing moves from area",
                    position = "bottomleft",
                    pal = pal_ingoing_moves,
                    values = ~n_moves,
                    layerId = "Ingoing_moves_legend",
                    className = "info legend ingoing-moves") %>%
            addLegend(data = adm_area_outgoing_data,
                      title = "Outgoing moves to area",
                      position = "bottomleft",
                      pal = pal_outgoing_moves,
                      values = ~n_moves,
                      layerId = "Outgoing_moves_legend",
                      className = "info legend outgoing-moves") %>%
            addLegend(data = adm_area_ingoing_data,
                      title = "Ingoing weight from area",
                      position = "bottomleft",
                      pal = pal_ingoing_weight,
                      values = ~total_weight,
                      layerId = "Ingoing_weight_legend",
                      className = "info legend ingoing-weight") %>%
            addLegend(data = adm_area_outgoing_data,
                      title = "Outgoing weight to area",
                      position = "bottomleft",
                      pal = pal_outgoing_weight,
                      values = ~total_weight,
                      layerId = "Outgoing_weight_legend",
                      className = "info legend outgoing-weight")
        }, warning = function(w) {
          if (grepl("Some values were outside the color scale and will be treated as NA", w$message)) {
            invokeRestart("muffleWarning")
          }
        })
        }},
        warning = function(w){
          if (grepl("The color scale provided", w$message)) {
            warning_messages <<- append(warning_messages, list(w$message))
            invokeRestart("muffleWarning")
          }
        })

    if(length(warning_messages) > 0){
      stop(paste0("The following error(s) occurred when defining colour scales for administrative areas:\n - ", paste0(warning_messages, collapse = "\n - ")), call. = FALSE)
    }
  }

    # Add holdings as "circle markers", with colour depending on their inclusion
    # in ingoing or outgoing chains. Alpha is set to 0.5 to allow over-plotting
    # (resulting in purple colouring) for holdings that are part of both chains.

  leaflet_map <- leaflet_map %>%
    addCircleMarkers(data = holding_data_root,
                     color = "black",
                     opacity = 0.5,
                     fillOpacity = 0.2,
                     group = "Root holding(s)",
                     clusterOptions = markerClusterOptions(),
                     popup = popups_root,
                     options = pathOptions(pane = "holdingsPane")) %>%
    addCircleMarkers(data = holding_data_in,
                     color = "blue",
                     opacity = 0.5,
                     fillOpacity = 0.2,
                     group = "Originating holdings",
                     clusterOptions = markerClusterOptions(),
                     popup = popups_in,
                     options = pathOptions(pane = "holdingsPane")) %>%
    addCircleMarkers(data = holding_data_out,
                     color = "red",
                     opacity = 0.5,
                     fillOpacity = 0.2,
                     group = "Destination holdings",
                     clusterOptions = markerClusterOptions(),
                     popup = popups_out,
                     options = pathOptions(pane = "holdingsPane")) %>%

    # Use JS plugins to add moves and grouped layer controls
    htmlwidgets::onRender("function(el, x, data) {
    var edge;
    var movement_geojson_in = data.movement_geojson_in
    var movement_geojson_out = data.movement_geojson_out
    var map = this;

    /*
    // Add interactive sidebar -- not using this at the moment

    var sidebar = L.control.sidebar('sidebar', {
        closeButton: true,
        position: 'left'
    });
    map.addControl(sidebar);
    */

    // Create a pane with a zIndex of 450 to make sure the holding markers are
    // drawn on top of the admin area backgrounds
    map.createPane('holdingsPane');
    map.getPane('holdingsPane').style.zIndex = 450;

    // Retrieve layer groups defined in R, allowing use in grouped layer control panel

    var groups = {
      originating_holdings: map.layerManager.getLayerGroup('Originating holdings'),
      destination_holdings: map.layerManager.getLayerGroup('Destination holdings'),
      ingoing_edges: new L.layerGroup(),
      outgoing_edges: new L.layerGroup(),
    };

    /*
    Create layers for edges (separately for ingoing and outgoing chains): using
    straight lines with arrowheads, and varying offsets to create parallel lines
    when moves are repeated.
    NB: to refer to a col header in GeoJSON, need to use '.properties.col_header'
    */

    // First make list of properties I don't want to report on in the map
    var excludedProperties = ['direction', 'edge_id_on_connection', 'edge_width', 'offset']

    // Create a pane with a zIndex of 450 to make sure the movement arrows are
    // drawn on top of the admin area backgrounds
    map.createPane('movementArrows');
    map.getPane('movementArrows').style.zIndex = 450;

    // Add ingoing and outgoing movements
    if (movement_geojson_in.features.length > 0) {
      movement_geojson_in.features.forEach(function(edge){
        var edge_coords = L.GeoJSON.coordsToLatLngs(edge.geometry.coordinates, 0);
        var polyline = L.polyline(edge_coords, {
                color: 'blue',
                weight: edge.properties.edge_width,
                opacity: 1,
                offset: edge.properties.offset,
                pane: 'movementArrows'
              }).arrowheads({
                offset: 0, yawn: 30, fill: true, size: '25px'
              }).addTo(groups.ingoing_edges)

        // Add popup with text based on all edge properties
        var text = '';
        for (var prop in edge.properties) {
          if (edge.properties.hasOwnProperty(prop) && !excludedProperties.includes(prop)) {
            text += '<b>'+ prop + ': </b>' + edge.properties[prop] + '<br>';
          }
        }
        polyline.bindPopup(text);
      });
    };
    groups.ingoing_edges.addTo(map);
    if (movement_geojson_out.features.length > 0) {
      movement_geojson_out.features.forEach(function(edge){
        var edge_coords = L.GeoJSON.coordsToLatLngs(edge.geometry.coordinates, 0);
        var polyline = L.polyline(edge_coords, {
                color: 'red',
                weight: edge.properties.edge_width,
                opacity: 1,
                offset: edge.properties.offset,
                pane: 'movementArrows'
              }).arrowheads({
                offset: 0, yawn: 30, fill: true, size: '25px'
              }).addTo(groups.outgoing_edges)

        // Add popup with text based on all edge properties
        var text = '';
        for (var prop in edge.properties) {
          if (edge.properties.hasOwnProperty(prop) && !excludedProperties.includes(prop)) {
            text += '<b>'+ prop + ': </b>' + edge.properties[prop] + '<br>';
          }
        }
        polyline.bindPopup(text);
      });
      groups.outgoing_edges.addTo(map);
    };


    // Set up grouped layer control

    var groupedOverlays = {
      'Incoming contact chains': {
          '<span class=\"fa-stack\" style=\"max-width: 12px; max-height: 12px; vertical-align: top;\"><i class=\"fa-solid fa-circle fa-stack-1x\" style=\"color: blue; opacity: 0.2\"></i><i class=\"fa-regular fa-circle fa-stack-1x\" style=\"color: blue; opacity: 0.5\"></i></span> Originating holdings': groups.originating_holdings ?? [],
          '<i class=\"fa-solid fa-right-long\" style=\"color: blue\"></i> Incoming movements': groups.ingoing_edges ?? []},
      'Outgoing contact chains': {
          '<span class=\"fa-stack\" style=\"max-width: 12px; max-height: 12px; vertical-align: top;\"><i class=\"fa-solid fa-circle fa-stack-1x\" style=\"color: red; opacity: 0.2\"></i><i class=\"fa-regular fa-circle fa-stack-1x\" style=\"color: red; opacity: 0.5\"></i></span> Destination holdings': groups.destination_holdings ?? [],
          '<i class=\"fa-solid fa-right-long\" style=\"color: red\"></i> Outgoing movements': groups.outgoing_edges ?? []}
      };

    // Make the Administrative area groups exclusive (use radio inputs)
    var options = {
      exclusiveGroups: ['Administrative areas'],
      collapsed: false};

    var layerControl = L.control.groupedLayers(null, groupedOverlays, options);
    layerControl.addTo(map);

    // If admin areas are provided, add these to layer control

    var adminAreas = map.layerManager.getLayerGroup('Administrative areas')
    if (adminAreas) {
      layerControl.addOverlay(adminAreas,
        '<span class=\"fa-stack fa-xs\" style=\"max-width: 16px; max-height: 12px; vertical-align: top; text-align: center;\"><i class=\"fa-regular fa-square fa-stack-2x\" style=\"color: black\"></i></span> Area boundaries only', 'Administrative areas');
      var ingoingMoves = map.layerManager.getLayerGroup('Ingoing moves');
      layerControl.addOverlay(ingoingMoves,
        '<span class=\"fa-stack fa-xs\" style=\"max-width: 16px; max-height: 12px; vertical-align: top; text-align: center;\"><i class=\"fa-solid fa-square fa-stack-2x\" style=\"color: blue; opacity: 0.2\"></i><i class=\"fa-solid fa-arrow-right-arrow-left fa-sm fa-stack-1x\" style=\"color: blue;\"></i></span> Total ingoing moves from area', 'Administrative areas');
      var ingoingWeight = map.layerManager.getLayerGroup('Ingoing weight');
      layerControl.addOverlay(ingoingWeight,
        '<span class=\"fa-stack fa-xs\" style=\"max-width: 16px; max-height: 12px; vertical-align: top; text-align: center;\"><i class=\"fa-solid fa-square fa-stack-2x\" style=\"color: blue; opacity: 0.2\"></i><i class=\"fa-solid fa-weight-hanging fa-sm fa-stack-1x\" style=\"color: blue;\"></i></span> Total ingoing weight from area', 'Administrative areas');
      var outgoingMoves = map.layerManager.getLayerGroup('Outgoing moves');
      layerControl.addOverlay(outgoingMoves,
        '<span class=\"fa-stack fa-xs\" style=\"max-width: 16px; max-height: 12px; vertical-align: top; text-align: center;\"><i class=\"fa-solid fa-square fa-stack-2x\" style=\"color: red; opacity: 0.2\"></i><i class=\"fa-solid fa-arrow-right-arrow-left fa-sm fa-stack-1x\" style=\"color: red;\"></i></span> Total outgoing moves to area', 'Administrative areas');
      var outgoingWeight = map.layerManager.getLayerGroup('Outgoing weight');
      layerControl.addOverlay(outgoingWeight,
        '<span class=\"fa-stack fa-xs\" style=\"max-width: 16px; max-height: 12px; vertical-align: top; text-align: center;\"><i class=\"fa-solid fa-square fa-stack-2x\" style=\"color: red; opacity: 0.2\"></i><i class=\"fa-solid fa-weight-hanging fa-sm fa-stack-1x\" style=\"color: red;\"></i></span> Total outgoing weight to area', 'Administrative areas');

    // Hide legends on first render, then toggle depending on which admin area map is active

      var legendClasses = ['outgoing-moves', 'ingoing-moves', 'outgoing-weight', 'ingoing-weight'];
      legendClasses.forEach(function(className) {
        var legend = document.getElementsByClassName('info legend ' + className)[0];
        if (legend) {
          legend.style.display = 'none';
        }
      })

      map.on('overlayadd',function(e){
       if (e.layer === adminAreas){
          var legendClasses = ['info legend outgoing-moves', 'info legend ingoing-moves', 'info legend outgoing-weight', 'info legend ingoing-weight'];
          legendClasses.forEach(function(className) {
            var legend = document.getElementsByClassName(className)[0];
            if (legend) {
              legend.style.display='none';
            }
          });
        } else if (e.layer === ingoingMoves){
          var legendClasses = ['info legend outgoing-moves', 'info legend outgoing-weight', 'info legend ingoing-weight'];
          legendClasses.forEach(function(className) {
            var legend = document.getElementsByClassName(className)[0];
            if (legend) {
              legend.style.display='none';
            }
          });
          var correctLegend = document.getElementsByClassName('info legend ingoing-moves')[0]
          correctLegend.style.display='block';
        } else if (e.layer === ingoingWeight){
          // var inWeightLeg = map.layerManager.getLayer('Ingoing_weight_legend');
          // inWeightLeg.addTo(map);
          var legendClasses = ['info legend outgoing-moves', 'info legend ingoing-moves', 'info legend outgoing-weight'];
          legendClasses.forEach(function(className) {
            var legend = document.getElementsByClassName(className)[0];
            if (legend) {
              legend.style.display='none';
            }
          });
          var correctLegend = document.getElementsByClassName('info legend ingoing-weight')[0]
          correctLegend.style.display='block';
        } else if (e.layer === outgoingMoves){
          // var outMovesLeg = map.layerManager.getLayer('Outgoing_moves_legend');
          // outMovesLeg.addTo(map);
          var legendClasses = ['info legend ingoing-moves', 'info legend outgoing-weight', 'info legend ingoing-weight'];
          legendClasses.forEach(function(className) {
            var legend = document.getElementsByClassName(className)[0];
            if (legend) {
              legend.style.display='none';
            }
          });
          var correctLegend = document.getElementsByClassName('info legend outgoing-moves')[0]
          correctLegend.style.display='block';
        } else if (e.layer === outgoingWeight){
          // var outWeightLeg = map.layerManager.getLayer('Outgoing_weight_legend');
          // outWeightLeg.addTo(map);
          var legendClasses = ['info legend outgoing-moves', 'info legend ingoing-moves', 'info legend ingoing-weight'];
          legendClasses.forEach(function(className) {
            var legend = document.getElementsByClassName(className)[0];
            if (legend) {
              legend.style.display='none';
            }
          });
          var correctLegend = document.getElementsByClassName('info legend outgoing-weight')[0]
          correctLegend.style.display='block';
        }

      });
    }
    }

", data = list(movement_geojson_in = movement_geojson_in,
                    movement_geojson_out = movement_geojson_out)) %>%
    hideGroup(c("Ingoing weight", "Ingoing moves", "Outgoing weight", "Outgoing moves")) %>%

    # Add a legend for movement weights
    addLegendLine(values = ~edge_width, title = "Movement weight",
                  color = "red", orientation = "vertical",
                  breaks = breaks,
                  baseSize = mean(movement_data$edge_width),
                  data = movement_data,
                  position = "bottomright")

  # # Create custom html div for sidebar and attach to leaflet_map
  # sidebar_div <- htmltools::tags$div(id="sidebar")
  # combined_html <- htmlwidgets::prependContent(
  #   leaflet_map,
  #   sidebar_div
  # )
  # combined_html

  leaflet_map

}
