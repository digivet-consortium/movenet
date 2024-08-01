#' Create a temporal network analysis report from a movement network
#'
#' @param network networkDynamic object containing a movement network.
#' @param output_file Output file name and path.
#' @param incl_reachability_analysis A logical indicating whether to include
#'   reachability analysis (slow).
#' @param n_threads An integer indicating the number of parallel threads to use
#'   for reachability analysis.
#' @param whole_months A logical indicating whether `movement_data` covers full
#'   months (default `TRUE`). This affects calculation of average network
#'   measures.
#' @param time_unit Character string indicating the time unit of analyses. One
#'   of `"month"`, `"quarter"`, or `"year"`.
#'
#' @importFrom rmarkdown render
#' @importFrom flextable qflextable set_flextable_defaults
#'
#' @return An html report is created with network analyses.
#'
#' @export
create_temporal_network_analysis_report <- function(network, output_file,
                                                    incl_reachability_analysis,
                                                    n_threads,
                                                    whole_months = TRUE,
                                                    time_unit = "month"){

  net_env <- new.env()

  intro_text <- "This page follows analyses and recreates some plots from [Schulz J et al. (2017) PLOS ONE 12(6):e0179915](https://doi.org/10.1371/journal.pone.0179915)."

  dates <- extract_dates(network, whole_months)

  basic_network_summary(network, dates, net_env)
  basic_network_summary_per_time_unit(network, dates, time_unit, net_env)

  static_network_snapshots_analysis(network, dates, time_unit, net_env)
  if(isTRUE(incl_reachability_analysis)){
    reachability_analysis(network, dates, time_unit, net_env, n_threads)
  }

  #####################
  ### Create report ###
  #####################

  report_source <- system.file("reports/temporal_network_analysis.Rmd",
                               package = "movenet")
  #Have this vary according to an arg report_type?
  if(length(report_source)!=1L || report_source=="") stop("Internal error:  temporal_network_analysis.Rmd file not found")

  old_defaults <- set_flextable_defaults(font.size = 10)

  #Knit report
  fn <- render(input = report_source,
         output_format = "html_document",
         output_file = output_file,
         envir = net_env,
         params = list(incl_reachability_analysis = incl_reachability_analysis))

  do.call(set_flextable_defaults, old_defaults)

  invisible(fn)

}

#' Extract dates for study periods & subperiods (7d, 14d, 28d, 84d, month, quarter, year)
#'
#' @param network A movement network
#' @param whole_months A logical indicating whether `movement_data` covers
#'   full months (default `TRUE`). This affects calculation of average network
#'   measures.
#'
#' @importFrom lubridate as_date ceiling_date floor_date
#' @importFrom networkDynamic get.edge.activity
#'
#' @keywords internal
extract_dates <- function(network, whole_months = TRUE){

  dates_data <- list()
  edge_spell_list <- get.edge.activity(network, as.spellList = TRUE)

  first_day <- ifelse(isTRUE(whole_months),
                      floor_date(as_date(min(edge_spell_list[[1]])), "month"),
                      min(edge_spell_list[[1]]))
  last_day <- ifelse(isTRUE(whole_months),
                     ceiling_date(as_date(max(edge_spell_list[[1]])), "month")-1,
                     max(edge_spell_list[[1]]))

  #Extract dates for every day
  dates_data$daily_int <- c(first_day:last_day)
  dates_data$daily <- as_date(dates_data$daily_int)

  #Extract dates for every 7, 14, 28, 84 days
  mapply(function(n_days){
   dates_data[[paste0(n_days,"days_int")]] <<- seq(first_day, last_day, by = n_days)
   dates_data[[paste0(n_days,"days")]] <<- as_date(dates_data[[paste0(n_days,"days_int")]])},
   c(7, 14, 28, 84))

 #Extract dates for every month, quarter, year
  mapply(function(period){
    dates_data[[paste0(period,"ly")]] <<-
      seq(floor_date(as_date(first_day), period), as_date(last_day), by = period)
    dates_data[[paste0(period,"ly_int")]] <<-
      as.integer(dates_data[[paste0(period,"ly")]])},
    c("month", "quarter", "year"))

  return(dates_data)
}

#' Create basic network summary plots for temporal network report and app
#'
#' @inheritParams create_temporal_network_analysis_report
#' @param dates A named list with dates for study subperiods
#' @param net_env The internal environment within which plots are stored for the
#'  report
#'
#' @importFrom dplyr between
#' @import network
#' @import networkDynamic
#'
#' @keywords internal
basic_network_summary <- function(network, dates, net_env){

  edge_spell_list <- get.edge.activity(network, as.spellList = TRUE)

  #####################
  ### Network sizes ###
  #####################

  #Extract network size data for study periods and subperiods
  net_env[["n_nodes"]] <- n_nodes <- network.size(network)

  size_data <- list()

  size_data$daily <-
    sapply(dates$daily_int,
           function(t){network.size.active(network, at = t)})

  mapply(function(n_days){size_data[[paste0(n_days,"days")]] <<-
    sapply(dates[[paste0(n_days,"days_int")]], function(t){
      network.size.active(network, onset = t, length = n_days)})},
    c(7, 14, 28, 84))

  #Turn network sizes into proportions
  mapply(
    function(x){size_data[[paste0(x,"_prop")]] <<- size_data[[x]]/n_nodes},
    c("daily","7days","14days","28days","84days"))

  #Plot network sizes
  net_env[["size_plot_number"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, size_data$daily),
        data.frame(dates$"7days", size_data$"7days"),
        data.frame(dates$"14days", size_data$"14days"),
        data.frame(dates$"28days", size_data$"28days"),
        data.frame(dates$"84days", size_data$"84days"),
        "Holdings active during each time period",
        "Number of holdings")
  net_env[["size_plot_prop"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, size_data$daily_prop),
        data.frame(dates$"7days", size_data$"7days_prop"),
        data.frame(dates$"14days", size_data$"14days_prop"),
        data.frame(dates$"28days", size_data$"28days_prop"),
        data.frame(dates$"84days", size_data$"84days_prop"),
        "Holdings active during each time period",
        "Proportion of holdings")


  #Extract number of movements for study periods and subperiods
  net_env[["n_moves"]] <- n_moves <- nrow(edge_spell_list)
  moves_data <- list()

  moves_data$daily <-
    sapply(dates$daily_int, function(t){
      nrow(edge_spell_list[which(edge_spell_list$onset == t),])})

  mapply(function(n_days){moves_data[[paste0(n_days,"days")]] <<-
    sapply(dates[[paste0(n_days,"days_int")]], function(t){
      nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+(n_days-1))),])})},
    c(7, 14, 28, 84))

  #Turn movement numbers into proportions
  mapply(
    function(x){moves_data[[paste0(x,"_prop")]] <<- moves_data[[x]]/n_moves},
    c("daily","7days","14days","28days","84days"))


  #Extract numbers of edges for study periods & subperiods
  net_env[["edge_count"]] <- edge_count <- network.edgecount(network)
  edges_data <- list()

  edges_data$daily <- sapply(dates$daily_int, function(t){
    network.edgecount.active(network, at = t)})

  mapply(function(n_days){edges_data[[paste0(n_days,"days")]] <<-
    sapply(dates[[paste0(n_days,"days_int")]], function(t){
      network.edgecount.active(network, onset = t, length = n_days)})},
    c(7, 14, 28, 84))

  #Turn edge numbers into proportions [of actual edge count]
  mapply(
    function(x){edges_data[[paste0(x,"_prop")]] <<- edges_data[[x]]/edge_count},
    c("daily","7days","14days","28days","84days"))

  #Turn edge numbers into densities (proportion of all *possible* edges)
  possible_edges <- n_nodes*(n_nodes-1)
  net_env[["edge_density"]] <- edge_density <- edge_count/possible_edges
  mapply(
    function(x){edges_data[[paste0(x,"_density")]] <<- edges_data[[x]]/possible_edges},
    c("daily","7days","14days","28days","84days"))

  #Plot movements / edges
  net_env[["moves_plot_number"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, moves_data$daily),
        data.frame(dates$"7days", moves_data$"7days"),
        data.frame(dates$"14days", moves_data$"14days"),
        data.frame(dates$"28days", moves_data$"28days"),
        data.frame(dates$"84days", moves_data$"84days"),
        "Movements during each time period",
        "Number of movements")
  net_env[["moves_plot_prop"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, moves_data$daily_prop),
        data.frame(dates$"7days", moves_data$"7days_prop"),
        data.frame(dates$"14days", moves_data$"14days_prop"),
        data.frame(dates$"28days", moves_data$"28days_prop"),
        data.frame(dates$"84days", moves_data$"84days_prop"),
        "Movements during each time period",
        "Proportion of movements")
  net_env[["edges_plot_number"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, edges_data$daily),
        data.frame(dates$"7days", edges_data$"7days"),
        data.frame(dates$"14days", edges_data$"14days"),
        data.frame(dates$"28days", edges_data$"28days"),
        data.frame(dates$"84days", edges_data$"84days"),
        "Edges active during each time period",
        "Number of edges")
  net_env[["edges_plot_prop"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, edges_data$daily_prop),
        data.frame(dates$"7days", edges_data$"7days_prop"),
        data.frame(dates$"14days", edges_data$"14days_prop"),
        data.frame(dates$"28days", edges_data$"28days_prop"),
        data.frame(dates$"84days", edges_data$"84days_prop"),
        "Edges active during each time period",
        "Proportion of edges")
  net_env[["edges_plot_density"]] <-
      snapshots_diffperiods_dotplot1(
        data.frame(dates$daily, edges_data$daily_density),
        data.frame(dates$"7days", edges_data$"7days_density"),
        data.frame(dates$"14days", edges_data$"14days_density"),
        data.frame(dates$"28days", edges_data$"28days_density"),
        data.frame(dates$"84days", edges_data$"84days_density"),
        "Edge density during each time period",
        "Proportion of all possible edges")
}

#' Create basic network summary tables and batch size plot for temporal network report and app
#'
#' @inheritParams create_temporal_network_analysis_report
#' @inheritParams basic_network_summary
#'
#' @importFrom dplyr between
#' @importFrom tibble tibble num
#' @importFrom lubridate as_date quarter year
#' @importFrom stats na.omit
#' @import network
#' @import networkDynamic
#'
#' @keywords internal
basic_network_summary_per_time_unit <- function(network, dates, time_unit,
                                                net_env){

  #Need movement config - do config load check!
  net_env[["time_unit"]] <- time_unit
  n_nodes <- network.size(network)
  n_months <- switch(time_unit, "month" = 1, "quarter" = 3, "year" = 12)
  dates_int <- dates[[paste0(time_unit,"ly_int")]]
  dates <- dates[[paste0(time_unit,"ly")]]
  date_format <- switch(time_unit, "month" = "%b %Y",
                        "quarter" = "%Y-%m-%d", "year" = "%Y")
  edge_spell_list <- get.edge.activity(network, as.spellList = TRUE)

  #Monthly/quarterly/yearly network sizes and proportions
  size_data <-
    sapply(dates_int, function(t){
      network.size.active(network, onset = t,
                          terminus = as.integer(as_date(t)+months(n_months)))})
  size_data_prop <- size_data/n_nodes

  size_summary_table <-
    tibble(!!time_unit := format(dates, date_format),
           "holdings active (n)" = size_data,
           "holdings active (prop.)" = num(size_data_prop,
                                           digits = 3))
  if(time_unit == "quarter"){
    size_summary_table["quarter"] <-
      paste0("Q", quarter(size_summary_table[["quarter"]]), " ",
             year(size_summary_table[["quarter"]]))
  }
  net_env[["size_summary_table"]] <- size_summary_table

  #Monthly/quarterly/yearly numbers of moves, edges, edge density
  n_moves <- nrow(edge_spell_list)
  edge_count <- network.edgecount(network)
  possible_edges <- n_nodes*(n_nodes-1)

  moves_data <- sapply(dates_int, function(t){
    nrow(edge_spell_list[
      which(between(edge_spell_list$onset, t, as.integer(as_date(t)+months(n_months))-1)),])})

  moves_data_prop <- moves_data/n_moves

  edges_data <- sapply(dates_int, function(t){
    network.edgecount.active(network, onset = t,
                             terminus = as.integer(as_date(t)+months(n_months)))})

  edges_data_prop <- edges_data/edge_count #proportion of actual edge count
  edges_data_density <- edges_data/possible_edges #proportion of possible edges

  net_env[["moves_summary_table"]] <-
    tibble(!!time_unit := size_summary_table[[time_unit]],
           "movements (n)" = moves_data,
           "movements (prop.)" = num(moves_data_prop, digits = 3),
           "edges (n)" = edges_data,
           "edges (prop.)" = num(edges_data_prop, digits = 3),
           "edge density (prop.)" = num(edges_data_density, digits = 3))

  #Extract summary stats for movement weights for each study subperiod
  weight_attribute_name <- get.network.attribute(network, "weight")
  weights_summary_stats <-
    lapply(dates_int, FUN = function(t) {
      sapply(get.edge.attribute.active(network,
                                       weight_attribute_name,
                                       onset = t, #list of edges during period
                                       terminus = as.integer(as_date(t)+months(n_months)),
                                       return.tea = TRUE, require.active = TRUE),
             function(x) x[[1]]) %>% #extract edge weights
        unlist() %>%
        na.omit()}) %>%
    periodic_data2summary_stats_df(dates, time_unit)

  net_env[["weights_summary"]] <-
    weights_summary_stats %>%
    mutate(!!time_unit := size_summary_table[[time_unit]])

  #Plot monthly summary stats for movement weights
  net_env[["weights_plot"]] <-
    snapshot_summary_stats_linechart(weights_summary_stats,
                                     "Development of movement weights over time",
                                     "Movement weight (batch size)")

}

#' Create component and loyalty analyses content for temporal network report and app
#'
#' @inheritParams create_temporal_network_analysis_report
#' @inheritParams basic_network_summary
#'
#' @importFrom network network.size
#' @importFrom networkDynamic network.collapse
#' @importFrom sna component.dist reachability
#'
#' @keywords internal
static_network_snapshots_analysis <- function(network, dates, time_unit,
                                              net_env){

  dates_int <- dates[[paste0(time_unit,"ly_int")]]
  dates <- dates[[paste0(time_unit,"ly")]]
  n_months <- switch(time_unit, "month" = 1, "quarter" = 3, "year" = 12)

  snapshots <- lapply(dates_int, function(t){
    suppressWarnings(
      network.collapse(network, onset = t,
                       terminus = as.integer(as_date(t)+months(n_months)),
                       rule = "any", active.default = FALSE,
                       retain.all.vertices = FALSE))})
  #this raises warnings: edges with multiple edges spells with different
  #attribute values, e.g. repeated moves with different batch sizes, are
  #assigned the earliest value in the static snapshot.
  #Upshot: DON'T USE SNAPSHOTS FOR WEIGHTED ANALYSES.
  lists_of_moves <- lapply(snapshots, function(s){as.data.frame(s)[,c(1,2)]})
  size_data <- sapply(snapshots, function(s) network.size(s))
  edge_count <- sapply(snapshots, function(s) network.edgecount(s))

  # Component analysis

  strongly_connected_component_data <- component.dist(snapshots,
                                                      connected = "strong")
  components <- list()
  component_size_data <- list()

  #Identifying GSCC, GIC & GOC
  components$GSCC <-
    sapply(strongly_connected_component_data, function(x){
      which(x$membership == which(x$csize == max(x$csize))[[1]]) #N.B. this selects the members of only the first out of many equally large components
      #which(x$membership == which(x$csize == max(x$csize))) #N.B this selects all largest components, and bungs all members together
    })

  reachabilities <- reachability(snapshots, return.as.edgelist = TRUE)
  components$GIC <-
    lapply(seq_along(reachabilities), function(n){
      unique(reachabilities[[n]][which((reachabilities[[n]][,2] %in% components$GSCC[[n]])
                                       & !(reachabilities[[n]][,1] %in% components$GSCC[[n]]))])
    })
  components$GOC <-
    lapply(seq_along(reachabilities), function(n){
      unique(reachabilities[[n]][which((reachabilities[[n]][,1] %in% components$GSCC[[n]])
                                       & !(reachabilities[[n]][,2] %in% components$GSCC[[n]]))])
    })

  #Calculating component sizes (proportion of active farms included in GSCC)
  component_size_data$GSCC_prop <-
    sapply(strongly_connected_component_data, function(x) max(x$csize))/size_data
  component_size_data$GIC_prop <-
    sapply(components$GIC, function(x) length(x))/size_data
  component_size_data$GOC_prop <-
    sapply(components$GOC, function(x) length(x))/size_data


  #Plotting component sizes
  net_env[["GSCC_size_plot"]] <-
    snapshot_barchart(data.frame(dates, component_size_data$GSCC_prop),
                      "Size of giant strongly connected component (GSCC)",
                      "Component size (proportion of active holdings)")
  net_env[["GIC_size_plot"]] <-
    snapshot_barchart(data.frame(dates, component_size_data$GIC_prop),
                      "Size of giant in-component (GIC)",
                      "Component size (proportion of active holdings)")
  net_env[["GOC_size_plot"]] <-
    snapshot_barchart(data.frame(dates, component_size_data$GOC_prop),
                      "Size of giant out-component (GOC)",
                      "Component size (proportion of active holdings)")

  # Loyalty

  #Creating df with fractions of common directed links between all monthly networks
  combinations <-
    expand.grid(m1 = 1:length(dates), m2 = 1:length(dates))
  combinations$loyalty <-
    sapply(1:nrow(combinations), function(x){
      sum(do.call(paste, lists_of_moves[[combinations[x,1]]]) %in%
            do.call(paste, lists_of_moves[[combinations[x,2]]])) /
        edge_count[[combinations[x,1]]]
    })
  combinations <- mutate(combinations,
                         m1 = dates[.data$m1],
                         m2 = dates[.data$m2])

  #Plotting heatmap with fractions of common directed links
  net_env[["common_links_heatmap"]] <-
    monthxmonth_heatmap(combinations,
                        "Fraction of common directed links")

}


#' Create reachability analysis content for temporal network report
#'
#' @inheritParams create_temporal_network_analysis_report
#' @inheritParams basic_network_summary
#'
#' @importFrom tidyr unnest
#' @import tibble tibble
#'
#' @keywords internal
reachability_analysis <- function(network, dates, time_unit, net_env, n_threads){

  cl <- makeCluster(n_threads)
  on.exit(stopCluster(cl))

  clusterEvalQ(cl, {
    library("tsna")
    library("networkDynamic")
  })

  dates_int <- dates[[paste0(time_unit,"ly_int")]]
  dates <- dates[[paste0(time_unit,"ly")]]
  n_months <- switch(time_unit, "month" = 1, "quarter" = 3, "year" = 12)

  message("Calculating reachabilities (slow)")

  #For each month in the dataset,
  reachabilities <-
    Map(c, dates_int, as.integer(dates+months(n_months))) %>%
    pblapply(function(period) {
      # ...extract the monthly subnetwork
      periodic_nw <-
        networkDynamic::network.extract(network, onset = period[[1]],
                                        terminus = period[[2]], rule = "any",
                                        retain.all.vertices = FALSE,
                                        trim.spells = TRUE)
        # ...calculate forward and backward reachabilities
      fwd_reachability <- tsna::tReach(periodic_nw, "fwd", graph.step.time = 1)
      bkwd_reachability <- tsna::tReach(periodic_nw, "bkwd", graph.step.time = 1)
      list(fwd_reachability = fwd_reachability,
           bkwd_reachability = bkwd_reachability)
    }, cl = cl )

  #transpose: from monthly lists of fwd & bkwd reach, to list of monthly fwd
  #reach & list of monthly bkwd reach
  values <- purrr::transpose(reachabilities)
  fwd_reachabilities <- values$fwd_reachability
  bkwd_reachabilities <- values$bkwd_reachability

  #plot reachability box plots
  net_env[["fwd_reachability_plot"]] <-
    snapshot_boxplot(
      unnest(tibble(dates, fwd_reachabilities), fwd_reachabilities),
      "Forward reachable set (out-going contact chain) sizes",
      "Size")
  net_env[["bkwd_reachability_plot"]] <-
    snapshot_boxplot(
      unnest(tibble(dates, bkwd_reachabilities), bkwd_reachabilities),
      "Backward reachable set (in-going contact chain) sizes",
      "Size")
}



#' Create snapshot dotplots for temporal network report and app
#'
#' @keywords internal
snapshots_diffperiods_dotplot1 <- function(data_daily, data_7days, data_14days,
                                           data_28days, data_84days, title, ylab){

  colours <- c("84 days" = "grey", "3 months" = "grey",
               "28 days" = "lightblue", "1 month" = "lightblue",
               "14 days" = "red", "2 weeks" = "red",
               "7 days" = "blue", "1 week" = "blue",
               "1 day" = "darkgreen")

  names(data_daily) <- c("x","y")
  names(data_7days) <- c("x","y")
  names(data_14days) <- c("x","y")
  names(data_28days) <- c("x","y")
  names(data_84days) <- c("x","y")

  ggplot(data_daily, aes(.data$x, .data$y)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_line(aes(col = "1 day")) +
    geom_point(data = data_7days, aes(col = "7 days")) +
    geom_point(data = data_14days, aes(col = "14 days")) +
    geom_point(data = data_28days, aes(col = "28 days")) +
    geom_point(data = data_84days, aes(col = "84 days")) +
    scale_color_manual(values = colours,
                       limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
}

#' Create time-period based summary tables for temporal network analysis and app
#'
#' @importFrom dplyr bind_rows
#'
#' @keywords internal
periodic_data2summary_stats_df <-
  function(periodic_data, period_dates, period = c("month", "quarter", "year")){
    df <-
      lapply(periodic_data, function(x){summary(x) %>% round(digits = 3) %>% unclass()}) %>%
      bind_rows() %>%
      `colnames<-`(c("min", "Q1", "median", "mean", "Q3", "max"))
    df <- cbind(period_dates, df)
    names(df)[1] <- period
    return(df)
  }

#' Create snapshot summary linechart (e.g. batchsize plot) for temporal network analysis and app
#'
#' @keywords internal
snapshot_summary_stats_linechart <- function(periodic_data, title, ylab){

  names(periodic_data) <- c("x", "min", "Q1", "median", "mean", "Q3", "max")

  ggplot(periodic_data) +
    aes(x=.data$x) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    scale_y_continuous(limits = c(0,NA)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_ribbon(aes(ymin = .data$Q1, ymax = .data$Q3), colour = "grey", alpha=0.5) +
    geom_line(aes(y = .data$median), linetype = 1) +
    geom_line(aes(y = .data$mean), linetype = 2)
}

#' Create snapshot summary barchart (e.g. component analyses) for temporal network analysis and app
#'
#' @keywords internal
snapshot_barchart <- function(periodic_data, title, ylab){

  names(periodic_data) <- c("x","y")

  ggplot(data = periodic_data,
         aes(.data$x, .data$y)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_bar(stat="identity")
}

#' Create snapshot heatmap (e.g. loyalty analysis) for temporal network analysis and app
#'
#' @keywords internal
monthxmonth_heatmap <- function(month_combinations, title){

  names(month_combinations)[3] <- "z"

  ggplot(data = month_combinations,
         aes(.data$m1, .data$m2, fill = .data$z)) +
    labs(title = title) +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_gradient2(lim=c(0,1), low = "yellow", mid = "red", high = "black",
                         midpoint = 0.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "right",
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_tile()
}

#' Create snapshot boxplots (e.g. reachability analyses) for temporal network analysis and app
#'
#' @keywords internal
snapshot_boxplot <- function(periodic_data, title, ylab) {

  names(periodic_data) <- c("x","y")

  ggplot(data = periodic_data,
         aes(.data$x, .data$y, group = .data$x)) +
    labs(title = title) +
    xlab("Time") +
    ylab(ylab) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    geom_boxplot()
}
