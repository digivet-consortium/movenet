library("igraph")
library("lubridate")

library("pbapply")
library("tidyverse")

#Synthetic data, on Carlijn's work computer
#datafile<-"C:/Users/carlijn/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv"
#Synthetic data, on Carlijn's laptop
#datafile<-"C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv"

# Danish data, available to Matt only:
datafile <- "/Users/matthewdenwood/Documents/Research/Projects/DigiVet/CS2/DK_pig_movements/svine_flytninger_2018_2020.csv"


#want to compare different coarsen_dates
# load_all()
library("movenet")
load_config("Denmark_processed") # Please Check options in inst/configurations/Denmark.yml!

# NB from Matt's Danish data summaries I gather that the date format string is different for each year
# i.e. that option needs changing between reformats for different years!
## Yes, that is super annoying but unfortunately true :(
## Another (possibly better) option is for me to reformat and harmonise the different years of data before reading with movenet

data <- reformat_move_data(datafile)

#Not including international movements for now
#What to do with missing ANTAL_FLYT_DYR? Remove?
## I have removed them from part of this data (they are movement of dead pigs / containers)

# concatenate data for a few years, if possible please
# data <- bind_rows(data.1, data.2)
## There are 3 years of data in the single file
data <- data # |> filter(DATO_FLYTNING >= "2019-01-01")

#anonymise
data <- anonymise(data, "holding")

#static graph
g_static <- igraph::graph_from_data_frame(data, directed=TRUE)

#daily snapshots
movenetenv <- movenet:::movenetenv
daily_data <- tibble::tibble(dates = as.numeric(seq(from=min(data[[movenetenv$options$movedata_cols$move_date]]),to=max(data[[movenetenv$options$movedata_cols$move_date]]),by=1)),
                             active_nodes = pbsapply(as.numeric(seq(from=min(data[[movenetenv$options$movedata_cols$move_date]]),to=max(data[[movenetenv$options$movedata_cols$move_date]]),by=1)),
                                                     function(x){vcount(subgraph.edges(g_static, E(g_static)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = TRUE))}),
                             edge_densities = pbsapply(as.numeric(seq(from=min(data[[movenetenv$options$movedata_cols$move_date]]),to=max(data[[movenetenv$options$movedata_cols$move_date]]),by=1)),
                                                     function(x){edge_density(subgraph.edges(g_static, E(g_static)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE))}),
                             median_degree = pbsapply(as.numeric(seq(from=min(data[[movenetenv$options$movedata_cols$move_date]]),to=max(data[[movenetenv$options$movedata_cols$move_date]]),by=1)),
                                                    function(x){median(degree(subgraph.edges(g_static, E(g_static)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE),mode="all"))}),
                             GSCC_size = pbsapply(as.numeric(seq(from=min(data[[movenetenv$options$movedata_cols$move_date]]),to=max(data[[movenetenv$options$movedata_cols$move_date]]),by=1)),
                                                function(x){
                                                  subgraph <- subgraph.edges(g_static, E(g_static)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE)
                                                  max(components(subgraph, mode = "strong")$csize)/vcount(subgraph)
                                                }))

#aggregrated data
tosave <- character(0)
for (level in c("week","month","bimonth","quarter","halfyear","year")){
  # create graph for aggregate data
  g <- coarsen_date(data, level, aggregate_data = TRUE) %>%
    igraph::graph_from_data_frame(directed=TRUE)
  # relevant dates in data
  n <- interval(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year"),(floor_date(max(data[[movenetenv$options$movedata_cols$move_date]]),"year")))/years()
  dates <-
    if(level == "week"){
      wks <- as.numeric(seq(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"week"),floor_date(max(data[[movenetenv$options$movedata_cols$move_date]]),"week"),7))
      wks[which(wks >= floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year"))]
    } else if(level == "month"){
      as.numeric(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year") + months(0:(n*12+11)))
    } else if(level == "bimonth"){
      as.numeric(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year") + months(seq(0,(n*12+11),2)))
    } else if(level == "quarter"){
      as.numeric(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year") + months(seq(0,(n*12+11),3)))
    } else if(level == "halfyear"){
      as.numeric(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year") + months(seq(0,(n*12+11),6)))
    } else { #if (level == "year")
      as.numeric(floor_date(min(data[[movenetenv$options$movedata_cols$move_date]]),"year") + years(0:n))
    }
  # edge_densities etc. using appropriate graph and dates
  active_nodes <-sapply(dates,
                        function(x){vcount(subgraph.edges(g, E(g)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = TRUE))})
  edge_densities <- sapply(dates,
                           function(x){edge_density(subgraph.edges(g, E(g)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE))})
  median_degree <- sapply(dates,
                          function(x){median(degree(subgraph.edges(g, E(g)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE),mode="all"))})
  GSCC_size <- sapply(dates,
                      function(x){
                        subgraph <- subgraph.edges(g, E(g)[eval(parse(text=movenetenv$options$movedata_cols$move_date))==x], delete.vertices = FALSE)
                        max(components(subgraph, mode = "strong")$csize)/vcount(subgraph)
                      })

  assign(paste0(level,"ly_data"),tibble::tibble(dates = dates,
                                                active_nodes = active_nodes,
                                                edge_densities = edge_densities,
                                                median_degree = median_degree,
                                                GSCC_size = GSCC_size))

  tosave <- c(tosave, paste0(level,"ly_data"))
}

save(list=tosave, file="summaries_DK_3yr.rda")

pdf("movenet_plots_DK_3yr.pdf")

#Fraction of nodes active plot
plot(as_date(daily_data$dates), daily_data$active_nodes/vcount(g_static),
     ylim=c(min(daily_data$active_nodes/vcount(g)),max(yearly_data$active_nodes/vcount(g_static))),
     xlab="Time",
     ylab="Fraction of nodes active",
     col="blue")
points(as_date(weekly_data$dates), weekly_data$active_nodes/vcount(g_static),
       col = "green")
points(as_date(monthly_data$dates), monthly_data$active_nodes/vcount(g_static),
       col = "red")
points(as_date(quarterly_data$dates), quarterly_data$active_nodes/vcount(g_static),
       col = "tan")
points(as_date(yearly_data$dates), yearly_data$active_nodes/vcount(g_static),
       col = "black")

#Edge density plot
plot(as_date(daily_data$dates), daily_data$edge_densities,
     ylim=c(min(daily_data$edge_densities[which(daily_data$edge_densities>0)]),max(yearly_data$edge_densities)),
     log='y',
     xlab="Time",
     ylab="Edge density",
     col="blue")
points(as_date(weekly_data$dates), weekly_data$edge_densities,
       col = "green")
points(as_date(monthly_data$dates), monthly_data$edge_densities,
       col = "red")
points(as_date(quarterly_data$dates), quarterly_data$edge_densities,
       col = "tan")
points(as_date(yearly_data$dates), yearly_data$edge_densities,
       col = "black")

edge_density(g_static) #edge density for static network. Can be compared with daily data -> what percent of edges is active each day
mean(daily_data$edge_densities)/edge_density(g_static) #proportion of edges active each day on average (need to x 100 for percentage)

#Median degree plot
plot(as_date(daily_data$dates), daily_data$median_degree,
     ylim=c(min(daily_data$median_degree),max(yearly_data$median_degree)),
     xlab="Time",
     ylab="Median degree",
     col="blue")
points(as_date(weekly_data$dates), weekly_data$median_degree,
       col = "green")
points(as_date(monthly_data$dates), monthly_data$median_degree,
       col = "red")
points(as_date(quarterly_data$dates), quarterly_data$median_degree,
       col = "tan")
points(as_date(yearly_data$dates), yearly_data$median_degree,
       col = "black")

#Relative GSCC size plot
plot(as_date(daily_data$dates), daily_data$GSCC_size,
     ylim=c(min(daily_data$GSCC_size),max(yearly_data$GSCC_size)),
     log='y',
     xlab="Time",
     ylab="size of GSCC",
     col="blue")
points(as_date(weekly_data$dates), weekly_data$GSCC_size,
       col = "green")
points(as_date(monthly_data$dates), monthly_data$GSCC_size,
       col = "red")
points(as_date(quarterly_data$dates), quarterly_data$GSCC_size,
       col = "tan")
points(as_date(yearly_data$dates), yearly_data$GSCC_size,
       col = "black")

dev.off()
