load_all()
library(network)
library(networkDynamic)
library(sna)
library(tsna)
library(lubridate) #as_date
library(dplyr) #between
library(tibble) #tibble

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"

data <- (movement_datafile |> reformat_data("movement"))
nw <- movedata2networkDynamic(data)

n_nodes <- network.size(nw) #network size = number of nodes overall
n_diff_connections <- network.edgecount(nw) #network order = number of edges overall #NOT THE SAME AS MOVES

edge_spell_list <- get.edge.activity(nw, as.spellList = TRUE)
n_moves <- nrow(edge_spell_list) #number of moves [or nrow(data)]
first_day <- min(edge_spell_list[[1]]) #(int format) or min(data[[3]]) (date format)
last_day <- max(edge_spell_list[[1]])  #(int format) or max(data[[3]]) (date format)

##When adding holding info:
# how many different holding types?
# n holdings per type (over the whole period & per year)
# descr stats holding sizes (median, mean, IQR, range, missing - over time)
# proportions of active holdings by holding size (over time)
# descr stats holding sizes by holding type (median, mean, IQR, range, missing - over time)


#create "snapshot" data dataframes
data_daily <- tibble(Time_int = first_day:last_day,
                     Time = as_date(Time_int))
data_7days <- tibble(Time_int = seq(first_day, last_day, by = 7),
                     Time = as_date(Time_int))
data_14days <- tibble(Time_int = seq(first_day, last_day, by = 14),
                      Time = as_date(Time_int))
data_28days <- tibble(Time_int = seq(first_day, last_day, by = 28),
                      Time = as_date(Time_int))
data_84days <- tibble(Time_int = seq(first_day, last_day, by = 84),
                      Time = as_date(Time_int))
data_monthly <- tibble(Time = seq(floor_date(as_date(first_day), "month"),
                                  as_date(last_day), by="month"),
                       Time_int = as.integer(Time))
data_quarterly <- tibble(Time = seq(floor_date(as_date(first_day), "quarter"),
                                    as_date(last_day), by="quarter"),
                         Time_int = as.integer(Time))

#Number of farms active each day, week, month etc.
data_daily$active_farms <-
  sapply(data_daily[["Time_int"]], function(t){network.size.active(nw, at = t)})
data_7days$active_farms <-
  sapply(data_7days[["Time_int"]],
         function(t){network.size.active(nw, onset = t, length = 7)})
data_14days$active_farms <-
  sapply(data_14days[["Time_int"]],
         function(t){network.size.active(nw, onset = t, length = 14)})
data_28days$active_farms <-
  sapply(data_28days[["Time_int"]],
         function(t){network.size.active(nw, onset = t, length = 28)})
data_84days$active_farms <-
  sapply(data_84days[["Time_int"]],
         function(t){network.size.active(nw, onset = t, length = 84)})
data_monthly$active_farms <-
  sapply(data_monthly[["Time_int"]],
         function(t){network.size.active(nw, onset = t, terminus = as.integer(as_date(t)+months(1)))})
data_quarterly$active_farms <-
  sapply(data_quarterly[["Time_int"]],
         function(t){network.size.active(nw, onset = t, terminus = as.integer(as_date(t)+months(3)))})

#Proportion of farms active
data_daily$prop_active_farms <- data_daily$active_farms/n_nodes
data_7days$prop_active_farms <- data_7days$active_farms/n_nodes
data_14days$prop_active_farms <- data_14days$active_farms/n_nodes
data_28days$prop_active_farms <- data_28days$active_farms/n_nodes
data_84days$prop_active_farms <- data_84days$active_farms/n_nodes
data_monthly$prop_active_farms <- data_monthly$active_farms/n_nodes
data_quarterly$prop_active_farms <- data_quarterly$active_farms/n_nodes

#summary stats
summary(data_daily$active_farms)
summary(data_7days$active_farms)
summary(data_14days$active_farms)
summary(data_28days$active_farms)
summary(data_84days$active_farms)
summary(data_monthly$active_farms)
summary(data_quarterly$active_farms)

#plot number of active farms
colours <- c("84 days" = "grey", "3 months" = "grey",
             "28 days" = "lightblue", "1 month" = "lightblue",
             "14 days" = "red", "2 weeks" = "red",
             "7 days" = "blue", "1 week" = "blue",
             "1 day" = "darkgreen")
g <- ggplot(data_daily) +
  aes(x = Time, y = active_farms) +
  labs(title="Number of farms active during each time period") +
  ylab("Farms active") +
  scale_y_continuous(limits = c(0, n_nodes)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  geom_line(aes(col = "1 day")) +
  geom_point(data = data_7days, aes(col = "7 days"), shape = 1) +
  geom_point(data = data_14days, aes(col = "14 days"), shape = 1) +
  geom_point(data = data_28days, aes(col = "28 days"), shape = 1) +
  geom_point(data = data_84days, aes(col = "84 days"), shape = 1) +
  scale_color_manual(values = colours,
                     limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
plot(g)

#plot proportion of active farms
g4 <- ggplot(data_daily) +
  aes(x = Time, y = 100*prop_active_farms) +
  labs(title="Proportion of farms active during each time period") +
  ylab("Proportion of farms (%)") +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  geom_line(aes(col = "1 day")) +
  geom_point(data = data_7days, aes(col = "7 days"), shape = 1) +
  geom_point(data = data_14days, aes(col = "14 days"), shape = 1) +
  geom_point(data = data_28days, aes(col = "28 days"), shape = 1) +
  geom_point(data = data_84days, aes(col = "84 days"), shape = 1) +
  scale_color_manual(values = colours,
                     limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
plot(g4)


#Number of movements each day
data_daily$n_movements <-
  sapply(data_daily[["Time_int"]],
         function(t){nrow(edge_spell_list[which(edge_spell_list$onset == t),])})
data_7days$n_movements <-
  sapply(data_7days[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+6)),])})
data_14days$n_movements <-
  sapply(data_14days[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+13)),])})
data_28days$n_movements <-
  sapply(data_28days[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+27)),])})
data_84days$n_movements <-
  sapply(data_84days[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, t+83)),])})
data_monthly$n_movements <-
  sapply(data_monthly[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, as.integer(as_date(t)+months(1))-1)),])})
data_quarterly$n_movements <-
  sapply(data_quarterly[["Time_int"]],
         function(t){nrow(edge_spell_list[which(between(edge_spell_list$onset, t, as.integer(as_date(t)+months(3))-1)),])})

#summary stats
summary(data_daily$n_movements)
summary(data_7days$n_movements)
summary(data_14days$n_movements)
summary(data_28days$n_movements)
summary(data_84days$n_movements)
summary(data_monthly$n_movements)
summary(data_quarterly$n_movements)

#plot number of movements
g2 <- ggplot(data_daily) +
  aes(x = Time, y = n_movements) +
  labs(title="Number of movements during each time period") +
  ylab("Number of movements") +
  scale_y_continuous(limits = c(0,NA)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  geom_line(aes(col = "1 day")) +
  geom_point(data = data_7days, aes(col = "7 days"), shape = 1) +
  geom_point(data = data_14days, aes(col = "14 days"), shape = 1) +
  geom_point(data = data_28days, aes(col = "28 days"), shape = 1) +
  geom_point(data = data_84days, aes(col = "84 days"), shape = 1) +
  scale_color_manual(values = colours,
                     limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
plot(g2)


#Number of edges each day
data_daily$n_edges <-
  sapply(data_daily[["Time_int"]], function(t){network.edgecount.active(nw, at = t)})
data_7days$n_edges <-
  sapply(data_7days[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, length = 7)})
data_14days$n_edges <-
  sapply(data_14days[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, length = 14)})
data_28days$n_edges <-
  sapply(data_28days[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, length = 28)})
data_84days$n_edges <-
  sapply(data_84days[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, length = 84)})
data_monthly$n_edges <-
  sapply(data_monthly[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, terminus = as.integer(as_date(t)+months(1)))})
data_quarterly$n_edges <-
  sapply(data_quarterly[["Time_int"]], function(t){network.edgecount.active(nw, onset = t, terminus = as.integer(as_date(t)+months(3)))})

#Density
data_daily$density <- data_daily$n_edges/(n_nodes*(n_nodes-1))
data_7days$density <- data_7days$n_edges/(n_nodes*(n_nodes-1))
data_14days$density <- data_14days$n_edges/(n_nodes*(n_nodes-1))
data_28days$density <- data_28days$n_edges/(n_nodes*(n_nodes-1))
data_84days$density <- data_84days$n_edges/(n_nodes*(n_nodes-1))
data_monthly$density <- data_monthly$n_edges/(n_nodes*(n_nodes-1))
data_quarterly$density <- data_quarterly$n_edges/(n_nodes*(n_nodes-1))

#Edges active each time period, as percentage of total possible edges
g3 <- ggplot(data_daily) +
  aes(x = Time, y = 100*density) +
  labs(title="Density: Proportion of all possible farm connections (edges) active each time period") +
  ylab("Density (%)") +
  scale_y_continuous(limits = c(0,NA)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  geom_line(aes(col = "1 day")) +
  geom_point(data = data_7days, aes(col = "7 days"), shape = 1) +
  geom_point(data = data_14days, aes(col = "14 days"), shape = 1) +
  geom_point(data = data_28days, aes(col = "28 days"), shape = 1) +
  geom_point(data = data_84days, aes(col = "84 days"), shape = 1) +
  scale_color_manual(values = colours,
                     limits = c("84 days", "28 days", "14 days", "7 days", "1 day"))
plot(g3)

#N pigs moved - Batch size  #Easier to calculate from data tibbles!
#monthly summary stats
monthly_batchsize_summary <-
  lapply(data_monthly[["Time_int"]], FUN = function(t) {
  sapply(get.edge.attribute.active(nw, "qty_pigs", onset = t, #list of edges during period
                                   terminus = as.integer(as_date(t)+months(1)),
                                   return.tea = TRUE, require.active = TRUE),
         function(x) x[[1]]) |> #extract edge weights
    unlist() |>
    na.omit() |>
    summary() |>
    unclass() }) |>
  bind_rows() |>
  `colnames<-`(c("batchsize_min", "batchsize_Q1", "batchsize_median",
                 "batchsize_mean", "batchsize_Q3", "batchsize_max"))
data_monthly <- cbind(data_monthly, monthly_batchsize_summary)

#plot of monthly summary stats
g5 <- ggplot(data_monthly) +
  aes(x = Time) +
  labs(title="Development of batch sizes over time") +
  ylab("Batch size") +
  scale_y_continuous(limits = c(0,NA)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  geom_ribbon(aes(ymin = batchsize_Q1, ymax = batchsize_Q3), colour = "grey", alpha=0.5) +
  geom_line(aes(y = batchsize_median), linetype = 1) +
  geom_line(aes(y = batchsize_mean), linetype = 2)
plot(g5)


#Create monthly snapshots  - these only keep 1st of repeated movements within a month
monthly_snapshots <-
  lapply(data_monthly[["Time_int"]], function(t){
    network.collapse(nw, onset = t,
                     terminus = as.integer(as_date(t)+months(1)),
                     active.default = FALSE,
                     retain.all.vertices = TRUE)})
monthly_adj_matrices <- lapply(monthly_snapshots, as.matrix)
data_monthly$move_lists <- lapply(seq_along(data_monthly[["Time_int"]]), function(n){
  as.data.frame(which(monthly_adj_matrices[[n]] == 1, arr.ind = TRUE, useNames = FALSE))})

#Network loyalty
month_combinations <-
  expand.grid(m1 = 1:nrow(data_monthly), m2 = 1:nrow(data_monthly))
month_combinations$loyalty <-
  sapply(1:nrow(month_combinations), function(x){
    sum(do.call(paste, data_monthly$move_lists[[month_combinations[x,1]]]) %in%
          do.call(paste, data_monthly$move_lists[[month_combinations[x,2]]])) /
    data_monthly$n_edges[[month_combinations[x,1]]]
  })
#month_combinations <- tidyr::complete(month_combinations, m1, m2)

ggplot(data = month_combinations,
       aes(m1, m2, fill = loyalty)) +
  labs(title="Network loyalty (fraction of common directed links)") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_gradient2(lim=c(0,1), low = "yellow", mid = "red", high = "black", midpoint = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  scale_x_continuous(lim=c(0.5,NA), breaks = c(1:nrow(data_monthly)),
                     labels = format(data_monthly$Time, "%b %y")) +
  scale_y_continuous(lim=c(0.5,NA), breaks = c(1:nrow(data_monthly)),
                     labels = format(data_monthly$Time, "%b %y")) +
  geom_tile()

#giant strongly connected component (GSCC) = largest subset of holdings where
#there exists a path between all pairs of holdings in that subset
data_monthly$GSCC_size_prop <-
  sapply(component.dist(monthly_snapshots, connected = "strong"),
       function(x) max(x$csize))/data_monthly$active_farms
#giant in-component (GIC) = all holdings with out-going contacts to the GSCC that
#they are not part of
#giant out-component (GIC) = all holdings with in-going contacts from the GSCC that
#they are not part of

#plot GSCC size barchart
ggplot(data = data_monthly,
       aes(x = Time, y = GSCC_size_prop)) +
  labs(title="Size of giant strongly connected component (GSCC)") +
  ylab("Component size (proportion of active holdings)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar(stat="identity")
