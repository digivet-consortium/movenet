library("igraph")

load_all()
load_config("Denmark") # Check options in inst/configurations/Denmark.yml
datafile<-"C:/Users/carlijn/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv"
datafile<-"C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv"
data <- reformat_move_data(datafile)
# concatenate data for a few years, if possible please?
data <- anonymise(data, "holding")

#static graphs
g_unweighted <- igraph::graph_from_data_frame(data, directed=TRUE)

for(i in list(FALSE,1,2,3)){
  for(j in list(FALSE,1,2,3)){
    assign(paste0("g_j",i,"r",j),igraph::graph_from_data_frame(coarsen_weight(data,jitter=10^i,round=j),directed=TRUE))

  }
}

#what are sensible values of jitter for the Danish data?
#what are sensible values (powers-of-10) to round to for the Danish data?

#average shortest path distance mean_distance(g, directed = TRUE)
#diameter (longest shortest path distance) diameter(g, directed = TRUE)
#assortativity coefficients
#weighted degree distribution
#edge weight distribution
#centrality measures
