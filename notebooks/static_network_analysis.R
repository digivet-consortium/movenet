#' ---
#' title: igraph static analysis
#' author: Carlijn Bogaardt
#' output: html_document
#' ---
#'
#'
# Thu 12 May 2022

#+ setup, message=FALSE
load_all()
library(igraph)
reformatted_data <- reformat_move_data("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv")


mygraph <- graph_from_edgelist(as.matrix(reformatted_data[c("origin_ID","dest_ID")]),directed=TRUE)

degree_distribution(mygraph)
plot(degree_distribution(mygraph))

