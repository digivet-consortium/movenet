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
library(tidyverse) #magrittr (%>%), dplyr (select, rename)
reformatted_data <- reformat_move_data("C:/Users/cboga/OneDrive - University of Glasgow/CS3-ASF/Pig movement data structure/sample_pigs_UK_with_dep_arr_dates.csv")


# reformat data into edgelist
edgelist<-reformatted_data %>%
  select(origin_ID, dest_ID, nr_pigs, dep_date, move_ID) %>% #need first 2 columns to be the nodes, others are edge attributes
  rename(weight = nr_pigs) # turns nr_pigs into weights

mygraph <- graph_from_data_frame(edgelist,directed=TRUE)

#' ## Network size and density
#' Network size (number of nodes)
vcount(mygraph)
#' Number of edges
ecount(mygraph)
#' Density (n of edges / total possible n of edges)
#' Total possible edges = network size * (network size - 1) for directed graph
ecount(mygraph)/(vcount(mygraph)*(vcount(mygraph)-1)) #formula
edge_density(mygraph, loops = FALSE) #direct function

#' If we have more networks (or snapshots of the same network over time), we can plot distributions of these with hist
#' (may need unlisting)

#' ## Transitivity, structural balance, and hierarchy
#' Reciprocity (tendency with which network ties sent out by egos are returned by alters)
reciprocity(mygraph)
#' One can create a random graph with same size & density, to compare reciprocity to:
random_graph <- erdos.renyi.game(n = vcount(mygraph), p.or.m = edge_density(mygraph, loops = FALSE), directed = TRUE)
reciprocity(random_graph)


degree_distribution(mygraph)
plot(degree_distribution(mygraph))

