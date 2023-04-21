#Results in NaNs - I think tbc doesnt work with directed graphs

library(TNC)

n_threads <- 4
n_nodes <- network.size(true_network)

adj_matrices <-
  true_network |>
  get.networks(
    start = NULL, end = NULL,
    time.increment = 1,
    retain.all.vertices = TRUE) |>
  lapply(as.matrix) |>
  lapply(unname)


adj_lists <-
  lapply(seq_along(adj_matrices),
         function(i){
         sapply(1:n_nodes,
                function(j){which(adj_matrices[[i]][j,]==1)})})


result <- tbc(adj_lists, "L", directed = TRUE, centrality_evolution = TRUE)


cl <- makeCluster(n_threads)
clusterExport(cl, c("adj_lists", "tbc"))

pblapply(1:n_nodes,
         function(x){tbc(adj_lists, "L", directed = TRUE, vertexindices = x)},
         cl = cl)
stopCluster(cl)

Reduce("+", TBC)
