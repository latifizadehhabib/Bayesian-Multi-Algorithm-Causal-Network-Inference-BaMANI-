plot_weighted_adjacency <- function(arcs, weight) {
  
  # Create a data frame of arcs and weights
  arcs_df <- data.frame(from = as.character(arcs[, 1]), 
                        to = as.character(arcs[, 2]), 
                        weight = weight)
  
  # Create a graph from the arcs and weights
  g <- graph_from_data_frame(arcs_df, directed = TRUE)
  
  # Get the weighted adjacency matrix of the graph
  adj_matrix <- as.matrix(get.adjacency(g, attr = "weight", sparse = FALSE))
  
  # Add a small positive constant to the weights to avoid taking logarithm of zero or negative values
  adj_matrix[adj_matrix <= 0] <- min(adj_matrix[adj_matrix > 0])/1000
  
  
  # Take the logarithm of the weights
  log_adj_matrix <- log(adj_matrix)
  
  # Plot the heatmap of the logarithmic weighted adjacency matrix
  plot <- ggplot(melt(log_adj_matrix), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "", y = "", title = "Weighted Adjacency Matrix")
  
  return(plot)
  # return(list(plot = plot, adj_matrix = adj_matrix, log_adj_matrix = log_adj_matrix))
  
}
