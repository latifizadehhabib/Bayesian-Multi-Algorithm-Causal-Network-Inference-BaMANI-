# Define the function to create the Blank_edge_list
create_edge_list <- function(Blank_edge_list, Edge_count, ars) {
  # Loop through each row in the arcs dataframe
  for (i in seq_len(nrow(ars))) {
    # Check if the arc is already in the Blank_edge_list
    if (!any(Blank_edge_list$from == ars[i, 1] & Blank_edge_list$to == ars[i, 2])) {
      # If not, add the arc and its reverse to the Blank_edge_list
      Blank_edge_list[nrow(Blank_edge_list) + 1,] <- c(ars[i,1], ars[i,2], Edge_count)
      Blank_edge_list[nrow(Blank_edge_list) + 1,] <- c(ars[i,2], ars[i,1], Edge_count)
      Edge_count <- Edge_count + 1
    } else {
      # If the arc is already in the list, skip it
      next
    }
  }
  
  # Return the Blank_edge_list and Edge_count as a list
  return(list(edge_list = Blank_edge_list, edge_count = Edge_count))
}