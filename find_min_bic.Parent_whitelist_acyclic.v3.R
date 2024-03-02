find_min_bic.Parent_whitelist_acyclic.v3 <- function(BIC_merged_table, npar_merged, parents_list_merged) {

  # library(igraph)
  bic_table_temp<- BIC_merged_table
  
  bic_min <- apply(bic_table_temp, 1, min)
  
  bic_min_index <- vector("numeric", nrow(bic_table_temp))
  
  for (i in 1:nrow(bic_table_temp)) {
    min_indices <- which(bic_table_temp[i,] == min(bic_table_temp[i,]))
    if (length(min_indices) > 1) {
      parent_cols <- colnames(npar_merged)
      parent_counts <- numeric(length(parent_cols))
      
      for (j in seq_along(parent_cols)) {
        parent_counts[j] <- npar_merged[i, parent_cols[j]]
        # parent_counts[j] <- min(npar_merged[i, parent_cols[j]], na.rm = TRUE)
      }
      #-------------------
      min_parent_count <- min(parent_counts[min_indices], na.rm = TRUE)
      
      min_parent_col <- parent_cols[parent_counts == min_parent_count][1]
      # min_parent_col <- parent_cols[parent_counts == min_parent_count][1]
      
      min_index <- match(min_parent_col,  colnames(npar_merged))
      # min_index <- match(min_parent_col, colnames(BIC_merged_table))
    } else {
      min_index <- which(bic_table_temp[i,] == min(bic_table_temp[i,]))
    }
    bic_min_index[i] <- min_index
  }
  
  bic_min_table <- data.frame(bic_min = bic_min, bic_min_index = bic_min_index)
  
  # --------------------------------------------------
  
  # Find parent nodes for each variable (node)
  nodes <- rownames(bic_min_table)
  parent_nodes <- lapply(nodes, function(node) {
    # the index of the row with the minimum BIC score for this node
    min_index <- bic_min_table[node, "bic_min_index"]
    # Get the parents for this variable based on the index
    # parents <- parents_list_merged[[min_index]][[node]]
    parents <- if(is.null(parents_list_merged[[min_index]][[node]])) 
      character() 
    else 
      parents_list_merged[[min_index]][[node]]
    # Return a data frame with the node name and its parent nodes
    # data.frame(from = rep(node, length(parents)), to = parents)
    data.frame(from = parents, to = rep(node, length(parents)))
  })
  
  # Combine the parent nodes for all variables into a single data frame
  possible.white.list <- do.call(rbind, parent_nodes)
  
  # --------------------------------------------------------------
  # Retain only the 'from' and 'to' columns of the data frame
  # possible.white.list <- possible.white.list[, c("from", "to")]
  
  
  # --------------------------------------------------------------
  # Check which arcs are making cycle of its reverse existed in the list
  # ---------------------------------------------
  # cycleRemovalFunction <- function(possible.white.list) {
  #   
  #   g <- graph_from_data_frame(possible.white.list, directed = TRUE)
  #   
  #   find_cycles <- function(graph) {
  #     sub_graphs <- decompose(graph, min.vertices = 2)
  #     cycles <- lapply(sub_graphs, function(sub_g) {
  #       if(girth(sub_g)$girth > 0) return(sub_g)
  #     })
  #     cycles <- Filter(Negate(is.null), cycles)
  #     return(cycles)
  #   }
  #   
  #   # Initial Graph Plot
  #   initialPlot <- function() {
  #     plot(g, vertex.color = "lightcoral", main = "Initial Graph")
  #   }
  #   
  #   cycles <- find_cycles(g)
  #   g_final <- g
  #   for (cycle in cycles) {
  #     # For simplicity, we just remove the first edge of each cycle in this example
  #     g_final <- delete_edges(g_final, E(cycle)[1])
  #   }
  #   
  #   # Check and auto-remove remaining cycles
  #   while(TRUE) {
  #     cycles <- find_cycles(g_final)
  #     if(length(cycles) == 0) break
  #     g_final <- delete_edges(g_final, E(cycles[[1]])[1])
  #   }
  #   
  #   final_white_list <- as.data.frame(get.edgelist(g_final, names = TRUE))
  #   
  #   # Final Graph Plot
  #   finalPlot <- function() {
  #     plot(g_final, main = "Final White list after Cycle check", vertex.color = "lightcoral")
  #   }
  #   return(list(final_white_list = final_white_list, plot_before = initialPlot, plot_after = finalPlot))
  # }
  # ---------------------------------------------
  return(list(
    bic_min_table = bic_min_table, 
    possible.white.list = possible.white.list
  ))
}
  # ---------------------------------------------
#   plot_graph_result <- cycleRemovalFunction(possible.white.list)
#   
#   return(list(
#     bic_min_table = bic_min_table, 
#     possible.white.list = possible.white.list, 
#     # plot_before = plot_graph_result$plot_before, 
#     plot_before = function() {
#       plot_graph_result$plot_before()
#     },
#     final_white_list = plot_graph_result$final_white_list, 
#     # plot_after = plot_graph_result$plot_after
#     plot_after = function() {
#       plot_graph_result$plot_after()
#     }
#   ))
# }
# ---------------------------------------------
# Run modified function
# ---------------------------------------------
#   plot_graph_result<- plot_graph_with_and_without_cycles(possible.white.list)
#   final_white_list<- plot_graph_result$final_white_list
#   plot_before<- plot_graph_result$plot_before
#   plot_after<- plot_graph_result$plot_after
#   
#   # --------------------------------------------------
#   
#   return(list(
#     bic_min_table = bic_min_table, 
#     possible.white.list = possible.white.list, 
#     plot_before = plot_before, 
#     final_white_list = final_white_list, 
#     plot_after = plot_after
#   ))
# }
# --------------------------------------------------

#   return(list(bic_min_table = bic_min_table,
#               possible.white.list = possible.white.list,
# 
#               plot_before = function() {
#                 plot_before
#                 },
#               # plot_before = plot_before,
#               final_white_list = final_white_list,
#               plot_after = function() {
#                 plot_after
#               }
#               # plot_after = plot_after
#               )
#          )
# }
# 
# 
