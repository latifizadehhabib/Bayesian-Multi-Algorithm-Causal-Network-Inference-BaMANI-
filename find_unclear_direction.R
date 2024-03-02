# ---------------------------------------------------------
#   # Important (1: Correct- function form and return unique unclear_direction arcs )
# ---------------------------------------------------------
find_unclear_direction <- function(augmented_edge_list) {
  
  
  
  # ------------------------------------------comapct 2 codes: Not_this_direction
  
  strength_cols <- grep("_strength", colnames(augmented_edge_list), value = TRUE)
  
  # tolerance <- 1e-250
  # Identify rows with all zero or empty values in strength columns  
  
  strength_cols <- grep("_strength", colnames(augmented_edge_list), value = TRUE)
  augmented_edge_list$Not_this_direction <- ifelse(rowSums(augmented_edge_list[strength_cols] == 0 | 
                                                             augmented_edge_list[strength_cols] == "" |
                                                             is.na(augmented_edge_list[strength_cols])) == length(strength_cols),
                                                   "1", "")
  # -------------------------------------------
  
  
  
  # Find pairs of rows that have the same Edge_No value
  augmented_edge_list_pairs <- split(augmented_edge_list, f = augmented_edge_list$Edge_No)
  
  # Define function to apply the "unclear_direction" rule
  unclear_direction <- function(augmented_edge_list_pair) {
    if (nrow(augmented_edge_list_pair) == 2) {
      # from1 <- augmented_edge_list_pair$from[1]      # to1 <- augmented_edge_list_pair$to[1]
      # from2 <- augmented_edge_list_pair$from[2]      # to2 <- augmented_edge_list_pair$to[2]
      
      strength_cols <- grep("_strength", names(augmented_edge_list_pair), value = TRUE)
      if ((any(augmented_edge_list_pair[1, strength_cols] > 0) & any(augmented_edge_list_pair[2, strength_cols] > 0))) {
      # if ((any(augmented_edge_list_pair[1, strength_cols] >= 0) & any(augmented_edge_list_pair[2, strength_cols] >= 0))) {
          
        augmented_edge_list_pair$unclear_direction <- 1
      } else {
        augmented_edge_list_pair$unclear_direction <- ""
      }
    }
    return(augmented_edge_list_pair)
  }
  
  # Apply the "unclear_direction" rule to each pair of rows
  augmented_edge_list_pairs <- lapply(augmented_edge_list_pairs, unclear_direction)
  
  # Combine the pairs back into a single dataframe
  augmented_edge_list_result <- do.call(rbind, augmented_edge_list_pairs)
  
  rownames(augmented_edge_list_result) <- NULL
  # Remove the original Edge_No column
  augmented_edge_list.unclear.arcs <- augmented_edge_list_result
  
 
  # Return the result and the unclear_direction_rows
  return(augmented_edge_list.unclear.arcs)
}








# --------------------------------------------------------------------------------

# # ---------------------------------------------------------
# #   # Important (1: Correct- function form and return unique unclear_direction arcs )
# # ---------------------------------------------------------
# find_unclear_direction <- function(augmented_edge_list) {
#   
#   # Find pairs of rows that have the same Edge_No value
#   augmented_edge_list_pairs <- split(augmented_edge_list, f = augmented_edge_list$Edge_No)
#   
#   # Define function to apply the "unclear_direction" rule
#   unclear_direction <- function(augmented_edge_list_pair) {
#     if (nrow(augmented_edge_list_pair) == 2) {
#       # from1 <- augmented_edge_list_pair$from[1]      # to1 <- augmented_edge_list_pair$to[1]
#       # from2 <- augmented_edge_list_pair$from[2]      # to2 <- augmented_edge_list_pair$to[2]
#       
#       strength_cols <- grep("_strength", names(augmented_edge_list_pair), value = TRUE)
#       if ((any(augmented_edge_list_pair[1, strength_cols] > 0) & any(augmented_edge_list_pair[2, strength_cols] > 0))) {
#         # if ((any(augmented_edge_list_pair[1, strength_cols] >= 0) & any(augmented_edge_list_pair[2, strength_cols] >= 0))) {
#         
#         augmented_edge_list_pair$unclear_direction <- 1
#       } else {
#         augmented_edge_list_pair$unclear_direction <- ""
#       }
#     }
#     return(augmented_edge_list_pair)
#   }
#   
#   # Apply the "unclear_direction" rule to each pair of rows
#   augmented_edge_list_pairs <- lapply(augmented_edge_list_pairs, unclear_direction)
#   
#   # Combine the pairs back into a single dataframe
#   augmented_edge_list_result <- do.call(rbind, augmented_edge_list_pairs)
#   
#   rownames(augmented_edge_list_result) <- NULL
#   # Remove the original Edge_No column
#   unclear_direction_column <- augmented_edge_list_result$unclear_direction
#   
#   # Find the unique rows that have "1" in the "unclear_direction" column
#   unclear_direction_rows <- unique(subset(augmented_edge_list_result, unclear_direction == 1, select = c("Edge_No")))
#   rownames(unclear_direction_rows) <- NULL
#   
#   # Return the result and the unclear_direction_rows
#   return(list(unclear_direction_column = unclear_direction_column, unclear_direction_rows = unclear_direction_rows))
# }
# 
