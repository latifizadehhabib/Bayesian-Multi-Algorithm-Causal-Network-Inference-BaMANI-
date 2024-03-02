calculate_loss_npar_table <- function(threshold, temp_list_merge, discretized_data, nboot, cl, Black_List) {
  npar_list <- list()
  L1_list <- list()
  parents_list = list()
  threshold <- c(0, threshold, "all")
  num_sample <- nrow(discretized_data)
  
  # threshold <- c(as.character(0), as.character(threshold), "all")
  num_arcs.All.thresh <- c()
  num_arcs.All.thresh_name <- c()
  
  # ------------------
  Total.BIC.thresh <- c()
  # ------------------
  
  parents_list_name <- c()
  
  
  
  #-------------------------------------------- new
  for (i in 1: length(threshold)) {
    
    # temp.white <- temp_list_merge[[i+1]]
    # row.names(temp.white) <- NULL
    # temp_white_thresh <- as.data.frame(temp.white)

    temp_white_thresh <- as.data.frame(temp_list_merge[[i+1]], row.names = NULL)
  
    
    # --------------------------
    has_cycles <- function(df_arcs) {
      g <- graph_from_data_frame(d = df_arcs, directed = TRUE)
      return(girth(g)$girth > 0)
    }
    
    
    
    while(has_cycles(temp_white_thresh)) {
      # Here, just remove the last arc as an example, but you can implement a more sophisticated approach
      temp_white_thresh <- temp_white_thresh[-nrow(temp_white_thresh),]
    }
    
    
    # ---------------
    # Check for common arcs and remove them from whitelist
    common_arcs <- intersect(temp_white_thresh, Black_List)
    if(length(common_arcs) > 0) {
      temp_white_thresh <- setdiff(temp_white_thresh, common_arcs)
    }
    # ---------------
    
    
    #-------------------------------------------- new 1
    
    if ((!is.null(temp_white_thresh) && nrow(temp_white_thresh)> 0) &
        (!is.null(Black_List) && nrow(Black_List)> 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
    } else if ((!is.null(temp_white_thresh) && nrow(temp_white_thresh)> 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(whitelist = temp_white_thresh))
    } else if((!is.null(Black_List) && nrow(Black_List)> 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(blacklist = Black_List))
    }
    
    #-------------------------------------------- new 2
    # if ((!is.null(temp_white_thresh) || length(temp_white_thresh) != 0) &
    #     (!is.null(Black_List) || length(Black_List) != 0)) {
    #   arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
    #                          algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
    # } else if ((!is.null(temp_white_thresh) || length(temp_white_thresh) != 0)) {
    #   arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
    #                          algorithm.args = list(whitelist = temp_white_thresh))
    # } else if((!is.null(Black_List) || length(Black_List) != 0)) {
    #   arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
    #                          algorithm.args = list(blacklist = Black_List))
    # }
  
  #-------------------------------------------- old
  # for (i in 1: length(threshold)) {
  #   temp_white_thresh <- as.data.frame(temp_list_merge[[i+1]], row.names = NULL)
  #   if ((is.null(temp_list_merge[[i+1]]) || nrow(temp_list_merge[[i+1]]) == 0)){
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
  #                            algorithm.args = list(blacklist = Black_List))
  #   } else {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
  #                            algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
  #   }
  # -------------------------------------------
    
    suppressWarnings( # Use suppressWarnings to skip the warning
      ave.BRCA <- averaged.network(arstr)
    )
    
    arcs(ave.BRCA) <- directed.arcs(ave.BRCA)
    
    # -------------- number of arcs in the DAG for each thershold
    
    num_arcs.All.thresh_name <- c(num_arcs.All.thresh_name , paste("thresh_# ", threshold[i], sep = ""))
    
    num_arcs.thresh <- length(directed.arcs(ave.BRCA))  # add empty graph in the first element
    num_arcs.All.thresh <- cbind(num_arcs.All.thresh, num_arcs.thresh)
    # --------------
    
    fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)
    
    nodes = nodes(fBRCABN)
    parents_list_thresh  <- list()
    
    for (node in nodes) {
      parents_list_thresh[[node]] = parents(fBRCABN, node)
    }
    
    parents_list_name <- c(parents_list_name , paste("parent_list_thresh_# ", threshold[i], sep = ""))
    parents_list[[i]] <- parents_list_thresh
    
    residBN <- as.data.frame(residuals(fBRCABN))
    residBN <- select_if(residBN, is.numeric)
    
    non_numeric_cols <- !sapply(residBN, is.numeric)
    if (any(non_numeric_cols)) {
      residBN[, non_numeric_cols] <- lapply(residBN[, non_numeric_cols], as.numeric)
    }
    
    L1_BN <- colSums(abs(residBN)/nrow(discretized_data))
    
    npar <- sapply(nodes(ave.BRCA), function(x) length(parents(fBRCABN,x)))
    # -------------------
    Total.Lost.thresh <- sum(L1_BN)*num_sample + sum(npar)* log(num_sample)

    Total.BIC.thresh <- cbind(Total.BIC.thresh, Total.Lost.thresh)

    # -------------------
    npar_name <- paste("npar_thresh_# ", threshold[i], sep = "")
    npar_list[[i]] <- data.frame(node = names(npar), npar = npar)
    rownames(npar_list[[i]]) <- NULL
    colnames(npar_list[[i]])[2] <- npar_name
    
    L1_name <- paste("L1_thresh_#", threshold[i], sep = "")
    L1_list[[i]] <- data.frame(node = names(L1_BN), L1 = L1_BN)
    rownames(L1_list[[i]]) <- NULL
    colnames(L1_list[[i]])[2] <- L1_name
  }
  
  npar_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), npar_list)
  L1_BN_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), L1_list)
  
  ng <- empty.graph(nodes = (as.character(colnames(discretized_data))))
  fnBN <- bn.fit(ng, data = discretized_data)
  residnBN <- as.data.frame(residuals(fnBN))
  
  L1_nBN <- colSums(abs(residnBN)/nrow(discretized_data))
  temp_L1_nBN <- L1_nBN
  names(L1_nBN)
  L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = L1_nBN)
  rownames(L1_nBN) <- NULL
  
  L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all = TRUE)
  L1_merged[is.na(L1_merged)] <- 0
  # --------------
  rownames(L1_merged) <- L1_merged$node  # Set the row names to the first column
  L1_merged <- L1_merged[, -1]  # Remove the first column
  
# --------------
  # ---------------------------
  # For each column we want to round, check if it's numeric. If it is, round it.
  for (col in names(L1_merged)) {
    if (is.numeric(L1_merged[[col]])) {
      L1_merged[[col]] <- as.numeric(sprintf("%.1f", L1_merged[[col]]))
    }
  }
  
  
  #---------------------------
  
  
  
  npar_nBN <- data.frame(node = npar_table$node, npar_empty_graph = rep(0, length(npar_table$node)))
  npar_merged <- merge(npar_nBN, npar_table, by = "node", all = TRUE)
  # --------------
  rownames(npar_merged) <- npar_merged$node  # Set the row names to the first column
  npar_merged <- npar_merged[, -1]  # Remove the first column
  # --------------

  num_arcs.All.thresh_name <- c("thresh_#empty_graph", num_arcs.All.thresh_name)
  
  num_arcs.All.thresh <- c(as.integer(0) , num_arcs.All.thresh)
  names(num_arcs.All.thresh) <- num_arcs.All.thresh_name
  
  # -------------------
  Total.Lost.thresh <- sum(temp_L1_nBN)*num_sample   # this term is zero because empty graph has zero parents: + sum(npar_nBN)* log(num_sample)
  Total.BIC.thresh <- c(Total.Lost.thresh, Total.BIC.thresh)
  names(Total.BIC.thresh) <- num_arcs.All.thresh_name
  
  # -------------------
  

  parents_list_name <- c("parent_list_empty_graph" , parents_list_name)
  
  parents_list_empty_graph <- vector(mode = "list", length = nrow(npar_merged))
  names(parents_list_empty_graph) <- names(parents_list[[2]])
  parents_list_empty_graph[] <- rep(list(NULL), length(parents_list_empty_graph))
  # parents_list_empty_graph[] <- list(0)
  
  parents_list_merged <- list() 
  parents_list_merged <- c(list(parents_list_empty_graph), parents_list)
  names(parents_list_merged) <- parents_list_name
  
  
  # return(list(npar_merged = npar_merged, L1_merged = L1_merged,
  #             parents_list_merged = parents_list_merged, 
  #             num_arcs.All.thresh = num_arcs.All.thresh))
  # -------------------
  return(list(npar_merged = npar_merged, L1_merged = L1_merged,
              parents_list_merged = parents_list_merged,
              num_arcs.All.thresh = num_arcs.All.thresh,
              Total.BIC.thresh = Total.BIC.thresh))
  
  # -------------------
}



#--------------------------------------------------------------------------------------------
# final with comment
#--------------------------------------------------------------------------------------------

# 
# calculate_loss_npar_table <- function(threshold, temp_list_merge, discretized_data, nboot, cl, Black_List) {
#   npar_list <- list()
#   L1_list <- list()
#   # initialize an empty list to store the results
#   parents_list = list()
#   
#   
#   threshold <- c(as.character(0), threshold, "all")  # add zero threshold
#   
#   parents_list_name <- c()
#   
#   # Loop over the thresholds and create a data frame for each
#   for (i in 1: length(threshold)) { # because the first list is for empty graph
#     temp_white_thresh <- as.data.frame(temp_list_merge[[i+1]], row.names = NULL)  
#     if ((is.null(temp_list_merge[[i+1]]) || nrow(temp_list_merge[[i+1]]) == 0)){
#       arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                              algorithm.args = list(blacklist = Black_List))
#       } else {
#         arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                                algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
#       }
#     # arstr[(arstr$strength > 0.85) & (arstr$direction >= 0.5), ]
#     ave.BRCA <- averaged.network(arstr)
#     arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
#     
#     # arcs(ave.BRCA) <- directed.arcs(ave.BRCA)
#     # ars.BRCA <- arcs(ave.BRCA)
#     fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)  # numbering for each threshold
#     
#     # ---------------- SAVING PARENTS OF EACH NODE
#     # get all nodes in the graph
#     nodes = nodes(fBRCABN)
#       
#     parents_list_thresh = list()
#     
#     # loop through each node and store its parents in the list
#     for (node in nodes) {
#       parents_list_thresh[[node]] = parents(fBRCABN, node)
#     }
#     
#     # Name the first list "numbers" and the second list "letters"
#     parents_list_name <- c(parents_list_name , paste("parent_list_thresh_# ", threshold[i], sep = ""))
#    
#     parents_list[[i]] <- parents_list_thresh  # since i start from 2 because empty graph is a first list
#     
#         # ----------------
#     
#     # residuals() (and its alias resid()) extracts model residuals and fitted()
#     # (and its alias fitted.values()) extracts fitted values from Gaussian and conditional Gaussian nodes. 
#     str(residuals(fBRCABN))
#     
#     #### Calculate L1 loss for each node
#     residBN <- as.data.frame(residuals(fBRCABN)) # calculating residuals for each sample (58 samples SKCM) of each features (Cancer, CD4, CD8,..)\
#     # residBN <- as.data.frame(lapply(residuals(fBRCABN), as.numeric))
#     residBN <- select_if(residBN, is.numeric)  # newly added (the function colSums() can only be applied to data frames with all numeric variables. In this case, residBN may contain non-numeric columns, which is causing the error.
#     
#     # Check for non-numeric columns
#     non_numeric_cols <- !sapply(residBN, is.numeric)
#     if (any(non_numeric_cols)) {
#       residBN[, non_numeric_cols] <- lapply(residBN[, non_numeric_cols], as.numeric)
#     }
#     
#     
#     L1_BN <- colSums(abs(residBN)/nrow(discretized_data)) # divide the residual columns to the # of sample (normalization) ---->  then calculate sum of each column (Cancer, CD4, CD8,..)
#     npar <- sapply(nodes(ave.BRCA), function(x) length(parents(fBRCABN,x)))
#     
#     npar_name <- paste("npar_thresh_# ", threshold[i], sep = "")
#     npar_list[[i]] <- data.frame(node = names(npar), npar = npar)
#     rownames(npar_list[[i]]) <- NULL  # view(npar_list)
#     colnames(npar_list[[i]])[2] <- npar_name
#     
#     L1_name <- paste("L1_thresh_#", threshold[i], sep = "")
#     L1_list[[i]] <- data.frame(node = names(L1_BN), L1 = L1_BN)
#     rownames(L1_list[[i]]) <- NULL  # view(L1_list)
#     colnames(L1_list[[i]])[2] <- L1_name
#   }
#   
#   # Merge all the data frames in the list based on the "node" column
#   npar_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), npar_list)
#   L1_BN_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), L1_list)
#   
#   # npar_table <- Reduce(function(x, y) merge(x, y, by = "node", all.x = TRUE), npar_list)
#   # L1_BN_table <- Reduce(function(x, y) merge(x, y, by = "node", all.x = TRUE), L1_list)
#   
#   
#   # Calculate loss for empty graph
#   ng <- empty.graph(nodes = (as.character(colnames(discretized_data))))
#   # ng <- empty.graph(nodes = nodes(ave.BRCA))
#   fnBN <- bn.fit(ng, data = discretized_data)
# 
#   residnBN <- as.data.frame(residuals(fnBN))
#   L1_nBN <- colSums(abs(residnBN)/nrow(discretized_data))
# 
#   names(L1_nBN)
#   L1_nBN <- data.frame(node = L1_BN_table$node, L1_empty = L1_nBN)
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = rep(0, length(L1_nBN)))
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = as.numeric(L1_nBN), stringsAsFactors = FALSE)
#     # Merge L1_nBN and L1_BN_table on the "node" column
#   L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all = TRUE)
#   # L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all.x = TRUE)
# 
#     # Replace any missing values with 0
#   L1_merged[is.na(L1_merged)] <- 0
# 
# 
#   npar_nBN <- data.frame(node = npar_table$node, npar_empty = rep(0, length(npar_table$node)))
#   npar_merged <- merge(npar_nBN, npar_table, by = "node", all = TRUE)
#   # npar_merged <- merge(npar_nBN, npar_table, by = "node", all.x = TRUE)
# 
#   # Replace any missing values with 0
#   # npar_merged[is.na(npar_merged)] <- 0
# 
#    # Return a list of the two data frames
#   
#   # ------------------------
#   # adding BIC score for each threshold 
#   
#   # --------------------- to assign name to each list of "parents_list_thresh"
#   parents_list_name <- c("parent_list_empty_graph" , parents_list_name)
#   
#   #  making a list with Zero value as zero parents for each node for an empty graph
#   # parents_list_empty_graph <- lapply(vector("list", length(npar_table$node)), function(x) 0) ########????????????????????????????
#   # parents_list_empty_graph <- setNames(rep(list(0), nrow(npar_merged), npar_merged$node))
#   # parents_list_empty_graph <- replicate(length(npar_table$node), 0, simplify = FALSE)
#   # ------------
#   
#             # Create a list of length equal to the number of rows in npar_merged
#             parents_list_empty_graph <- vector(mode = "list", length = nrow(npar_merged))
# 
#             # Set the names of the list elements to the values in rownames(npar_merged)
#             names(parents_list_empty_graph) <- npar_merged$node
# 
#             # Set each list element to 0
#             parents_list_empty_graph[] <- list(0)
# 
#   # ------------
#   # parents_list_empty_graph <- setNames(rep(list(0), nrow(npar_merged), npar_merged$node))
#   # ------------
#   parents_list_merged <- c(list(parents_list_empty_graph), parents_list)  # add the new list for empty graph to the "list of list"
#   
#   names(parents_list_merged) <- parents_list_name
#   
#   # Access the "numbers" list inside my_list
#   # parents_list_merged[["parents_list_thresh....."]]
#   
#   # ---------------------
#   
#   # ------------------------
#   return(list(npar_merged = npar_merged, L1_merged = L1_merged, parents_list_merged = parents_list_merged))
# 
#   
#   # return(list(npar_table = npar_table, L1_BN_table = L1_BN_table))
#   
# }









#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

# 2 Original code: worked for making table for number of parent and L1 loss
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

# calculate_loss_npar_table <- function(threshold, temp_list_merge, discretized_data, nboot, cl, Black_List) {
#   npar_list <- list()
#   L1_list <- list()
#   
#   threshold <- c(0, threshold)  # add zero threshold
#   
#   # Loop over the thresholds and create a data frame for each
#   for (i in seq_along(threshold)) {
#     temp_white_thresh <- as.data.frame(temp_list_merge[[i]], row.names = NULL)
#     if ((is.null(temp_list_merge[[1]]) || nrow(temp_list_merge[[1]]) == 0)){
#       arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                              algorithm.args = list(blacklist = Black_List))
#     } else {
#       arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                              algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
#     }
#     attr(arstr, "threshold")
#     # arstr[(arstr$strength > 0.85) & (arstr$direction >= 0.5), ]
#     ave.BRCA <- averaged.network(arstr)
#     arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
#     
#     # arcs(ave.BRCA) <- directed.arcs(ave.BRCA)
#     # ars.BRCA <- arcs(ave.BRCA)
#     fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)  # numbering for each threshold
#     
#     # residuals() (and its alias resid()) extracts model residuals and fitted()
#     # (and its alias fitted.values()) extracts fitted values from Gaussian and conditional Gaussian nodes. 
#     str(residuals(fBRCABN))
#     
#     #### Calculate L1 loss for each node
#     residBN <- as.data.frame(residuals(fBRCABN)) # calculating residuals for each sample (58 samples SKCM) of each features (Cancer, CD4, CD8,..)\
#     # residBN <- as.data.frame(lapply(residuals(fBRCABN), as.numeric))
#     residBN <- select_if(residBN, is.numeric)  # newly added (the function colSums() can only be applied to data frames with all numeric variables. In this case, residBN may contain non-numeric columns, which is causing the error.
#     
#     # Check for non-numeric columns
#     non_numeric_cols <- !sapply(residBN, is.numeric)
#     if (any(non_numeric_cols)) {
#       residBN[, non_numeric_cols] <- lapply(residBN[, non_numeric_cols], as.numeric)
#     }
#     
#     L1_BN <- colSums(abs(residBN)/nrow(discretized_data)) # divide the residual columns to the # of sample (normalization) ---->  then calculate sum of each column (Cancer, CD4, CD8,..)
#     npar <- sapply(nodes(ave.BRCA), function(x) length(parents(fBRCABN,x)))
#     
#     npar_name <- paste("npar_thresh_# ", format(threshold[i], scientific = TRUE), sep = "")
#     npar_list[[i]] <- data.frame(node = names(npar), npar = npar)
#     rownames(npar_list[[i]]) <- NULL  # view(npar_list)
#     colnames(npar_list[[i]])[2] <- npar_name
#     
#     L1_name <- paste("L1_thresh_#", format(threshold[i], scientific = TRUE), sep = "")
#     L1_list[[i]] <- data.frame(node = names(L1_BN), L1 = L1_BN)
#     rownames(L1_list[[i]]) <- NULL  # view(L1_list)
#     colnames(L1_list[[i]])[2] <- L1_name
#   }
#   
#   # Merge all the data frames in the list based on the "node" column
#   npar_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), npar_list)
#   L1_BN_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), L1_list)
#   
#   
#   # Calculate loss for empty graph
#   ng <- empty.graph(nodes = (as.character(colnames(discretized_data))))
#   # ng <- empty.graph(nodes = nodes(ave.BRCA))
#   fnBN <- bn.fit(ng, data = discretized_data)
#   
#   residnBN <- as.data.frame(residuals(fnBN))
#   L1_nBN <- colSums(abs(residnBN)/nrow(discretized_data))
#   
#   names(L1_nBN)
#   L1_nBN <- data.frame(node = L1_BN_table$node, L1_empty = L1_nBN)
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = rep(0, length(L1_nBN)))
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = as.numeric(L1_nBN), stringsAsFactors = FALSE)
#   # Merge L1_nBN and L1_BN_table on the "node" column
#   L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all = TRUE)
#   # L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all.x = TRUE)
#   
#   # Replace any missing values with 0
#   L1_merged[is.na(L1_merged)] <- 0
#   
#   
#   npar_nBN <- data.frame(node = npar_table$node, npar_empty = rep(0, length(npar_table$node)))
#   npar_merged <- merge(npar_nBN, npar_table, by = "node", all = TRUE)
#   # npar_merged <- merge(npar_nBN, npar_table, by = "node", all.x = TRUE)
#   
#   # Replace any missing values with 0
#   # npar_merged[is.na(npar_merged)] <- 0
#   
#   # Return a list of the two data frames
#   
#   # ------------------------
#   # adding BIC score for each threshold 
#   
#   
#   
#   # ------------------------
#   return(list(npar_merged = npar_merged, L1_merged = L1_merged))
#   
#   
#   # return(list(npar_table = npar_table, L1_BN_table = L1_BN_table))
#   
# }

# 

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

# 1 Original code: worked for making table for number of parent and L1 loss
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------


# calculate_loss_npar_table <- function(threshold, temp_list_merge, discretized_data, nboot, cl, Black_List) {
#   npar_list <- list()
#   L1_list <- list()
#   
#   threshold <- c(0, threshold)  # add zero threshold
#   
#   # Loop over the thresholds and create a data frame for each
#   for (i in seq_along(threshold)) {
#     temp_white_thresh <- as.data.frame(temp_list_merge[[i]], row.names = NULL)
#     if ((is.null(temp_list_merge[[1]]) || nrow(temp_list_merge[[1]]) == 0)){
#       arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                              algorithm.args = list(blacklist = Black_List))
#     } else {
#       arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, 
#                              algorithm.args = list(whitelist = temp_white_thresh, blacklist = Black_List))
#     }
#     attr(arstr, "threshold")
#     # arstr[(arstr$strength > 0.85) & (arstr$direction >= 0.5), ]
#     ave.BRCA <- averaged.network(arstr)
#     arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
#     
#     # arcs(ave.BRCA) <- directed.arcs(ave.BRCA)
#     # ars.BRCA <- arcs(ave.BRCA)
#     fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)  # numbering for each threshold
#     
#     # residuals() (and its alias resid()) extracts model residuals and fitted()
#     # (and its alias fitted.values()) extracts fitted values from Gaussian and conditional Gaussian nodes. 
#     str(residuals(fBRCABN))
#     
#     #### Calculate L1 loss for each node
#     residBN <- as.data.frame(residuals(fBRCABN)) # calculating residuals for each sample (58 samples SKCM) of each features (Cancer, CD4, CD8,..)\
#     # residBN <- as.data.frame(lapply(residuals(fBRCABN), as.numeric))
#     residBN <- select_if(residBN, is.numeric)  # newly added (the function colSums() can only be applied to data frames with all numeric variables. In this case, residBN may contain non-numeric columns, which is causing the error.
#     
#     # Check for non-numeric columns
#     non_numeric_cols <- !sapply(residBN, is.numeric)
#     if (any(non_numeric_cols)) {
#       residBN[, non_numeric_cols] <- lapply(residBN[, non_numeric_cols], as.numeric)
#     }
#     
#     L1_BN <- colSums(abs(residBN)/nrow(discretized_data)) # divide the residual columns to the # of sample (normalization) ---->  then calculate sum of each column (Cancer, CD4, CD8,..)
#     npar <- sapply(nodes(ave.BRCA), function(x) length(parents(fBRCABN,x)))
#     
#     npar_name <- paste("npar_thresh_# ", format(threshold[i], scientific = TRUE), sep = "")
#     npar_list[[i]] <- data.frame(node = names(npar), npar = npar)
#     rownames(npar_list[[i]]) <- NULL  # view(npar_list)
#     colnames(npar_list[[i]])[2] <- npar_name
#     
#     L1_name <- paste("L1_thresh_#", format(threshold[i], scientific = TRUE), sep = "")
#     L1_list[[i]] <- data.frame(node = names(L1_BN), L1 = L1_BN)
#     rownames(L1_list[[i]]) <- NULL  # view(L1_list)
#     colnames(L1_list[[i]])[2] <- L1_name
#   }
#   
#   # Merge all the data frames in the list based on the "node" column
#   npar_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), npar_list)
#   L1_BN_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), L1_list)
#   
#   
#   # Calculate loss for empty graph
#   ng <- empty.graph(nodes = (as.character(colnames(discretized_data))))
#   # ng <- empty.graph(nodes = nodes(ave.BRCA))
#   fnBN <- bn.fit(ng, data = discretized_data)
#   
#   residnBN <- as.data.frame(residuals(fnBN))
#   L1_nBN <- colSums(abs(residnBN)/nrow(discretized_data))
#   
#   names(L1_nBN)
#   L1_nBN <- data.frame(node = L1_BN_table$node, L1_empty = rep(0, length(L1_BN_table$node)))
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = rep(0, length(L1_nBN)))
#   # L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = as.numeric(L1_nBN), stringsAsFactors = FALSE)
#   # Merge L1_nBN and L1_BN_table on the "node" column
#   L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all = TRUE)
#   # L1_merged <- merge(L1_nBN, L1_BN_table, by = "node", all.x = TRUE)
#   
#   # Replace any missing values with 0
#   L1_merged[is.na(L1_merged)] <- 0
#   
#   
#   npar_nBN <- data.frame(node = npar_table$node, npar_empty = rep(0, length(npar_table$node)))
#   npar_merged <- merge(npar_nBN, npar_table, by = "node", all = TRUE)
#   # npar_merged <- merge(npar_nBN, npar_table, by = "node", all.x = TRUE)
#   
#   # Replace any missing values with 0
#   # npar_merged[is.na(npar_merged)] <- 0
#   
#   # Return a list of the two data frames
#   return(list(npar_merged = npar_merged, L1_merged = L1_merged))
#   
#   
#   # return(list(npar_table = npar_table, L1_BN_table = L1_BN_table))
#   
# }

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
