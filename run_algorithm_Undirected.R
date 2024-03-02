run_algorithm_Undirected <- function(algorithm_undirected, 
                                     # Arcs.Cor_streng_table.alg,
                                     Blank_edge_list, 
                                     Edge_count, 
                                     discretized_data, 
                                     nboot, cl, corrcoef) {
 
  
  # install.packages("rlang")
  # Load the dplyr package
  # library(dplyr)
  
  
  
  Arcs.Cor_table.alg = list()
  
  for (algorithm in algorithm_undirected) {
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl) # undirected
  
  suppressWarnings( # Use suppressWarnings to skip the warning
    ave.dag <- averaged.network(arstr) 
  )
  
  (ars <- arcs(ave.dag))
  if (nrow(ars) == 0) {    # handle the case where there are no arcs
  } else {    # proceed with the loop
    }
  
  ars <- as.data.frame(ars)
  
  # edge_list------------------------------------------
  list_numb_before <- nrow(Blank_edge_list)
  Edge_count_before <- Edge_count
  
  # Call the create_edge_list function to create the Blank_edge_list
  source("create_edge_list.R")
  create_edge <- create_edge_list(Blank_edge_list, Edge_count, ars)
  Blank_edge_list <- create_edge$edge_list
  Edge_count <- create_edge$edge_count
  
  
  # access the updated Blank_edge_list and Edge_count from the function output
  all_edge_list <- Blank_edge_list
  
  # save(all_edge_list, file = paste("Blank_edge_list", ".rda", sep = ' '))
  # write.csv(all_edge_list, paste("Blank_edge_list.csv", sep = ' '), row.names=FALSE)
# 
#   cat("************************************************************************", "\n")
# 
#   cat(sprintf("List is Empty before  undirected algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
#   cat(sprintf("# of rows in edge_list:  ===>  before running algorithm: %d  || after algorithm: %d\n", list_numb_before, nrow(Blank_edge_list)))
#   cat(sprintf("Edge_count:  ===>  before running algorithm: %d  || after algorithm: %d\n", Edge_count_before, Edge_count))
  
  # ---------------------------:   # Add the reverse of each row after the current row
  # Create an empty data frame with the same column names
  ars_both <- data.frame(from = character(), to = character())
  # Loop through each row of the original data frame and add the reverse as a new row after the current row
  for (i in 1:nrow(ars)) {
    ars_both <- rbind(ars_both, ars[i, ])
    ars_both <- rbind(ars_both, data.frame(from = ars[i, "to"], to = ars[i, "from"]))
  }
  # Reset the row names of the new data frame
  rownames(ars_both) <- NULL
  
  # --------------------------- 
  source("calculate_cor_sign.R")
  CorSign <- calculate_cor_sign(ars_both, corrcoef)
  
  # Create a data frame from the columns
  Arcs.Cor_table <- data.frame(ars_both, CorSign)
  
  # -------------
  #  can replace the "save" and "write.csv" function
  
  Arcs.Cor_table.alg[[algorithm]] <- Arcs.Cor_table
  
  # ------------
  
  # # Save the data frame as an R data file
  # save(Arcs.Cor_table, file = paste(file, "Bootstrap", nboot, "_", algorithm, ".rda", sep = ' '))
  # 
  # write.csv(cbind(ars_both, CorSign), paste(file, "Bootstrap", nboot, "_", algorithm, ".csv", sep = ' '), row.names=FALSE)
  # # write.csv(rbind(cbind(ars, CorSign), cbind(rev(ars), CorSign)), paste(file, "Bootstrap", nboot, "_", algorithm_undirected[i], ".csv", sep = ' '), row.names=FALSE)
  
  
  cat("************************************************************************", "\n")
  
  cat(sprintf("Does the list Empty before undirected algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
  cat(sprintf("Number of arcs:  ==>  before the algorithm: %d  || after algorithm: %d\n", list_numb_before, nrow(Blank_edge_list)))
  # cat(sprintf("Number of Edges:  ==>  before running algorithm: %d  || after algorithm: %d\n", Edge_count_before, Edge_count))
  
  
  if (Edge_count_before == Edge_count) {
    cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
  } else {
      
    # Insert code to execute if Edge_count_before and Edge_count are not equal
  } 
  
  }
  # cat("************************************************************************", "\n")
  # cat(sprintf("Final raw nuber of Arcs  %d\n", (Edge_count-1)/2))
  
  return(list (all_edge_list = all_edge_list, Arcs.Cor_table.alg =Arcs.Cor_table.alg))
  }
