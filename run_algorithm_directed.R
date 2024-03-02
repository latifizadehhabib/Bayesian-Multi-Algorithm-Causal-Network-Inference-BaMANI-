run_algorithm_directed <- function(algorithm_directed, Blank_edge_list, Edge_count, 
                                   # discretized_data, nboot, cl, Black_List, white_List= NULL, corrcoef) {
                                   discretized_data, nboot, cl, Black_List, white_List, corrcoef) {
  
  
  
  # --------------------------------------------
    # Debug print statements:
  #   
  #   if (is.na(white_List) || is.na(Black_List)) {
  #     stop("Either white_List or Black_List is NA")
  #   }
  # cat("Type of white_List: ", class(white_List), "\n")
  # print(white_List)
  # cat("White List NULL: ", is.na(white_List), "\n")
  # cat("White List NULL: ", is.null(white_List), "\n")
  # 
  # cat("White List rows: ", nrow(white_List), "\n")
  # cat("Black List NULL: ", is.null(Black_List), "\n")
  # cat("Black List rows: ", nrow(Black_List), "\n")
  # --------------------------------------------
  
  
  
  # arcs.strength.table.alg = list()
  Arcs.Cor_streng_table.alg = list()
  
  
  
  # *---------------------------------------new 2
    
    for (algorithm in algorithm_directed) {
      # white_list_present <- !is.na(white_List) && nrow(white_List) > 0
      # white_list_present <- !is.null(white_List) && !is.na(white_List) && length(white_List) > 0
      white_list_present <- !is.null(white_List) && nrow(white_List) > 0
      
      black_list_present <- !is.null(Black_List) && nrow(Black_List) > 0
      if (white_list_present && black_list_present) {
        arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                               algorithm.args = list(whitelist = white_List, blacklist = Black_List))
        } else if (white_list_present) {
          arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                                 algorithm.args = list(whitelist = white_List))
          } else if(black_list_present) {
            arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                                   algorithm.args = list(blacklist = Black_List))
          }
      
      # }
  
  # ------------------------------------new 1
  # for (algorithm in algorithm_directed) {
  #   # if (!is.na(nrow(white_List)) && !is.na(nrow(Black_List))) {
  #     if ((!is.null(white_List) && nrow(white_List)> 0) &
  #         (!is.null(Black_List) && nrow(Black_List)> 0)) {
  #       arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                              algorithm.args = list(whitelist = white_List, blacklist = Black_List))
  #       } else if ((!is.null(white_List) && nrow(white_List)> 0)) {
  #         arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                                algorithm.args = list(whitelist = white_List))
  #         } else if((!is.null(Black_List) && nrow(Black_List)> 0)) {
  #           arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                                  algorithm.args = list(blacklist = Black_List))
  #         }
  #   # }
    # ------------------------------------
    
  
  # ------------------------------------new
  # for (algorithm in algorithm_directed) {
  #   if ((nrow(white_List)>0) & 
  #       (nrow(Black_List)>0)) {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl, 
  #                            algorithm.args = list(whitelist = white_List, blacklist = Black_List))
  #   } else if ((nrow(white_List)>0)) {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl, 
  #                            algorithm.args = list(whitelist = white_List))
  #   } else if((nrow(Black_List)>0)) {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl, 
  #                            algorithm.args = list(blacklist = Black_List))
  #   }
    
  # ------------------------------------old
  # for (algorithm in algorithm_directed) {
  #   if ((!is.null(white_List) || length(white_List) != 0) &
  #       (!is.null(Black_List) || length(Black_List) != 0)) {
  #           arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                                  algorithm.args = list(whitelist = white_List, blacklist = Black_List))
  #   } else if ((!is.null(white_List) || length(white_List) != 0)) {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                            algorithm.args = list(whitelist = white_List))
  #   } else if((!is.null(Black_List) || length(Black_List) != 0)) {
  #     arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
  #                            algorithm.args = list(blacklist = Black_List))
  #   }
  # ------------------------------------
    

    #Average model:
    
    suppressWarnings( # Use suppressWarnings to skip the warning
      ave.dag <- averaged.network(arstr) 
    )
    
    #------------------------------test 
    # Use cextend() to extend partially directed edges to fully directed edges
    # ave.dag <- cextend(ave.dag)
    #------------------------------ 
    
    #------------------------------original
    arcs(ave.dag) <- directed.arcs(ave.dag) # ignore undirected arcs

        #------------------------------
    ars <- arcs(ave.dag)
    ars <- as.data.frame(ars)
    
    #------------------------------
    
    # BRCA_str = arc.strength(ave.dag, data = discretized_data)
    # weight.strength <- BRCA_str$strength
    # 
    # # arcs.strength.table.alg.names <- c(arcs.strength.table.alg.names, paste("algorithm_ ", algorithm, sep = ""))
    # 
    # DAG.arcs.strength <- data.frame(from = as.character(ars[, 1]), 
    #                                 to = as.character(ars[, 2]), 
    #                                 strength = weight.strength)
    # 
    # arcs.strength.table.alg[[algorithm]] <- DAG.arcs.strength
    
    #------------------------------

    list_numb_before <- nrow(Blank_edge_list)
    Edge_count_before <- Edge_count
    
    # Call the create_edge_list function to create the Blank_edge_list
    source("create_edge_list.R")
    create_edge <- create_edge_list(Blank_edge_list, Edge_count, ars)
    Blank_edge_list <- create_edge$edge_list
    Edge_count <- create_edge$edge_count
    
    # cat("************************************************************************", "\n")
    # 
    # cat(sprintf("List is Empty before  directed algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
    # cat(sprintf("# of rows in edge_list:  ===>  before running algorithm: %d  || after algorithm: %d\n", list_numb_before, nrow(Blank_edge_list)))
    # cat(sprintf("# of Edge:  ===>  before running algorithm: %d   || after algorithm: %d\n", Edge_count_before-1, Edge_count-1))
    # 
    
    # -------------------------------------------
    source("calculate_cor_sign.R")
    CorSign <- calculate_cor_sign(ars, corrcoef)
    
    # Create a data frame from the columns
    Arcs.Cor_streng_table <- data.frame(arc.strength(ave.dag, discretized_data), CorSign)
    
    # ---------------------------------
    #  can replace the "save" and "write.csv" function
    Arcs.Cor_streng_table.alg[[algorithm]] <- Arcs.Cor_streng_table
    
    # ---------------------------------
    
    # # Save the data frame as an R data file
    # save(Arcs.Cor_streng_table, file = paste(file, "Bootstrap", nboot, "_", algorithm, ".rda", sep = ' '))
    # 
    # write.csv(cbind(arc.strength(ave.dag, discretized_data), CorSign),
    #           paste(file, "Bootstrap", nboot, "_", algorithm, ".csv", sep = ' '), row.names = FALSE)
    
    cat("************************************************************************", "\n")
    
    cat(sprintf("Does the list Empty before directed algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
    cat(sprintf("Number of arcs: ==> before the algorithm: %d  || after algorithm: %d\n", list_numb_before, nrow(Blank_edge_list)))
    # cat(sprintf("Number of Edges:  ==>  before running algorithm: %d   || after algorithm: %d\n", Edge_count_before-1, Edge_count-1))
    
    if (Edge_count_before == Edge_count) {
      cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
    } else {
      # Insert code to execute if Edge_count_before and Edge_count are not equal
    }
    }
  # return the updated Blank_edge_list and Edge_count
  # names(Blank_edge_list) <- c("from", "to", "Edge_No")
  # return(edge_list = Blank_edge_list)
  return(list(Arcs.Cor_streng_table.alg = Arcs.Cor_streng_table.alg, 
              # arcs.strength.table.alg = arcs.strength.table.alg, 
              edge_list = Blank_edge_list, 
              edge_count = Edge_count))
  
}
