  library(bnlearn)

  # ------------------------------------------------------------
  Final.DAG_network_plot_v6 <- function(augmented_edge_list,
                                        possible_seed_arcs_filter,
                                        data, discretized_data,
                                        possible.white.list,
                                        Black_List,
                                        nboot, cl,
                                        corrcoef,
                                        Status,
                                        userSelected_key_feature) {


  print("Starting Final.DAG_network_plot_v6 with inputs:")
  print(list(
    Status = Status,
    userSelected_key_feature = userSelected_key_feature,
    nboot = nboot
    # Add other inputs as needed
  ))

  print("hi im status ")
  print(Status)
  print("and im other")
  print(userSelected_key_feature)
  print("WHITE LIST")
  print(possible.white.list)

  possible.white.list <- as.data.frame(possible.white.list)
  discretized_data <- as.data.frame(discretized_data)
  Black_List <- as.data.frame(Black_List)

  print("After discretized_data processing:")
  print(head(discretized_data))

  # Check the structure and content of possible.white.list and Black_List
  print("Structure and content of possible.white.list:")
  print(str(possible.white.list))
  print(head(possible.white.list))

  print("Structure and content of Black_List:")
  print(str(Black_List))
  print(head(Black_List))

  print("Dimensions of possible.white.list:")
  print(dim(possible.white.list)) 

  print("Dimensions of Black_List:")
  print(dim(Black_List))
  print("nrow")
  print(nrow(possible.white.list))
  print("condi2")
  print(names(possible.white.list))

  if (nrow(possible.white.list) > 0) {
  print("i made it here 2")

    # Safe to proceed with operations on possible.white.list

  # Check if possible.white.list is empty
  if (ncol(possible.white.list) == 0) {
    # Handle the empty case - for example, by skipping the intersection operation
    # Or initialize possible.white.list to match the structure of Black_List but with no rows
    possible.white.list <- data.frame(from=character(), to=character())
  } else{
    # Now, proceed with the intersection (or any other operation) as intended
    # common_arcs <- intersect(possible.white.list, Black_List)

    # Check for common arcs and remove them from whitelist
    common_arcs <- intersect(possible.white.list, Black_List)
    if(length(common_arcs) > 0) {
      possible.white.list <- setdiff(possible.white.list, common_arcs)
    }
  }
  print("i made it here 4")

  # ---------------
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, algorithm.args = list(whitelist = possible.white.list, blacklist = Black_List))
  ave.BRCA <- averaged.network(arstr)

  arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
  arcs.BRCA <- arcs(ave.BRCA)
  arcs.BRCA <- as.data.frame(arcs.BRCA)

  fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)


  # ----------------
  BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
  weight.strength <- BRCA_str$strength


  # ----------------------------------
  arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())

  print("i made it here 5")

  # iterate over all nodes in the network
  for(node in nodes(fBRCABN)) {
    # get the names of the parent nodes
    parents <- parents(fBRCABN, node)
    
    # iterate over all parent nodes and get the slope coefficients
    for(parent in parents) {
      # get the slope coefficient
      slope <- coef(fBRCABN)[[node]][[parent]]
      
      # add the arc and slope coefficient to the data frame
      arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
    }
  }
  arcs_strength <- data.frame(from = as.character(arcs.BRCA[, 1]), 
                              to = as.character(arcs.BRCA[, 2]), 
                              P_strength = weight.strength)

  # arc_slopes.strength <- merge(arcs_strength, arc_slopes)
  arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)


  #------------------------------------------------  visNetwork
  # Apply the transformation
  transformed_values <- sapply(arcs_strength$P_strength, function(p) -log10(p))
  # transformed_values <- sapply(P_strength, function(p) -log10(p))


  # Handle infinite values after transformation
  max_value <- max(transformed_values[!is.infinite(transformed_values)], na.rm = TRUE)
  transformed_values[is.infinite(transformed_values)] <- max_value

  # Normalize the transformed values
  normalized_weights <- rescale(transformed_values, to = c(0, 1))
  #------------------------------------------------ 
  # create nodes and edges data frames

  nodes <- data.frame(id = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)), 
                      label = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)))

  # --------------------------

  final_DAG_detail <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    color = ifelse(arc_slopes.strength$slope < 0, "red", "black"), 
    Effect_Size = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  # Added spaces
    Arc_strength = paste0("  ", as.character(signif(normalized_weights, digits = 2)), "  ")   # use the normalized arc strength as the value
  )
  # --------------------------
  edges <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    arrows = 'to', 
    color = ifelse(arc_slopes.strength$slope < 0, "red", "black"), 
    label = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  # Added spaces
    value = normalized_weights  # use the normalized arc strength as the value
  )
  # --------------------------
  # --------------------------

  network <- visNetwork(nodes, edges, width = "100%") %>%
    visNodes(shape = "circle",
            font = list(size = 12, 
                        vadjust = 0, 
                        bold = TRUE, 
                        color = "black")) %>%
    visEdges(smooth = TRUE, 
            value = "value",  
            font = list(size = 15, color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
    visOptions(highlightNearest = list(enabled = T, hover = T),
              nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123, 
              improvedLayout = TRUE)  %>%
    visPhysics(solver = "forceAtlas2Based",  # The physics solver
              forceAtlas2Based = list(gravitationalConstant = -50,  # Adjust as needed
                                      centralGravity = 0.005,  # Adjust as needed
                                      springLength = 100,  # Adjust as needed
                                      springConstant = 0.18))  # Adjust as needed 


  print("Network object created:")
  print(summary(network))  # or simply print(network) if that's more informative

  # -----
  performCpdistWithChecks <- function(bnModel, data, statusColumnName, keyFeature, cellType) {
    print("IM IN HEREEEEEE")
    if(!inherits(bnModel, "bn.fit")) {
      print("bnModel should be a bn.fit object.")
      return(NULL)
    }
    if(!is.data.frame(data)) {
      print("data should be a data frame.")
      return(NULL)
    }
    if(!(statusColumnName %in% names(data))) {
      print(paste("Status column", statusColumnName, "not found in data columns."))
      return(NULL)
    }
    if(!(keyFeature %in% names(data))) {
      print(paste("keyFeature", keyFeature, "not found in data columns."))
      return(NULL)
    }
    if(!(cellType %in% names(data))) {
      print(paste("cellType", cellType, "not found in data columns."))
      return(NULL)
    }

    # Assuming binary 1/0 represents high/low respectively
    # Map your actual data accordingly if it uses different representations
    evidence_high <- list(statusColumnName = "1")  # Replace "1" with actual high representation if different
    evidence_low <- list(statusColumnName = "0")   # Replace "0" with actual low representation if different

    # Now perform cpdist with these evidence lists
    tryCatch({
      cpDistResultsHigh <- cpdist(bnModel, nodes = c(keyFeature, cellType), evidence = evidence_high, n = 1e4)
      print("cpdist executed successfully for high status.")
    }, error = function(e) {
      print(paste("ERROR in cpdist for high status:", conditionMessage(e)))
    })

    tryCatch({
      cpDistResultsLow <- cpdist(bnModel, nodes = c(keyFeature, cellType), evidence = evidence_low, n = 1e4)
      print("cpdist executed successfully for low status.")
    }, error = function(e) {
      print(paste("ERROR in cpdist for low status:", conditionMessage(e)))
    })
  }

  # ----------------------------------- final legend line  Cantour Plot 
  generatePlot <- function(status, key_feature, cellType, fBRCABN, data) {

      # Validate inputs
      if(!is.character(cellType) || length(cellType) != 1)
        stop("cellType should be a single character value.")

      if(!inherits(fBRCABN, "bn.fit"))
        stop("fBRCABN should be a bn.fit object.")
      
      if(!is.data.frame(data) || !all(c(key_feature, cellType, status) %in% names(data))){
        stop("Invalid data frame.")
      }



  # Prepare evidence directly from the binary values in the specified 'Status' column
    # Here, we assume the BN model expects evidence in the form of character values "1" or "0"
    #rint("ia m the key feature")
    #print(key_feature)
    #browser()
    print("Defining evidence_high and evidence_low")
    #evidence_high <<- (status >= 0.95)
    #evidence_low <<- (status <= 0.05)
    #print(evidence_high)
    #print(evidence_low)
    #print(ls())


  #print(bnlearn::nodes(fBRCABN))
  #print(bnlearn::nodeStates(fBRCABN))
  print("hi im status")
  print(status)
  # Example cpdist call adjusted
  sim1 <- tryCatch(cpdist(fBRCABN, nodes = c(userSelected_key_feature, cellType), n = 10^5, evidence =  setNames(list(1), status), method="lw"), error = function(e) {print(e$message)})
  sim2 <- tryCatch(cpdist(fBRCABN, nodes = c(userSelected_key_feature, cellType), n = 10^6, evidence = setNames(list(0), status), method = "lw"), error = function(e) {print(e$message)})
  

  # sim1 <- tryCatch(
  #   cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^5, evidence = evidence_high),
  #   error = function(e) {
  #     message("Error in sim1: ", e$message)
  #     NULL  # Return NULL on error to make it clear where the failure occurred
  #   }
  # )

  # sim2 <- tryCatch(cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^6, evidence = evidence_low), error = function(e) print(e))





  print("made it here")
  # sim2 <- cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^6, evidence =  A < 0.05, debug = TRUE)
  #print("SIM 2")
  #print(sim2)
      #sim1 <- cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^5, evidence = list(StatusDiscretized = "high") )
      #sim2 <- cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^6, evidence = list(StatusDiscretized = "low") )

      if(is.null(sim1) || is.null(sim2)) stop("Error in performing cpdist with given evidence.")

      # Prepare data for plotting
      prepareSimData <- function(sim, node) data.frame(x = sim[[key_feature]], y = sim[[node]])
      
      simData.tumor <- prepareSimData(sim1, cellType)
      simData.normal <- prepareSimData(sim2, cellType)

      # Perform linear regression and normalize data
      normalize <- function(var, condition) {
        (var[condition] - min(var)) / (max(var) - min(var))
      }
    
    xa <- normalize(data[[key_feature]], data[[status]] == 1)
    ya <- normalize(data[[cellType]], data[[status]] == 1)
    xb <- normalize(data[[key_feature]], data[[status]] == 0)
    yb <- normalize(data[[cellType]], data[[status]] == 0)

    Data.tumor <- data.frame(x = xa, y = ya)
    Data.normal <- data.frame(x = xb, y = yb)
    
    # Define the slopes and intercepts for the lines
    cancer_slope <- coef(lm(y ~ x, data = simData.tumor))[2]
    cancer_intercept <- coef(lm(y ~ x, data = simData.tumor))[1]
    normal_slope <- coef(lm(y ~ x, data = simData.normal))[2]
    normal_intercept <- coef(lm(y ~ x, data = simData.normal))[1]
    
    # --------------------------------------------------------NEW fINAL
    # Assuming Data.tumor and Data.normal have the same structure
    # Combine Point Data
    Data.tumor$type <- "Tumor"
    Data.normal$type <- "Normal"
    Data.combined <- rbind(Data.tumor, Data.normal)
    
    # Combine Density Data
    simData.tumor$type <- "Tumor"
    simData.normal$type <- "Normal"
    simData.combined <- rbind(simData.tumor, simData.normal)
    
    # Construct the Plot
    p <- ggplot() +
    geom_density_2d(data = simData.combined, aes(x = x, y = y, color = type)) +
      geom_point(data = Data.combined, aes(x = x, y = y, color = type), alpha = 0.5, shape = 19, size = 2) +
      geom_abline(aes(slope = cancer_slope, intercept = coef(lm(y ~ x, data = simData.tumor))[1]), color = "red", lty = "solid", lwd = 1.05) +
      geom_abline(aes(slope = normal_slope, intercept = coef(lm(y ~ x, data = simData.normal))[1]), color = "blue", lty = "solid", lwd = 1.05) +
      
      scale_color_manual(values = c("Tumor" = brewer.pal(9, "Reds")[6], "Normal" = brewer.pal(9, "Blues")[6]),
                        labels = c(sprintf("Disease Slope = %1.3f", cancer_slope), sprintf("Normal Slope = %1.3f", normal_slope))) +
      
      xlab(paste("Normalized", key_feature)) + ylab(cellType) +
      
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(size = 15),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 15))
    
    # Return the plot
    return(p)
  }

  # --------------------------------------------------------

  # Status <- names(data)[1] # A: " Binary variable column name variable 'Status' with binary 0, 1 value
  # Status <- names(data)== Status

  # key_feature <- names(data)[2] # the column of data that is selected as ""key_feature"" by user
  key_feature <- userSelected_key_feature

  all.features <- colnames(discretized_data)

  cellTypes <- all.features[!all.features %in% c(key_feature, Status)]
  # cellTypes <- all.features[!all.features %in% c("B", "A")]

  plots_list <- vector("list", length(cellTypes))
  names(plots_list) <- cellTypes

  for(cellType in cellTypes) {
    tryCatch({
      # Generate the plot with userSelectedCell
      suppressWarnings({
        # plot <- generatePlot(cellType, fBRCABN, data)
        plot <- generatePlot(Status, key_feature, cellType, fBRCABN, data)
        # plot <- generatePlot(userSelectedCell, cellType, fBRCABN, data)
        
        # Store the generated plot in the list
        plots_list[[cellType]] <- plot
      })
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
    })
  }





  # ----------------------------------  DAG plot 

  # DAG.arcs.strength <- as.data.frame( from=arcs.BRCA[ , 1], to = arcs.BRCA[ , 2] , strength= weight.strength)
  # -----------------------------------
  source("calculate_cor_sign.R")
  CorSign <- calculate_cor_sign(arcs.BRCA, corrcoef)

  HLarcs <- arcs.BRCA[CorSign == "-",]

  # ----------------------------------- 
  # DAG plot
  # DAG.Plot <- strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))

  # ----------------------------------- 
  temp <- augmented_edge_list %>%
    select(-contains("_CorSign"), -contains("_strength"))
  # library(dplyr)
  Max.min.Col <- temp %>% dplyr::select(contains(".strength"))
  Max.min.Col <- as.matrix(Max.min.Col)

  # ----------------------------
  for(i in unique(temp$Edge_No)){  # we use command "unique", because for each "Edge_No", we have two rows (A-->B & B-->A)
    inx<- which(temp$Edge_No == i)
    if (any(!is.na(as.numeric(Max.min.Col[inx, ])))) {
      temp$Min_strength[inx[1]] <- min(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Min_strength[inx[2]] <- min(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Max_strength[inx[1]]<- max(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Max_strength[inx[2]]<- max(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
    } else {
      # If all values are missing, set the min and max strength to NA
      temp$Min_strength[inx[1]] <- NA
      temp$Min_strength[inx[2]] <- NA
      temp$Max_strength[inx[1]]<- NA
      temp$Max_strength[inx[2]]<- NA
    }
  }
  # -------------------------
  # remove the two columns "Min.strength and Min.strength" which is different from "Min_strength and Min_strength""
  temp <- temp %>% select(-c(Min.strength, Max.strength))
  #  add column "Clear_direction" with value 1 if that arc is in the final DAG is in the "possible_seed_arcs_filter"
  clear.direction <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                              apply(as.matrix(possible_seed_arcs_filter[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$clear.direction <- clear.direction

  #  add column "Min.BIC_white.list"
  Min.BIC_white.list <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                                apply(as.matrix(possible.white.list[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$Min.BIC_white.list <- Min.BIC_white.list  # sum(nchar(gsub("[^1]", "", temp$Black_list))) #check how many arc is in the blacklist

  #  add column "Final.DAG.Arcs"
  Final.DAG.Arcs <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                            apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$Final.DAG.Arcs <- Final.DAG.Arcs    # sum(as.numeric(temp$Final.DAG.Arcs), na.rm = T)== nrow(arcs.BRCA)

  #  add column "Min.BIC_clear.direction"
  temp$Min.BIC_clear.direction <- ifelse(temp$clear.direction == "1" &
                                          # (temp$unclear_direction == "" | temp$Not_this_direction == "" | temp$clear.direction == "1") &
                                          temp$Min.BIC_white.list == "1" & 
                                          temp$Final.DAG.Arcs == "1",
                                        "1", "")
  # sum(as.numeric(temp$Min.BIC_clear.direction ), na.rm = T)


  #Min.BIC.unclear: Create a new column that checks if all three columns have a value of "1"
  temp$Min.BIC.unclear.direction <- ifelse( (temp$unclear_direction == "1" | temp$Not_this_direction == "1")  &
                                            # (temp$unclear_direction == "1" | temp$Not_this_direction == "1" | temp$clear.direction == "") &
                                            # temp$clear.direction == "" &
                                            temp$Min.BIC_white.list == "1" &
                                            temp$Final.DAG.Arcs == "1",
                                          "1", "")

  # Create a new column that checks if "Final.DAG.Arcs" and "unclear_direction" columns have a value of "1" and "Min.BIC_white.list" has an empty string ""
  temp$Unclear.direction <- ifelse(temp$Final.DAG.Arcs == "1" & 
                                    # temp$clear.direction == "" & 
                                    temp$Min.BIC_white.list == "" & 
                                    (temp$unclear_direction == "1" | temp$Not_this_direction == "1"),
                                    # temp$unclear_direction == "1" ,
                                    # (temp$unclear_direction == "1" | temp$Not_this_direction == "1" | temp$clear.direction == ""),
                                  "1", "")

  # sum(as.numeric(temp$Unclear.direction ), na.rm = T)

  #  we put NA for any row that have value "1" in column "Not_this_direction" then assign "" value to column "Hit.Count" of the corresponding row
  temp$Hit.Count[temp$Not_this_direction == "1"] <- ""
  temp$Hit.Count[temp$Black_list == "1"] <- ""
  # ---------------------------:   # Add the reverse of each row after the current row

  arcs.BRCA <- as.data.frame(arcs.BRCA)
  # Create an empty data frame with the same column names
  Final.ars_both <- data.frame(from = character(), to = character())
  # Loop through each row of the original data frame and add the reverse as a new row after the current row
  for (i in 1:nrow(arcs.BRCA)) {
    Final.ars_both <- rbind(Final.ars_both, arcs.BRCA[i, ])
    Final.ars_both <- rbind(Final.ars_both, data.frame(from = arcs.BRCA[i, "to"], to = arcs.BRCA[i, "from"]))
  }
  # Reset the row names of the new data frame
  rownames(Final.ars_both) <- NULL
  # --------------------------- 
  #  add column "Excluded"
  # converts the first data frame into a matrix, concatenates the "from" and "to" columns into a single string for each row using paste(x, collapse = "|"), and returns a vector of strings representing the arcs in temp.
  Excluded <- ifelse(
    apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
      # apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
      apply(as.matrix(Final.ars_both[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
    
    "", "1")

  temp$Excluded <- Excluded    # sum(as.numeric(temp$Final.DAG.Arcs), na.rm = T)== nrow(arcs.BRCA)
  # sum(as.numeric(temp$Excluded), na.rm = T)
  # any "Black_list" should be added as "Excluded"
  temp$Excluded[temp$Black_list == "1"] <- "1"
  temp$Excluded[temp$Min.BIC_clear.direction == "1"] <- ""
  temp$Excluded[temp$Min.BIC.unclear.direction == "1"] <- ""
  temp$Excluded[temp$Unclear.direction == "1"] <- ""

  Alg.count.table <- temp %>% select(c(Edge_No, Hit.Count, Min.BIC_clear.direction, Min.BIC.unclear.direction,
                                      Unclear.direction, Excluded, Min_strength, Max_strength ))

  # Move Min_strength and Max_strength to the last columns: everything() selects all remaining columns. Finally, Min_strength and Max_strength are added to the end of the data frame.
  Alg.count.table <- as.data.frame(lapply(Alg.count.table, as.numeric))
  library(dplyr)
  temp2 <- Alg.count.table %>%
    group_by(Edge_No) %>%   # check if there are any non-missing values in the vector using the any function and the is.na function. If there are no non-missing values, return NA instead of calling the max function.
    summarize(Hit.Count = if (any(!is.na(Hit.Count))) max(Hit.Count, na.rm = TRUE) else NA,
              Min.BIC_clear.direction = ifelse(sum(Min.BIC_clear.direction, na.rm = TRUE) >= 1, "1", ""),
              Min.BIC.unclear.direction = ifelse(sum(Min.BIC.unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Unclear.direction = ifelse(sum(Unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Excluded = ifelse(sum(Excluded, na.rm = TRUE) >= 1, "1", ""),
              Min_strength = first(Min_strength),
              Max_strength = first(Max_strength))%>%
    ungroup()

  #   removes rows with NA, empty string, or zero values in the Hit.Count column
  temp2 <- temp2[!(is.na(temp2$Hit.Count) | temp2$Hit.Count == "" | temp2$Hit.Count == 0), ]

  temp2$Excluded[temp2$Min.BIC_clear.direction == "1"] <- ""
  temp2$Excluded[temp2$Min.BIC.unclear.direction == "1"] <- ""
  temp2$Excluded[temp2$Unclear.direction == "1"] <- ""

  rownames(temp2) <- NULL 
  Alg.Count_arcs.strength.table <- temp2

  # Before returning from the function
  print("Function return objects:")
  print(list(
    network = network,
    final_DAG_detail = final_DAG_detail
    # Include other objects you find relevant
  ))

  return(list(Alg.Count_arcs.strength.table = Alg.Count_arcs.strength.table, 
              network = network,
              final_DAG_detail = final_DAG_detail,
              
              arcs.BRCA = arcs.BRCA,
              P_strength = weight.strength,
              arc_slopes.strength= arc_slopes.strength, 
              
              plots_list = plots_list,
              plotFunction = function() {
                strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
              }
              ))

  }
  }
