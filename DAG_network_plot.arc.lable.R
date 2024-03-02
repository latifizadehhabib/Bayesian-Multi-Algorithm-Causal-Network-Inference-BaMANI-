DAG_network_plot.arc.lable <- function (                                                        
                                                          augmented_edge_list, 
                                                          possible_seed_arcs_filter, 
                                                          data, 
                                                          discretized_data, 
                                                          possible.white.list, 
                                                          Black_List, 
                                                          nboot, cl, 
                                                          corrcoef) {
  

    possible.white.list <- as.data.frame(possible.white.list)
    discretized_data <- as.data.frame(discretized_data)
    Black_List <- as.data.frame(Black_List)
    
    arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, algorithm.args = list(whitelist = possible.white.list, blacklist = Black_List))
    ave.BRCA <- averaged.network(arstr)
    
    arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
    arcs.BRCA <- arcs(ave.BRCA)
    arcs.BRCA <- as.data.frame(arcs.BRCA)
    
    fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)
    
    BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
    weight.strength <- BRCA_str$strength
    
    # ----------------------------------
    # DAG.arcs.strength <- as.data.frame( from=arcs.BRCA[ , 1], to = arcs.BRCA[ , 2] , strength= weight.strength)
    # -----------------------------------
    # source("calculate_cor_sign.R")
    # CorSign <- calculate_cor_sign(arcs.BRCA, corrcoef)
    # 
    # HLarcs <- arcs.BRCA[CorSign == "-",]
    # BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
    # 
    # strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
    # # plot <-  strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
    # # }
    
    
    arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())
    
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
                                weight.strength = weight.strength)
    
    arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)
    
    
    # arcs <- arc_slopes.strength[, c("from", "to")]
    # Rename the column names in arcs.BRCA
    # colnames(arcs) <- c("from_node", "to_node")
    
    # Create the graph using the renamed column names
    g <- graph_from_data_frame(d = arc_slopes.strength[, c("from", "to")], directed = TRUE)
    
    E(g)$weight.strength <- arc_slopes.strength$weight.strength
    E(g)$slope <- signif(arc_slopes.strength$slope, digits = 2)
    
    # Set the layout of the graph
    layout <- layout_with_kk(g)
    
    # Define colors for the edges based on slope
    edge_colors <- ifelse(E(g)$slope < 0, "red", "black")
    
    vertex_shapes <- rep("circle", nrow(arc_slopes))
    
    E(g)$width <- signif(E(g)$weight.strength, digits = 2)
    
    # assign vertex shapes to the graph object
    V(g)$shape <- vertex_shapes
    
    # change the shape of the "Cancer" vertex to "diamond"
    V(g)$shape[V(g)$name == "CCN4"] <- "square"
    V(g)$shape[V(g)$name == "Cancer"] <- "rectangle"
    
    # set the arrowhead mode for the edges based on slope
    # E(g)$arrow.mode <- ifelse(E(g)$slope >= 0, "triangle", "tooth")
    
    # Plot the graph
    plot(g, layout = layout, edge.arrow.size = 0.6, vertex.color = "lightblue",
         # edge.width = E(g)$width, 
         edge.label = signif(E(g)$slope, digits = 2), 
         edge.label.dist = 2, edge.label.color = "black",
         edge.color = edge_colors,
         # vertex.shape = vertex_shapes,
         rescale = T, vertex.size = 15, vertex.label.cex = 0.95,
         width = 12, height = 14)
}