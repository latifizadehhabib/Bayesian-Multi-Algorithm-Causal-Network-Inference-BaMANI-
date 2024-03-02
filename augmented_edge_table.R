augmented_edge_table <- function(Black_List,
                                 all_edge_list, 
                                 algorithm_directed, 
                                 Arcs.Cor_streng_table.alg,
                                 algorithm_undirected,
                                 Arcs.Cor_table.alg) {
  
  # Load the dplyr package
  # install.packages("rlang")
  # install.packages("dplyr")
  library("dplyr")
  
  tmp <- all_edge_list


for(algorithm in algorithm_undirected){
  
  Arcs.Cor <- as.data.frame(Arcs.Cor_table.alg[[algorithm]])
  names(Arcs.Cor) <- c("from", "to", paste(algorithm, "_CorSign", sep = ' '))
  tmp <- merge(tmp, Arcs.Cor, by = c("from", "to"), all = TRUE)
  
  # load(paste(file, "Bootstrap", nboot, "_", algorithm, ".rda", sep = ' '))
  # names(Arcs.Cor_table) <- c("from", "to", paste(algorithm, "_CorSign", sep = ' '))
  # tmp <- merge(tmp, Arcs.Cor_table, by = c("from", "to"), all = TRUE)
}

#----
# Remove duplicate rows based on "from" and "to" columns (For example, we have three rows CD4-->CD8)
# distinct() function from the dplyr package in the tidyverse library to remove the duplicate rows. 
# The .keep_all argument is set to TRUE so that all other columns in the original dataset are kept.
#----

tmp <- tmp %>% distinct(from, to, .keep_all = TRUE) 


for(algorithm in algorithm_directed){
  
  Arcs.Cor_streng <- as.data.frame(Arcs.Cor_streng_table.alg[[algorithm]])
  names(Arcs.Cor_streng) <- c("from", "to", paste(algorithm, "_strength", sep = ' '), paste(algorithm, "_CorSign", sep = ' '))
  tmp <- merge(tmp, Arcs.Cor_streng, by = c("from", "to"), all = TRUE)
  
  # load(paste(file, "Bootstrap", nboot, "_", algorithm, ".rda", sep = ' '))
  # names(Cor_streng_table) <- c("from", "to", paste(algorithm, "_strength", sep = ' '), paste(algorithm, "_CorSign", sep = ' '))
  # tmp <- merge(tmp, Cor_streng_table, by = c("from", "to"), all = TRUE)
}

# Remove duplicate rows based on "from" and "to" columns
tmp <- tmp %>% distinct(from, to, .keep_all = TRUE) # distinct() function from the dplyr package in the tidyverse library to remove the duplicate rows. The .keep_all argument is set to TRUE so that all other columns in the original dataset are kept.

tmp[mapply(is.na, tmp)] <- ""  #tmp[is.na(tmp)] <- 0   # remove any "NA"
# tmp$Edge_No <- as.numeric(tmp$Edge_No)

tmp$Edge_No <- as.numeric(tmp$Edge_No)  #class(tmp$Edge_No)
tmp <- tmp[order(tmp$Edge_No, decreasing = F), ]

augmented_edge_list <- tmp

#auxiliary data
# tmp1 <- tmp
# tmp2 <- tmp

#----
# save(augmented_edge_list, file = paste(file, "_Seed_Summary_Int_", nboot, ".rda", sep = ' '))
# write.csv(augmented_edge_list, paste(file, "_Seed_Summary_Int_", nboot, ".csv", sep = ''), row.names=FALSE)
#----


#---------------------------------------------------------------------------------------
# adding column "Hit.count" 
arcs_CorSign <- tmp %>% select(contains("_CorSign"))

# apply() function to count the number of "+" and "-" signs in each row # add hit_count as a new column in the original data frame
augmented_edge_list$Hit.Count <- apply(arcs_CorSign, 1, function(x) sum(x == "+" | x == "-"))

# ***************important for the next section*****************
# augmented_edge_list <- subset(augmented_edge_list, Hit.Count != "0")
rownames(augmented_edge_list) <- NULL

#---------------------------------------------------------------------------------------
# Add a Blacklist (Red) column to Check if the row (arc) is in Blacklist # The ifelse() function checks if each row in the from and to columns of tmp is in the set of rows in the from and to columns of Black_List, using apply() and paste() to concatenate the values in each row into a single string. If the row is in Black_List, the function returns "1" for that row, otherwise it returns an empty string. The resulting Black_Sign variable contains "1" for rows in tmp that are also in Black_List, and an empty string for all other rows.

Black_Sign <- ifelse(apply(tmp %>% select(from, to), 1, function(x) paste(x, collapse = "|")) %in% 
                       apply(Black_List %>% select(from, to), 1, function(x) paste(x, collapse = "|")), "1", "")

augmented_edge_list$Black_list <- Black_Sign  # sum(nchar(gsub("[^1]", "", augmented_edge_list$Black_list))) #check how many arc is in the blacklist

# -------------------------------------Not_this_direction (A-->B not been learned by any algorithm, zero or empty value for this row )
# Any row that has all zero or empty value in the arc strength columns (which means that specific directed arc ( or row) has not been recognized by any directed algorithm) and we assign a column "Not this drection" with value 1 to it. 

# ----------------------------new
# # Select columns containing "_strength"
# strength_cols <- grep("_strength", names(tmp1), value = TRUE)
# 
# # Check for rows with all zero or all NA values in the selected columns
# augmented_edge_list$Not_this_direction <- ifelse(rowSums(tmp1[strength_cols] == 0, na.rm = TRUE) | rowSums(is.na(df[strength_cols])) == length(strength_cols), 1, "")

# ----------------------------old version
# source("not_this_direction_arcs.R")
# # debug(not_this_direction_arcs)
# table.not_this_direction <- not_this_direction_arcs(augmented_edge_list)
# augmented_edge_list <- table.not_this_direction

# dim(Not_this_direction_rows) #3*22 Because "2.22824728286868e-127" is very samll close to zero, machine count as zero


#---------------------------------------------------------------------------------------
# "Unclear_direction": When Row A->B and Row B->A have some arc strength columns with non-zero (even for zero) value at the same time (which means some algorithm learnt A->B and others B->A, so it's not safe to choose one of them by chance (unclear))

source("find_unclear_direction.R")  # if add two columns "Not_this_direction" and "unclear_direction"
# debug(find_unclear_direction)
augmented_edge_list <- find_unclear_direction (augmented_edge_list)


#---------------------------------------------------------------------------------------
# Select columns with "_strength" in the column name
# Load the matrixStats package

strength_cols_names <- grep("_strength", names(tmp), value = TRUE)
strength_cols <- tmp[, strength_cols_names]

# Convert the data frame to a numeric data type
strength_cols_numeric <- apply(strength_cols, 2, as.numeric)

min_vals <- rowMins(as.matrix(strength_cols_numeric), na.rm = TRUE)
max_vals <- rowMaxs(as.matrix(strength_cols_numeric), na.rm = TRUE)

# Replace Inf and -Inf with empty
min_vals[min_vals == Inf | min_vals == -Inf] <- ""
max_vals[max_vals == Inf | max_vals == -Inf] <- ""

# Add the max values as a new column in the data frame
augmented_edge_list$Min.strength <- min_vals
augmented_edge_list$Max.strength <- max_vals

#-------------------
# Find the minimum and maximum values for each row
# strength_min <- apply(strength_cols, 1, min, na.rm = TRUE)
# strength_max <- apply(strength_cols, 1, max, na.rm = TRUE)

# # Find the minimum and maximum values for each row
# strength_min <- apply(strength_cols_numeric, 1, min, na.rm = TRUE)
# strength_max <- apply(strength_cols_numeric, 1, max, na.rm = TRUE)

# Add the minimum and maximum values as new columns to the original data frame
# augmented_edge_list$Min_strength <- strength_min
# augmented_edge_list$Max_strength <- strength_max
#-------------------

# ---------------------------------------------------------------------------------------
# save(augmented_edge_list, file = paste(file, "_Seed_Summary_Int_", nboot, ".rda", sep = ' '))
# write.csv(augmented_edge_list, paste(file, "_Seed_Summary_Int_", nboot, ".csv", sep = ''), row.names=FALSE)

return(augmented_edge_list)

}

