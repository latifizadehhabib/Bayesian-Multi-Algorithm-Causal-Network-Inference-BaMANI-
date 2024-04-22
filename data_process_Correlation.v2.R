data_process_Correlation.v2 <- function(data) {
  
  # remove first column (row column)
  # data <- data[, -1]
  
  # convert all columns to numeric
  data[] <- lapply(data, as.numeric)

  # data[, sapply(data, is.numeric) == FALSE] <- lapply(data[, sapply(data, is.numeric) == FALSE], as.numeric)
  
  print(class(data))
  
  
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      numeric_col <- suppressWarnings(as.numeric(col))
      if (!any(is.na(numeric_col))) return(numeric_col)
    }
    return(col)
  })
  
  data <- as.data.frame(data)
  
  # --------
  
  # data <- as.data.frame(data)
  
  # Identify constant columns and store them separately
  constant_columns <- sapply(data, function(x) length(unique(x)) == 1)
  constant_data <- data[, constant_columns, drop = FALSE]
  
  # Exclude constant columns from data to be discretized
  data_to_discretize <- data[, !constant_columns, drop = FALSE]
  
  # Calculate breaks for discretization
  num_categories <- sapply(data_to_discretize, function(x) if (is.factor(x)) length(unique(x)) else NA)
  breaks <- ifelse(is.na(num_categories), 15, pmax(2, num_categories))
  adjusted_breaks <- mapply(pmin, breaks, sapply(data_to_discretize, function(x) length(unique(x))))
  
  # Discretize data
  discretized_data <- discretize(data_to_discretize, method = "interval", breaks = adjusted_breaks)
  
  # view(discretized_data)
  
  # Reinsert constant columns back into their original positions
  for (col_name in names(constant_data)) {
    discretized_data <- cbind(discretized_data, constant_data[col_name])
    # To maintain original order
    discretized_data <- discretized_data[, names(data)]
  }
  
  # discretized_data now contains discretized dataset, including constant columns in their original locations
  # view(discretized_data)
  
  # -----------------------------
  
  # Identify constant columns and store them separately
  constant_columns.cor <- sapply(discretized_data, function(x) length(unique(x)) == 1)
  constant_data.cor <- discretized_data[, constant_columns.cor, drop = FALSE]
  
  # Exclude constant columns from data to be discretized
  discretized_data_no_constant <- discretized_data[, !constant_columns.cor, drop = FALSE]
  
  
  ddata <- as.data.frame(lapply(discretized_data_no_constant, function(x) (as.numeric(x) - 1)/max(as.numeric(x) -1)))
  
  # view(ddata)
  
  # Reinsert constant columns back into their original positions
  for (col_name in names(constant_data.cor)) {
    ddata <- cbind(ddata, constant_data.cor[col_name])
    # To maintain original order
    ddata <- ddata[, names(data)]
  }
  
  # discretized_data now contains discretized dataset, including constant columns in their original locations
  # view(ddata)
  
  
  cat("*************", "\n")
  cat(sprintf("Data characteristics"), "\n")
  cat("*************", "\n")
  str(ddata)
  cat("*************", "\n")
  
  # ------------------
  # Assume 'data' is dataframe
  # First, identify constant columns
  constant_columns <- sapply(ddata, function(x) length(unique(na.omit(x))) == 1)
  
  # Compute correlation matrix for non-constant columns
  non_constant_data <- ddata[, !constant_columns]
  corr_matrix_non_constant <- cor(non_constant_data, use = "complete.obs")
  
  # Create a full-sized correlation matrix initialized with zeros
  corrcoef <- matrix(0, nrow = ncol(ddata), ncol = ncol(ddata),
                     dimnames = list(colnames(ddata), colnames(ddata)))
  
  # Copy correlation coefficients from non-constant correlation matrix to full matrix
  non_constant_colnames <- colnames(non_constant_data)
  corrcoef[non_constant_colnames, non_constant_colnames] <- corr_matrix_non_constant
  
  # Now, corrcoef is correlation matrix with zeros for constant columns
  # Optionally, set diagonal for constant columns to 1
  diag(corrcoef) <- 1
  
  # corrcoef <- as.data.frame(corrcoef)
  # class(corrcoef)
  
  #  correlation matrix
  # View(corrcoef)
  str(corrcoef)
  print(class(corrcoef))
  print(corrcoef)
  
  
  return(list(corrcoef = corrcoef, discretized_data= ddata, data = data))
  }
