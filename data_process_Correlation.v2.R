data_process_Correlation.v2 <- function(data) {
  # load data
  # library(bnlearn)
  
  # data <- read.csv(paste(data_address, data_name, ".csv", sep = ""), head = TRUE)
  # data <- as.data.frame(data)
  
  # remove first column (row column)
  # data <- data[, -1]
  
  # convert all columns to numeric
  data[] <- lapply(data, as.numeric)
  
  # ----------------------------
  # convert_to_numeric <- function(col) {
  #   as_numeric <- suppressWarnings(as.numeric(col))
  #   if (anyNA(as_numeric)) return(col)  # Return original if conversion fails
  #   return(as_numeric)
  # }
  # data[] <- lapply(data, convert_to_numeric)
  # ----------------------------
  
  
  # data[, sapply(data, is.numeric) == FALSE] <- lapply(data[, sapply(data, is.numeric) == FALSE], as.numeric)
  
  class(data)
  data <- as.data.frame(data)
  
  # discretize the data: Use interval to get seed network and interval to refine the structure and infer parameters
  tmp_discrete <- discretize(data, method = "interval", breaks = c(2,rep(15,16)))
  
  # Still is in interval format, but the scale is the same for all the nodes
  ddata <- as.data.frame(lapply(tmp_discrete, function(x) (as.numeric(x) - 1)/max(as.numeric(x) -1)))

  cat("************************************************************************", "\n")
  cat(sprintf("Data characteristics"), "\n")
  cat("************************************************************************", "\n")
  str(ddata)
  cat("************************************************************************", "\n")
  
  # Convert any non-numeric variables to numeric
  ddata[, sapply(ddata, is.numeric) == FALSE] <- lapply(ddata[, sapply(ddata, is.numeric) == FALSE], as.numeric)
  # remove incomplete cases
  ddata <- ddata[complete.cases(ddata),]
  
  # calculate correlation matrix
  corrcoef <- cor(sapply(ddata, as.numeric))
  colnames(corrcoef) <- colnames(ddata)
  rownames(corrcoef) <- colnames(ddata)
  
  return(list(corrcoef = corrcoef, discretized_data= ddata, data = data))
  }
