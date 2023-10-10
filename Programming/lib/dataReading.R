readUrlZip <- function(url,filename,w_directory){
  file_1 <- download.file(url = url,destfile=paste0(w_directory,"/data/raw/downloads/",filename)) 
  file_1 <- unzip(paste0(w_directory,"/data/raw/downloads/",filename))
  return(fread(file_1))
}

dataDimensions  <- function (data) {
  return (c("nrows"=dim(data)[1],"ncols"=dim(data)[2]) ) 
}

computeColumnTypes <- function(df) {
  # Initialize an empty list to store the counts of different types
  type_counts <- list()
  # Iterate through the columns of the data frame
  for (col_name in colnames(df)) {
    # Get the class (data type) of the column
    col_class <- class(df[[col_name]])
    
    # If the class is not in the type_counts list, add it with a count of 1
    if (!col_class %in% names(type_counts)) {
      type_counts[[col_class]] <- 1
    } else {
      # If the class is already in the list, increment its count by 1
      type_counts[[col_class]] <- type_counts[[col_class]] + 1
    }
  }
  # Convert the list of counts into a named vector
  type_vector <- unlist(type_counts)
  names(type_vector) <- names(type_counts)
  return(type_vector)
}

# Function to extract columns by data type for data.table
extractColumnsByType <- function(data_table, target_type) {
  # Get the names of columns with the specified data type
  target_columns <- sapply(data_table, function(x) class(x) == target_type)
  
  # Extract the columns with the specified data type
  result <- data_table[, .SD, .SDcols = target_columns]
  
  return(result)
}

#Function to compute the number of NAs per row in a data 
NAsPerRow <- function (data_table) {
  return(rowSums(is.na(data_table)))
}

removeColumnsWithNonNA <- function(data_table, row_indices) {
  columns_to_keep <- rep(TRUE, ncol(data_table))
  
  # Loop through the specified row indices
  for (row_index in row_indices) {
    # Check if the row has any non-NA values
    row_has_non_na <- !is.na(data_table[row_index, ])
    
    # Update the columns_to_keep vector
    columns_to_keep <- columns_to_keep & !row_has_non_na
  }
  
# Remove columns where columns_to_keep is FALSE
  filtered_data <- data_table[, columns_to_keep, with = FALSE]
  
  return(filtered_data)
}

library(data.table)

removeRowsByIndices <- function(data_table, indices_to_remove) {
  # Use negative indexing to exclude the specified indices
  filtered_data_table <- data_table[-indices_to_remove]
  return(filtered_data_table)
}

# Create a function to count observations exceeding the threshold
countExceedThreshold <- function(data, threshold = 0.15) {
  # Initialize an empty counter list
  counter <- list()
  # Initialize counters
  total_count <- 0
  unique_stocks <- 0
  # Iterate over columns (using column names as tickers)
  for (ticker in colnames(data)) {
    # Count the number of observations exceeding the threshold
    count <- sum(abs(data[[ticker]]) > threshold)
    
    # Add the result to the counter list only if count is greater than zero
    if (count > 0) {
      counter[[ticker]] <- count
      total_count <- total_count + count
      unique_stocks <- unique_stocks + 1
    }}
  
  return(list(counter = counter, unique_stocks = unique_stocks, total_count = total_count))
}

# Define a function to replace values exceeding the threshold
ReplaceExceededThreashold <- function(data, threshold=0.15) {
  # Use apply to process each column (stock returns)
  data <- apply(data,1, function(column) {
    # Use ifelse to replace values exceeding the threshold
    ifelse(abs(column) > threshold, sign(column) * threshold, column)
  })
  data <- t(data) 
  # Reset row names in the transposed data frame (if needed)
  rownames(data) <- NULL
  # Return should be of type data table 
  
  return(as.data.table(data))
}



