# Computing the log return # 

logReturn <- function(price){
  log_price_diff <- c(0, diff(log(price)))
  log_price_diff[is.na(log_price_diff)] <- 0  # Replace NA values with 0
  return(log_price_diff)
}
findNAindices <- function(input_vector) {
  na_indices <- which(is.na(input_vector))
  return(na_indices)
}
