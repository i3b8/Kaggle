plotLogReturns <- function(data){
  if ("Date" %in% colnames(data)) {
    plot <- data %>% ggplot(aes(x = Date, y = logReturn)) +
      geom_line() +
      labs(title = "Log Returns of Stock Price",
           x = "Date",
           y = "Log Return") +
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "red")
      )
    return(plot)
  }
  else {
    plot <- data %>%
      ggplot(aes(x = seq_along(logReturn), y = logReturn)) +
      geom_line() +
      labs(title = "Log Returns of Stock Price",
           x = "Index",
           y = "Log Return") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "red")
      )
    return(plot)
  }
  
}

plotACF <- function (variable) {
  return(acf(variable,main="ACF",plot=TRUE))
}
plotMyECDF <- function (variable,xlab ="") {
  # Compute the empirical cumulative distribution function (ECDF)
  my_ecdf <- ecdf(variable)
  ecdf_values <- my_ecdf(variable)
  
  # Create a plot with improved labels
  plot(1 - ecdf_values, variable,
       ylab = expression(P[">" ~ x]),
       xlab = xlab)
  
  # Add a title with specific formatting
  title(main = expression(bold("myecdf")), line = 1.5, col.main = "red")
  
}
plotLogECDFLogX <- function (X,X_name="Missing variable name") {
  myecdf <- ecdf(X)
  df <- data.frame(x = X, y =1 - myecdf(X)  )
  ggplot(df, aes(x, y)) +
    geom_step() +
    labs(x = paste0("log(",X_name,")"), y = expression(logP [">"]~ (x))) +
    scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) + # Round to 2 digits
    scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) + # Round to 2 digits
    ggtitle(paste0("Log(P)> x Vs log(",X_name,")")) +  # Set the title
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Centered and bold title
  
} 

plotNAsPerRow <- function (na_count_vector) {
  # Create a data frame for the plot
  data_plot <- data.frame(Index = 1:length(na_count_vector), NA_Count = na_count_vector)
  # Create the ggplot with jittered points
  p <- ggplot(data_plot, aes(x = Index + jitter(NA_Count, factor = 0.3), y = NA_Count)) +
    geom_point(size = 3, alpha = 0.6) +  # Create scatter-like points with jitter
    labs(
      x = "Row Index",
      y = "Missing Values Count"
    )   + 
    ggtitle("Missing Values Count Per Row") +  # Add a centered title
    theme(plot.title = element_text(color = "red", face = "bold", hjust = 0.5))  # Customize title appearance
  return(p)
  
}
  