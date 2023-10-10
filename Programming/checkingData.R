# Setting working directory # 

wd <-"/Users/ilyesbenayed/Desktop/Programming"
setwd(wd)
list.files() # You should observe 4 elements: data lib filename.R and results

# Packages #
library(dplyr)
library(crayon)
library(ggplot2)
library(data.table)

# Source files 
source(paste0(wd,"/lib/vectorOperations.R"))
source(paste0(wd,"/lib/makePlot.R"))
source(paste0(wd,"/lib/dataReading.R"))

# Reading Online zipped data # 

myurl='https://www.forexite.com/free_forex_quotes/2011/11/011111.zip'
raw_data <- readUrlZip(url= myurl,filename = "freeForexQuotes",wd)


#Checking equity daily data ## 

dataPath <- paste0(wd,"/data/raw/daily/equity/us-stocks.csv")
raw_data <- fread(dataPath)

## cleaning data ## 
clean_data  <- raw_data[,-1]

# Dimensions 
print(dataDimensions(clean_data)) 
print(computeColumnTypes(clean_data))
extractColumnsByType(clean_data,"integer") # Evelyn Wye Investment K GBP Inc ( Started at 23 but in my data all NA)

# Plotting one column : IBM ticker here 
IBM_vec <- na.omit(as.vector(unlist(clean_data[,"IBM"])))
IBM_df  <- data.frame(price = IBM_vec , logReturn = logReturn(IBM_vec))
plotLogReturns(IBM_df)
plotACF(IBM_df$logReturn)
plotMyECDF(abs(IBM_df$logReturn),"|r|")
plotLogECDFLogX(IBM_df$logReturn,"r")



## Missing values ## 
plotNAsPerRow(NAsPerRow(clean_data)) # Plot the NAs per row 
# Indexes when ^GSPC is null
NA_indices_GSPC <- findNAindices(clean_data[,1])
clean_data <- removeColumnsWithNonNA(clean_data,NA_indices_GSPC)
clean_data <- removeRowsByIndices(clean_data,NA_indices_GSPC)
fwrite(clean_data,file=paste0(wd,"/data/clean/DailyUsStock.csv"))


clean_data <- fread(file=paste0(wd,"/data/clean/DailyUsStock.csv"))
# Apply the logReturn function to each column of clean_data
log_return_df <- lapply(clean_data, logReturn) # Log return data 
# Convert the result back to a data frame and set column names
log_return_df <- as.data.table(log_return_data)
colnames(log_return_df) <- paste("Log_Return_", colnames(log_return_df))


# Call the function to get the counter
result <- countExceedThreshold(log_return_df)
cat("Number of different stocks:", result$unique_stocks,
    "\nTotal exceeded values:",result$total_count, "\n")
## replacing outliers ## 
log_return_df <- ReplaceExceededThreashold(log_return_df)
dataDimensions(log_return_df)
dataDimensions(clean_data)
View(log_return_df)
colnames(log_return_df)
class(log_return_df)
