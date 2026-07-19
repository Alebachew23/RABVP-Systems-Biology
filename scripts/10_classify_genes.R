
# classify_genes
# Function to classify genes based on conditions
classify_genes <- function(dataframe) {
  dataframe$Classification <- "Background"  # Initialize all rows with "Background"
  
  # OSM-Induced Genes (dataframe[, 11] == 1)
  dataframe$Classification[dataframe[, 11] == 1 & dataframe[, 12] == 1 & abs(dataframe[, 6] - dataframe[, 5]) < 0.5] <- "Unaffected"
  dataframe$Classification[dataframe[, 11] == 1 & dataframe[, 12] == 1 & (dataframe[, 5] - dataframe[, 6]) > 0.5] <- "Antagonized"
  dataframe$Classification[dataframe[, 11] == 1 & dataframe[, 12] == 0] <- "Antagonized"
  dataframe$Classification[dataframe[, 11] == 1 & dataframe[, 12] == 1 & (dataframe[, 6] - dataframe[, 5]) > 0.5] <- "Enhanced"
  
  # OSM-Suppressed Genes (dataframe[, 11] == -1)
  dataframe$Classification[dataframe[, 11] == -1 & dataframe[, 12] == -1 & abs(dataframe[, 6] - dataframe[, 5]) < 0.5] <- "Unaffected"
  dataframe$Classification[dataframe[, 11] == -1 & dataframe[, 12] == -1 & (dataframe[, 6] - dataframe[, 5]) > 0.5] <- "Antagonized"
  dataframe$Classification[dataframe[, 11] == -1 & dataframe[, 12] == 0] <- "Antagonized"
  dataframe$Classification[dataframe[, 11] == -1 & dataframe[, 12] == 1] <- "Antagonized"
  dataframe$Classification[dataframe[, 11] == -1 & dataframe[, 12] == -1 & (dataframe[, 5] - dataframe[, 6]) > 0.5] <- "Enhanced"
  
  return(dataframe)
}
