library(GGally)
library(ggplotify)
create_parallel_plot <- function(dataframe, title, custom_colors, direction = 1) {
  
  # Filter based on direction (1 = induced, -1 = suppressed)
  filtered_data <- dataframe[dataframe[, 11] == direction &
                               dataframe$Classification != "Background", ]
  
  # Rename columns for better display
  colnames(filtered_data)[5] <- "FLAG + IFN"
  colnames(filtered_data)[6] <- "IFN + P1"
  
  # Add a baseline control column
  filtered_data$Control <- 0
  
  # Build parallel coordinate plot
  ggparcoord(data = filtered_data,
             columns = c(ncol(filtered_data), 5, 6),  # Control, FLAG+IFN, IFN+P1
             groupColumn = "Classification",
             alphaLines = 1,
             scale = "globalminmax",
             showPoints = TRUE) +
    geom_point(aes(text = SYMBOL), size = 3) +
    theme_bw() +
    labs(title = title, x = "", y = "Log2Fold Change") +
    scale_color_manual(values = custom_colors) +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 8, face = "bold"),
      axis.title = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.position = "none",
      aspect.ratio = 0.9
    )
}


