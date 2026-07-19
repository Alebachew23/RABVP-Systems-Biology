library(ggpubr)
library(cowplot)
# Bar plot function to visualize the number of Differentially expressed genes following OSM treatment 
# Define a function to generate the barplot from a given data frame
plot_DE_summary <- function(df, timepoint_label) {
  # Extract 11th column (decideTests output)
  status_col <- df[[8]]
  
  # Count 1s and -1s
  count_df <- as.data.frame(table(status_col)) %>%
    dplyr::filter(status_col %in% c("-1", "1")) %>%
    mutate(status_col = factor(status_col, levels = c("-1", "1"),
                               labels = c("Downregulated", "Upregulated")))
  
  colnames(count_df) <- c("Regulation", "Count")
  
  # Create bar plot
  p <- ggpubr::ggbarplot(count_df,
                         x = "Regulation",
                         y = "Count",
                         fill = "Regulation",
                         color = "black",
                         label = TRUE,
                         lab.pos = "out",
                         lab.col = "black",
                         orientation = "vertical",
                         palette = c("Upregulated" = "#ff6361", "Downregulated" = "#ffa600")) +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.7, face = "bold")) +
    labs(title = timepoint_label, x = NULL, y = NULL)
  
  return(p)
}