plot_DE_All_summary <- function(df, timepoint_label, 
                                cols = c(27, 28, 29),
                                custom_labels = NULL) {
  
  library(dplyr)
  library(tidyr)
  
  # Subset selected columns
  sub_df <- df[, cols]
  
  # If custom labels provided, assign them
  if (!is.null(custom_labels)) {
    if (length(custom_labels) != ncol(sub_df)) {
      stop("Length of custom_labels must match number of selected columns")
    }
    colnames(sub_df) <- custom_labels
  }
  
  # Convert to long format
  long_df <- sub_df %>%
    mutate(Gene = rownames(sub_df)) %>%
    pivot_longer(-Gene, names_to = "Comparison", values_to = "status_col") 

    long_df$Comparison <- factor(long_df$Comparison, levels = custom_labels)
  
  # Count -1 and 1 per column
  count_df <- long_df %>%
    filter(status_col %in% c(-1, 1)) %>%
    group_by(Comparison, status_col) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(Regulation = factor(status_col,
                               levels = c(-1, 1),
                               labels = c("Downregulated", "Upregulated")))
  
  # Plot
  p <- ggpubr::ggbarplot(count_df,
                         x = "Comparison",
                         y = "Count",
                         fill = "Regulation",
                         color = "black",
                         label = TRUE,
                         lab.pos = "out",
                         lab.col = "black",
                         palette = c("Upregulated" = "#ff6361",
                                     "Downregulated" = "#ffa600"),
                         position = position_dodge()) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")) +
    labs(title = timepoint_label, x = NULL, y = "")
  
  return(p)
}