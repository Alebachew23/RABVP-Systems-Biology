library(pheatmap)

# heatmap
heat.map.of.interest <- function(data, chosen, subset = TRUE,
                                 scale = "none", cluster_cols = FALSE, scale.max = NULL, labels_row = myDGElist$genes$SYMBOL, pdf_file = NULL,fontsize = 40, fontsize_row = 40, fontsize_col = 40, ...) {
  
  # Subset the data matrix
  data %<>% `[`(chosen, subset)
  
  # Set the color palette for the heatmap
  colours <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)
  
  # Adjust the color breaks if scale.max is provided 
  if (!is.null(scale.max)) {
    breaks <- seq(-scale.max, scale.max, length.out = 101)
    colours <- colorRampPalette(rev(brewer.pal(7, "RdBu")))(100)
  } else {
    breaks <- NA
  }
  
  # Create the annotation data frame
  annotation_col <- data.frame(
    Protein = myDGElist$samples$Protein[subset],
    IFN_Treatment = myDGElist$samples$Time[subset], 
    row.names = colnames(data)
  )
  
  # Set the desired order for the IFN_Treatment factor levels
  desired_order <- c("0hr", "1hr", "6hr", "24hr")  # replace with actual desired order
  annotation_col$IFN_Treatment <- factor(annotation_col$IFN_Treatment, levels = desired_order)
  
  # Define the annotation colors
  annotation_colors <- list(
    Protein = Protein.cols,
    IFN_Treatment = IFN.Cols[desired_order]
  )
  
  
  # Create the heatmap using pheatmap
  data %>%
    pheatmap(
      cluster_rows = TRUE,
      cluster_cols = cluster_cols,
      show_colnames = FALSE,
      annotation_col = annotation_col,
      annotation_colors = annotation_colors,
      border_color = NA,
      breaks = breaks,
      color = colours,
      labels_row = labels_row[chosen],
      scale = scale,
      fontsize = fontsize,           # Adjust overall font size (affects legend)
      fontsize_row = fontsize_row,   # Adjust row label font size
      fontsize_col = fontsize_col,  
      ...
    )
  
}
