library(ggrepel)
library(patchwork)


create_pie_chart <- function(dataframe, title, custom_colors, direction = 1) {
  
  # Filter rows dynamically using the user-supplied osm value
  filtered_data <- dataframe[
    dataframe[, 11] == direction & dataframe$Classification != "Background",
  ]
  
  # Count classifications
  classification_counts <- as.data.frame(table(filtered_data$Classification))
  colnames(classification_counts) <- c("Classification", "Count")
  
  # Calculate proportions and label positions
  classification_counts$Proportion <- round((classification_counts$Count / sum(classification_counts$Count)) * 100, 1)
  
  classification_counts <- classification_counts %>%
    mutate(
      csum = rev(cumsum(rev(Count))),
      pos  = Count / 2 + lead(csum, 1),
      pos  = if_else(is.na(pos), Count / 2, pos)
    )
  
  # Create pie chart
  pie_chart <- ggplot(classification_counts, aes(x = "", y = Count, fill = Classification)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.position = "right",
      plot.margin = margin(2, 2, 2, 2)
    ) +
    labs(title = title, fill = "Classification") +
    scale_fill_manual(values = custom_colors) +
    geom_label_repel(
      data = classification_counts,
      aes(y = pos, label = paste0(Proportion, "%")),
      size = 4.5,
      nudge_x = 1,
      show.legend = FALSE
    )
  
  return(pie_chart)
}

# Define custom colors for classifications
custom_colors <- c("Unaffected" = "grey", "Antagonized" = "#de425b", "Enhanced" = "#488f31")
