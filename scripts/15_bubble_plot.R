library(ggplot2)
library(dplyr)

plot_camera_gsea <- function(camera.results, 
                             contrast1, 
                             contrast2, 
                             direction = c("Up", "Down"),
                             Title = "GSEA Plot") {
  
  direction <- match.arg(direction)
  
  # Helper to build the table per contrast
  build_df <- function(contrast, label) {
    df <- camera.results[[contrast]][["H"]] %>%
      mutate(
        pathway = rownames(.),
        neg_log10_fdr = -log10(FDR),
        avg_logFC_clamped = pmin(pmax(avg.logFC, -2), 2),
        contrast = label
      )
    
    df <- df %>% filter(Direction == direction)
    
    df %>% arrange(FDR) %>% head(20)
  }
  
  # Build both contrasts
  plot_data <- bind_rows(
    build_df(contrast1, "- RABV-P"),
    build_df(contrast2, "+ RABV-P")
  )
  
  # Plot
  ggplot(plot_data, aes(x = neg_log10_fdr, 
                        y = reorder(pathway, neg_log10_fdr))) +
    geom_point(aes(size = NGenes, color = avg_logFC_clamped)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red",
                          midpoint = 0, name = "avg logFC") +
    labs(
      x = expression(-log[10]~"FDR"),
      y = "Pathway",
      title = Title
    ) +
    facet_wrap(~contrast, scales = "free_y") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(size = 10, face = "bold")
    )
}
