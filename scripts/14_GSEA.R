# Required packages
library(ggplot2)
library(dplyr)
library(patchwork)   # for combining plots

#' Generate GSEA comparison plots for contrast pairs
#'
#' @param camera.results named list of camera results: camera.results[[contrast]][[gene_set]]
#' @param contrast_pairs named list. Each element is a character vector of length 2:
#'        e.g. list("1 Hour" = c("FLAG_1hr.v.FLAG_0hr", "P1_1hr.v.FLAG_0hr"), ...)
#'        The function labels the first element "- RABV-P" and second "+ RABV-P".
#' @param gene_set character, which gene set within each camera.results[[contrast]] to use (default "H")
#' @param top_n integer, number of top pathways to keep per contrast (default 20)
#' @param clamp numeric, absolute value to clamp avg.logFC to for coloring (default 2)
#' @param save logical, whether to save the assembled plot (default FALSE)
#' @param out_file character, output filename (PNG/PDF) when save = TRUE
#' @param width,height,dpi numeric graphics parameters passed to ggsave when save = TRUE
#' @return a patchwork object (stacked plots)
generate_gsea_plots <- function(camera.results,
                                contrast_pairs,
                                gene_set = "H",
                                top_n = 20L,
                                clamp = 2,
                                save = FALSE,
                                out_file = "GSEA.png",
                                width = 14,
                                height = 12,
                                dpi = 300,
                                unit = "in") {
  # helper to safely extract and format camera dataframe for a single contrast
  get_camera_df <- function(contrast_name) {
    if (is.null(camera.results[[contrast_name]])) {
      warning(sprintf("contrast '%s' not found in camera.results", contrast_name))
      return(NULL)
    }
    if (is.null(camera.results[[contrast_name]][[gene_set]])) {
      warning(sprintf("gene set '%s' not found for contrast '%s'", gene_set, contrast_name))
      return(NULL)
    }
    df <- as.data.frame(camera.results[[contrast_name]][[gene_set]])
    if (nrow(df) == 0) return(NULL)
    # Ensure required columns exist
    required <- c("FDR", "avg.logFC", "NGenes")
    missing_cols <- setdiff(required, colnames(df))
    if (length(missing_cols) > 0) {
      warning(sprintf("contrast '%s' is missing columns: %s", contrast_name, paste(missing_cols, collapse = ", ")))
      return(NULL)
    }
    df <- df %>%
      mutate(
        pathway = rownames(df),
        neg_log10_fdr = -log10(pmax(FDR, .Machine$double.eps)), # avoid -log10(0)
        avg_logFC_clamped = pmin(pmax(avg.logFC, -clamp), clamp),
        NGenes = as.numeric(NGenes)
      ) %>%
      dplyr::select(pathway, neg_log10_fdr, avg.logFC_clamped, NGenes, everything())
    return(df)
  }
  
  make_pair_plot <- function(c1, c2, title_text) {
    df1 <- get_camera_df(c1)
    df2 <- get_camera_df(c2)
    
    if (is.null(df1) && is.null(df2)) {
      # Return an informative blank plot
      p_blank <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste0("No data for pair:\n", title_text), size = 5) +
        theme_void()
      return(p_blank)
    }
    
    if (!is.null(df1)) df1 <- df1 %>% arrange(FDR) %>% head(top_n) %>% mutate(contrast = "- RABV-P")
    if (!is.null(df2)) df2 <- df2 %>% arrange(FDR) %>% head(top_n) %>% mutate(contrast = "+ RABV-P")
    
    combined <- bind_rows(df1, df2)
    
    # reorder pathways within each contrast by neg_log10_fdr for plotting
    # To keep per-facet ordering, we'll create a combined factor where levels are set per contrast.
    # Using reorder_within pattern: here we use interaction(contrast, pathway) trick for plotting,
    combined <- combined %>%
      group_by(contrast) %>%
      mutate(pathway_ordered = reorder(pathway, neg_log10_fdr)) %>%
      ungroup()
    
    p <- ggplot(combined, aes(x = neg_log10_fdr, y = pathway_ordered)) +
      geom_point(aes(size = NGenes, color = avg_logFC_clamped)) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "avg logFC") +
      labs(x = expression(-log[10]~"FDR"), y = "Pathway", title = title_text) +
      facet_wrap(~contrast, scales = "free_y") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10, face = "bold")
      )
    
    return(p)
  }
  
  # Build list of plots
  plot_list <- lapply(names(contrast_pairs), function(title) {
    pair <- contrast_pairs[[title]]
    if (length(pair) < 2) {
      warning(sprintf("contrast pair '%s' does not contain 2 elements; skipping", title))
      return(ggplot() + theme_void() + ggtitle(title))
    }
    make_pair_plot(pair[1], pair[2], title)
  })
  
  # Combine with patchwork: stack vertically
  combined_plot <- wrap_plots(plot_list, ncol = 1)
  
  # Optionally save
  if (isTRUE(save)) {
    ggplot2::ggsave(filename = out_file, plot = combined_plot, width = width, height = height, dpi = dpi, units = unit)
    message("Saved GSEA plot to: ", out_file)
  }
  
  return(combined_plot)
}