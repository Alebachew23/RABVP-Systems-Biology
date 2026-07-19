# ===== Create and Export Differentially Expressed Genes =====

# Define the style for rounding
round.style <- createStyle(numFmt = "0.000")

# Define the columns to extract from topTreat
tt.cols <- c("logFC", "P.Value", "adj.P.Val")

# Generate a list of topTreat results for each contrast
tt.list <- lapply(colnames(cont.matrix), function(coef) {
  topTreat(fit2, coef, genelist = NULL, number = Inf, sort.by = "none") %>% 
    `[`(, tt.cols)
}) %>%
  set_names(colnames(cont.matrix))

# Define the types and their corresponding contrasts
types <- list(
  P1_Expression = contrast.type$Basal_P1,
  IFN_Regulation = contrast.type$IFN_Regulation,
  Modulation = contrast.type$P1_IFN,
  All_comparisons = contrast.type$v,
  One_hour = contrast.type$Comparison_1hr,
  Six_hours = contrast.type$Comparison_6hr,
  TwentyFour_hours = contrast.type$Comparison_24hr
)

# Initialize workbook
wb <- createWorkbook()

# Create a named list to store dataframes for each type
dataframes_list <- list()

# Iterate over each type: generate dataframe and export to Excel
for (type.name in names(types)) {
  ct <- types[[type.name]]
  
  # Build merged dataframe
  tt.merge <- do.call(what = "cbind", tt.list[ct]) %>%
    `[`(, order(rep(1:length(tt.cols), length.out = ncol(.))))
  res <- results[, ct] %>%
    set_colnames(paste(colnames(.), "Results", sep = "."))
  tt.merge <- cbind(fit2$genes, AveExpr = fit2$Amean, tt.merge, res)
  
  # *** CLASSIFY GENES ***
  tt.merge <- classify_genes(tt.merge)
  
  # Store the dataframe in the list
  dataframes_list[[type.name]] <- tt.merge
  
  # ===== Export to Excel =====
  addWorksheet(wb, type.name)
  writeData(wb, type.name, tt.merge, colNames = TRUE)
  
  # Color-code Classification column
  class_col <- which(colnames(tt.merge) == "Classification")
  class_colors <- list(
    Background = createStyle(bgFill = "#CCCCCC"),      # Gray
    Antagonized = createStyle(bgFill = "#FF6B6B"),     # Red
    Enhanced = createStyle(bgFill = "#51CF66"),        # Green
    Unaffected = createStyle(bgFill = "#FFD43B")       # Yellow
  )
  
  for (row in 2:(nrow(tt.merge) + 1)) {
    class_label <- tt.merge[row - 1, class_col]
    if (class_label %in% names(class_colors)) {
      addStyle(wb, type.name, style = class_colors[[class_label]], 
               rows = row, cols = class_col)
    }
  }
  
  # Bold headers
  addStyle(wb, type.name, createStyle(textDecoration = "bold"), 
           rows = 1, cols = 1:ncol(tt.merge))
  
  # Conditional formatting: AveExpr (color scale)
  addStyle(wb, type.name, style = round.style, 
           cols = grep("AveExpr", colnames(tt.merge)), 
           rows = 1 + 1:nrow(tt.merge), stack = TRUE)
  conditionalFormatting(wb, type.name, 
                        cols = grep("AveExpr", colnames(tt.merge)), 
                        rows = 1 + 1:nrow(tt.merge),
                        style = rev(RColorBrewer::brewer.pal(3, "RdYlBu")), 
                        type = "colourScale")
  
  # Conditional formatting: logFC columns
  max.abs <- 3
  for (i in grep("logFC", colnames(tt.merge))) {
    conditionalFormatting(wb, type.name, cols = i, rows = 1 + 1:nrow(tt.merge),
                          style = rev(RColorBrewer::brewer.pal(3, "RdBu")),
                          rule = max.abs * (-1:1),
                          type = "colourScale")
    addStyle(wb, type.name, style = round.style, cols = i, 
             rows = 1 + 1:nrow(tt.merge), stack = TRUE)
  }
  
  # Conditional formatting: Results columns
  conditionalFormatting(wb, type.name, 
                        cols = grep("Results", colnames(tt.merge)), 
                        rows = 1 + 1:nrow(tt.merge), 
                        rule = -1:1,
                        style = rev(RColorBrewer::brewer.pal(3, "RdBu")), 
                        type = "colourScale")
  
  # Conditional formatting: P-values
  conditionalFormatting(wb, type.name, 
                        cols = grep("P.Val", colnames(tt.merge)), 
                        rows = 1 + 1:nrow(tt.merge), 
                        rule = "<0.05",
                        style = createStyle(bgFill = brewer.pal(4, "Pastel2")[4]), 
                        type = "expression")
}

# Save workbook
saveWorkbook(wb, "Differentially_Expressed_Genes.xlsx", overwrite = TRUE)

# Create individual dataframe objects in environment (optional)
for (name in names(dataframes_list)) {
  assign(name, dataframes_list[[name]])
}
