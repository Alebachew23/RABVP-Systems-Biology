rename_columns <- function(df) {
  colnames(df)[colnames(df) == "Transcription Factor Name"] <- "TF"
  colnames(df)[colnames(df) == "No. Transcription Factor Search Genes"] <- "IRGs"
  colnames(df)[colnames(df) == "Gene Representation"] <- "Effect"
  colnames(df)[colnames(df) == "Average Log2 Proportion Bound"] <- "Bound"
  colnames(df)[colnames(df) == "Log2 Enrichment"] <- "Enrichment"
  colnames(df)[colnames(df) == "Gene P-Value"] <- "Pvalue"
  colnames(df)[colnames(df) == "Transcription Factor ID"] <- "ID"
  colnames(df)[colnames(df) == "Significance Score"] <- "Significance"
  return(df)
}