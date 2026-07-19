# gene_plot
gene.plot <- function(gene, groups = TRUE, full.expression = FALSE, raw.counts = FALSE,  ...){
  display.group <- group
  
  if(full.expression){
    display.group <- group
    if(raw.counts){
      data <- as.matrix(countdata)
    } else {
      data <- log2.cpm.filtered.norm
    }
  } else {
    data <- log2.cpm.filtered.norm
    
  }
  
  if(full.expression){
    gene.annotation <- genes
  } else {
    gene.annotation <- myDGElist$genes
  }
  
  ylab <- expression("log"[2]*"(Counts per Million)")
  if(raw.counts){
    ylab <- "Counts"
  }
  
  beeswarm::beeswarm(data[gene, groups] ~ droplevels(display.group[groups]),
                     pwcol = group.cols[as.character(display.group)][groups],
                     cex = 1.5,
                     ylab = ylab,
                     labels = sub("_", "\n", levels(droplevels(display.group[groups]))),
                     xlab = "",
                     las = 1, cex.axis = 1,
                     main = gene.annotation[gene, "SYMBOL"],
                     corral = "random", font.main = 4,
                     ...
  )
  # abline(v = which(diff(as.integer(factor(sub("_.*", "", levels(experiment.group))))) != 0) + 0.5)
}
