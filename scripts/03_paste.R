paste.factor <- function(..., sep = " "){
  y <- list(...)
  y <- lapply(y, function(x){
    if(!is.factor(x)) x <- factor(x)
    x
  })
  levels <- lapply(y, function(x) unique(c(levels(x), NA)))
  levels <- apply(expand.grid(rev(levels))[, length(y):1, drop = FALSE], 1, paste, collapse = sep)
  y.factor <- factor(do.call(what = "paste", args = c(y, sep = sep)), levels = levels)
  droplevels(y.factor)
}
