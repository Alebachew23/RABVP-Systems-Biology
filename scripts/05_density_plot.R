density.plot <- function(expression, col, lwd = 2, ...){
  densities <- apply(expression, 2, density)
  high.peak <- which.max(sapply(densities, function(d) max(d$y))) 
  
  plot(densities[[high.peak]], 
       type = "l", 
       lwd = lwd,
       col = col[high.peak],
       ...
  )
  
  for(i in (1:ncol(expression))[-high.peak]){
    lines(densities[[i]], col = col[i], lwd = lwd)
  }
}