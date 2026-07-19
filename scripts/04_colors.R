group <- paste.factor(targets$Protein, factor(targets$Time, levels = c("0hr", "1hr", "6hr", "24hr","24hr_OFF")), sep = "_")


group.cols <- c(brewer.pal(8, "Blues")[3],brewer.pal(4, "BuGn")[3],brewer.pal(4, "YlOrRd")[3],
                brewer.pal(8, "BrBG")[3],brewer.pal(3, "Greens")[3],brewer.pal(6, "Greys")[3],
                brewer.pal(3, "Oranges")[3],brewer.pal(8, "OrRd")[3],brewer.pal(4, "PuBu")[3],
                brewer.pal(3, "PuBuGn")[3],brewer.pal(6, "PuRd")[3],brewer.pal(8, "Purples")[3],
                brewer.pal(6, "RdPu")[3],brewer.pal(8, "Reds")[3],brewer.pal(8, "YlGn")[3]) %>% magrittr::set_names(levels(group))

Protein.cols <- brewer.pal(8, "Accent") %>% `[`(1:length(unique(targets$Protein))) %>% magrittr::set_names(unique(targets$Protein))

Time.cols <- brewer.pal(8, "Set1") %>% `[`(1:length(unique(targets$Time))) %>% magrittr::set_names(unique(targets$Time))

IFN.Cols <- c( "0hr"= "#feebe2","1hr"= "#fbb4b9","6hr" = "#f768a1","24hr"= "#c51b8a")