pkg <- c("ggplot2", "data.table", "cowplot", "shiny")

pyes <- suppressWarnings(sapply(pkg, require, character.only = TRUE))
pno <- pkg[!pyes]
install.packages(pno)
