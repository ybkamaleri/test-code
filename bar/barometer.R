pkg <- c("ggplot2", "data.table", "cowplot", "shiny")

pyes <- suppressWarnings(sapply(pkg, require, character.only = TRUE))
pno <- pkg[!pyes]
install.packages(pno)

path <- "/mnt/F/Forskningsprosjekter/PDB 2455 - Helseprofiler og til_/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/KH2021NESSTAR"
file01 <- "ARBLEDIGE_2021-08-16-15-17.csv"
df <- fread(file.path(path, file01))
df
DF <- df[, {
  gg = nchar(GEO);
  typ = fcase(gg == 4, "komm");
  list(typ = typ, teller = TELLER, rate = RATE, smr = SMR)
}][!is.na(typ)&!is.na(rate)][1:50]
DF
fwrite(DF, "testdata.csv")
