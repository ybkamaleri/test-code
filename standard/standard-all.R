## Calculate direct and indirect standardization
library(data.table)
DT <- fread("TestData.csv")
DT[, .N, by = GEO]


## Direct standardization
geov <- c(3, 11, 15, 18, 38, 42, 46, 30, 50, 54)
dirStd <- list()
for (i in geov){
  dt <- subset(DT, GEO == i)
  indx <- epitools::ageadjust.direct(
    count = dt$case,
    pop = dt$TELLER,
    stdpop = DT[GEO == 0, ]$TELLER
  )

  ut <- round(1e3 * indx, 2)
  dtut <- data.table(geo = i, crude = ut[1], adj = ut[2], lci = ut[3], uci = ut[4])
  col <- paste0("geo_", i)
  dirStd[[col]] <- dtut
}


## Indirect Standardization
indStd <- list()
for (i in geov){
  dt <- subset(DT, GEO == i)
  indx <- epitools::ageadjust.indirect(
    count = dt$case,
    pop = dt$TELLER,
    stdcount = DT[GEO==0, case], 
    stdpop = DT[GEO == 0, TELLER]
  )

  ut <- round(1e3 * indx$rate, 2)
  dtut <- data.table(geo = i, crude = ut[1], adj = ut[2], lci = ut[3], uci = ut[4])
  col <- paste0("geo_", i)
  indStd[[col]] <- dtut
}


