### Invandring and invandrere
### -------------------------
library(klassR)
ListFamily(family = 8) #Invandring family
GetFamily(klass = 82) #klass is klass_nr and not klass_name

GetVersion(klass = 82, date = "2020-01-01", family = 8) #all versions
verFam <- GetVersion(family = 8)
lapply(verFam, function(x) GetName(x))

GetKlass(82)
GetKlass(355)

GetVersion(82, family = 8)
GetName(332)
GetName(972)


sapply(c("httr", "jsonlite"), require, character.only = T)
## Access the same table for bydel
byJRawInd <- "https://data.ssb.no/api/klass/v1/versions/332.json"
byJsInd <- fromJSON(byJRawInd)
names(byJsInd)
str(byJsInd, max.level = 1)
byJsInd$classificationItems
