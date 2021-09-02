library(rgdal)
library(caret)

path = 'C:/Users/italo/Google Drive'
dsn = paste0(path,'/','covers_CALVAL_ALLBANDSUPDATEcaca.shp')
lc_spatial = readOGR(dsn)
lc = lc_spatial@data
val = as.factor(lc$valId)
cal = as.factor(lc$first)

cm=confusionMatrix(cal,val)
str(cm)
print(cm)
summarycm=cm$overall
print(summarycm)

