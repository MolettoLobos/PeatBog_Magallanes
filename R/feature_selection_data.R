# Import libraries, functions and data -------------------------------------------------------------
library(caret)
library(rgdal)

path = 'C:/Users/italo/Google Drive/PeatBog_Magallanes'
dsn = paste0(path,'/','lc_data_spectra.shp')
shp = readOGR(dsn)
# get data for processing ------------------------------------------------

data = shp@data

correlation=cor(data, use = "complete.obs")
maitenes_index_filtered_NA = na.omit(data)
pred = data[,-1]
var = data[,1]
# GET BEST INDEX TO PREDICT BY LOOCV------------------------------------------------
set.seed(550) 
control <- rfeControl(functions=rfFuncs, method="LOOCV", verbose = T) 
rfeDAT<- rfe(pred, var, rfeControl=control); rfeDAT
print(rfeDAT); pred(rfeDAT)

df<- pickVars(rfeDAT$variables, 4)
prueba  <- data.frame(maitenes_index_filtered_NA[,c(df)],var)
