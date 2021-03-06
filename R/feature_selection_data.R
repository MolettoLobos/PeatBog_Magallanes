# Import libraries, functions and data -------------------------------------------------------------
library(caret)
library(rgdal)
library(doParallel)
path = 'C:/Users/italo/Google Drive/PeatBog_Magallanes/feature_selectiondata'
dsn = paste0(path,'/','training_data_23_covers_spectra.shp')
shp = readOGR(dsn)
# get data for processing ------------------------------------------------
numCores <- detectCores()-1
numCores
registerDoParallel(numCores)
data = shp@data

correlation=cor(data, use = "complete.obs")
data_NA = na.omit(data)
pred = data_NA[,-1]
var = data_NA[,1]
# GET BEST INDEX TO PREDICT BY LOOCV------------------------------------------------
set.seed(550) 
control <- rfeControl(functions=rfFuncs, method="LOOCV", verbose = T) 
rfeDAT<- rfe(pred, var, rfeControl=control); rfeDAT
print(rfeDAT); pred(rfeDAT)
# summarize the results
print(rfeDAT)
# list the chosen features
predictors(rfeDAT)
# plot the results
plot(rfeDAT, type=c("g", "o"))


df<- pickVars(rfeDAT$variables, 4)
prueba  <- data.frame(maitenes_index_filtered_NA[,c(df)],var)
