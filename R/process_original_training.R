# Import libraries, functions and data -------------------------------------------------------------
library(rgdal)
library(dplyr)
sdp025 = function(data,na.rm=T) {
  average = mean(data,na.rm=T)
  std025 = sd(data,na.rm=T)/4
  stdp025 = average+std025
  return(stdp025)
}
sdm025 = function(data,na.rm=T) {
  average = mean(data,na.rm=T)
  std025 = sd(data,na.rm=T)/4
  stdm025 = average-std025
  return(stdm025)
}
summary_lc = function(shp) {
  data = shp@data
  DFEND = data %>% 
    group_by(landcover) %>% 
    summarise_all(funs(min,mean,max,sd,sdp025,sdm025),na.rm=T)

  
  return(DFEND)
}
path = 'C:/Users/italo/Google Drive/PeatBog_Magallanes'
dsn = paste0(path,'/','lc_data.shp')
shp = readOGR(dsn)
# get statistics per cover ------------------------------------------------
summary_allcover = summary_lc(shp)
# export data -------------------------------------------------------------
dsn_out = paste0(path,'/','summary_covers.csv')
write.csv(summary_allcover,dsn_out,row.names=F)