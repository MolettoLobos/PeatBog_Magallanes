#install.packages("remotes")
# Import libraries, functions and data -------------------------------------------------------------
library(rgdal)
library(dplyr)
library(spatialEco)
library(rlist)
library(corrplot)
# get data for processing ------------------------------------------------
path = 'C:/Users/italo/Google Drive/PeatBog_Magallanes'
dsn = paste0(path,'/','lc_data.shp')
cover_data_fnames=list.files(path=path,pattern='.shp',full.names=TRUE)
cover_data=list.files(path=path,pattern='.shp',full.names=FALSE)
x=length(cover_data)
spectra_list = list()
spectra_list_stacked = list()
spectra_list_mean = list()
for (i in 1:x) {
  nameobj=strsplit(cover_data[i],'.shp')[[1]]
  nameobj_stack = paste0(nameobj,'_stack')
  nameobj_statistics = paste0(nameobj,'_statistics')
  nameobj_mean = paste0(nameobj,'_mean')
  spectra_list = list.append(spectra_list,nameobj)
  spectra_list_stacked = list.append(spectra_list_stacked,nameobj_stack)
  spectra_list_mean = list.append(spectra_list_mean,nameobj_mean)
  norm1 <- dnorm(seq(-20,20,length=5000),mean=0,sd=1) 
  norm2 <- dnorm(seq(-20,20,length=5000),mean=0.2,sd=2) 
  assign(nameobj,readOGR(cover_data_fnames[i]))
  assign(nameobj,get(nameobj)@data %>% select(B1,B2,B3,B4,B5,B6,B7,B8,B8A,B9,B11,B12))
  assign(nameobj_stack,stack(get(nameobj))$values)
  assign(nameobj_statistics , get(nameobj) %>% 
            summarise_all(funs(mean,max,sd),na.rm=T)) 
  assign(nameobj_mean , stack(get(nameobj) %>% 
           summarise_all(funs(mean),na.rm=T))$value)
  
}

y = length(spectra_list)

df_B <- data.frame(matrix(ncol = y, nrow = y))
df_JM <- data.frame(matrix(ncol = y, nrow = y))
df_M <- data.frame(matrix(ncol = y, nrow = y))
df_D <- data.frame(matrix(ncol = y, nrow = y))
df_TD <- data.frame(matrix(ncol = y, nrow = y))

colnames(df_B) = spectra_list
rownames(df_B) = spectra_list
colnames(df_JM) = spectra_list
rownames(df_JM) = spectra_list
colnames(df_M) = spectra_list
rownames(df_M) = spectra_list
colnames(df_D) = spectra_list
rownames(df_D) = spectra_list
colnames(df_TD) = spectra_list
rownames(df_TD) = spectra_list


for (j in 1:y) {
  for (k in 1:y) {
    # 1. Open jpeg file
    #jpeg(paste0("images/rplots/plot_",spectra_list[j][[1]],'_',spectra_list[k][[1]],".jpg"), width = 350, height = "350")
    separ = separability(x=get(spectra_list_mean[j][[1]]),y=get(spectra_list_mean[k][[1]]), plot=FALSE,cols=c("red","blue"),clabs=c(spectra_list[j],spectra_list[k]))
    #dev.off()
    print(spectra_list[j][[1]])
    print(spectra_list[k][[1]])
    print(separ)
    df_B[j,k] = separ[1]
    df_JM[j,k] = separ[2]
    df_M[j,k] = separ[3]
    df_D[j,k] = separ[5]
    df_TD[j,k] = separ[6]
  }
}
m_B = as.matrix(df_B)
m_JM = as.matrix(df_JM)
m_M = as.matrix(df_M)
m_D = as.matrix(df_D)
m_TD = as.matrix(df_TD)

mm_B= m_B[-20,-20]
mm_JM= m_JM[-20,-20]
mm_M= m_M[-20,-20]
mm_D= m_D[-20,-20]
mm_TD= m_TD[-20,-20]

corrplot(mm_B, method="circle",is.corr = FALSE)
corrplot(mm_JM, method="circle",is.corr = FALSE)
corrplot(mm_M, method="circle",is.corr = FALSE)
corrplot(mm_D, method="circle",is.corr = FALSE)
corrplot(mm_TD, method="circle",is.corr = FALSE)

