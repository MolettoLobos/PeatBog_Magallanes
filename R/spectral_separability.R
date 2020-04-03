#install.packages("remotes")
# Import libraries, functions and data -------------------------------------------------------------
library(rgdal)
library(dplyr)
library(spatialEco)
library(rlist)
library(corrplot)
library(ggplot2)
# get data for processing ------------------------------------------------
path = 'C:/Users/italo/Google Drive/PeatBog_Magallanes'
dsn = paste0(path,'/','lc_data.shp')
cover_data_fnames=list.files(path=path,pattern='.shp',full.names=TRUE)
cover_data=list.files(path=path,pattern='.shp',full.names=FALSE)
x=length(cover_data)
spectra_list = list()
spectra_list_stacked = list()
spectra_list_statistics = list()
spectra_list_mean = list()
for (i in 1:x) {
  nameobj=strsplit(cover_data[i],'.shp')[[1]]
  nameobj_stack = paste0(nameobj,'_stack')
  nameobj_statistics = paste0(nameobj,'_statistics')
  nameobj_mean = paste0(nameobj,'_mean')
  spectra_list = list.append(spectra_list,nameobj)
  spectra_list_stacked = list.append(spectra_list_stacked,nameobj_stack)
  spectra_list_statistics = list.append(spectra_list_statistics,nameobj_mean)
  spectra_list_mean = list.append(spectra_list_mean,nameobj_mean)
  norm1 <- dnorm(seq(-20,20,length=5000),mean=0,sd=1) 
  norm2 <- dnorm(seq(-20,20,length=5000),mean=0.2,sd=2) 
  assign(nameobj,readOGR(cover_data_fnames[i]))
  assign(nameobj,get(nameobj)@data %>% select(B1,B2,B3,B4,B5,B6,B7,B8,B8A,B9,B11,B12))
  assign(nameobj_stack,stack(get(nameobj))$values)
  assign(nameobj_statistics , get(nameobj) %>% 
            summarise_all(funs(mean,sd,min,max),na.rm=T)) 
  assign(nameobj_mean , stack(get(nameobj) %>% 
           summarise_all(funs(mean),na.rm=T))$value)
  
}
S2Bands = c(443,490,560,665,705,740,783,842,865,945,1610,2190)


master_df = data.frame(S2Bands=S2Bands,
                       ext_BushA2019_mean=t(ext_BushA2019_statistics[,1:12]),
                       ext_BushB2019_mean=t(ext_BushB2019_statistics[,1:12]),
                       ext_BushC2019_mean=t(ext_BushC2019_statistics[,1:12]),
                       ext_BushD2019_mean=t(ext_BushD2019_statistics[,1:12]),
                       ext_BushE2019_mean=t(ext_BushE2019_statistics[,1:12]),
                       ext_BushF2019_mean=t(ext_BushF2019_statistics[,1:12]),
                       ext_BushG2019_mean=t(ext_BushG2019_statistics[,1:12]),
                       ext_BushH2019_mean=t(ext_BushH2019_statistics[,1:12]),
                       ext_BushI2019_mean=t(ext_BushI2019_statistics[,1:12]),
                       ext_BushJ2019_mean=t(ext_BushJ2019_statistics[,1:12]),
                       ext_Clearland2019_mean=t(ext_Clearland2019_statistics[,1:12]),
                       ext_Forest2019_mean=t(ext_Forest2019_statistics[,1:12]),
                       ext_ForestC2019_mean=t(ext_ForestC2019_statistics[,1:12]),
                       ext_Herb2019_mean=t(ext_Herb2019_statistics[,1:12]),
                       ext_HerbsB2019_mean=t(ext_HerbsB2019_statistics[,1:12]),
                       ext_PeatA2019_mean=t(ext_PeatA2019_statistics[,1:12]),
                       ext_PeatB2019_mean=t(ext_PeatB2019_statistics[,1:12]),
                       ext_Sombra2019_mean=t(ext_Sombra2019_statistics[,1:12]),
                       ext_Waterbodies2019_mean=t(ext_Waterbodies2019_statistics[,1:12]),
                       ext_BushA2019_sd=t(ext_BushA2019_statistics[,13:24]),
                       ext_BushB2019_sd=t(ext_BushB2019_statistics[,13:24]),
                       ext_BushC2019_sd=t(ext_BushC2019_statistics[,13:24]),
                       ext_BushD2019_sd=t(ext_BushD2019_statistics[,13:24]),
                       ext_BushE2019_sd=t(ext_BushE2019_statistics[,13:24]),
                       ext_BushF2019_sd=t(ext_BushF2019_statistics[,13:24]),
                       ext_BushG2019_sd=t(ext_BushG2019_statistics[,13:24]),
                       ext_BushH2019_sd=t(ext_BushH2019_statistics[,13:24]),
                       ext_BushI2019_sd=t(ext_BushI2019_statistics[,13:24]),
                       ext_BushJ2019_sd=t(ext_BushJ2019_statistics[,13:24]),
                       ext_Clearland2019_sd=t(ext_Clearland2019_statistics[,13:24]),
                       ext_Forest2019_sd=t(ext_Forest2019_statistics[,13:24]),
                       ext_ForestC2019_sd=t(ext_ForestC2019_statistics[,13:24]),
                       ext_Herb2019_sd=t(ext_Herb2019_statistics[,13:24]),
                       ext_HerbsB2019_sd=t(ext_HerbsB2019_statistics[,13:24]),
                       ext_PeatA2019_sd=t(ext_PeatA2019_statistics[,13:24]),
                       ext_PeatB2019_sd=t(ext_PeatB2019_statistics[,13:24]),
                       ext_Sombra2019_sd=t(ext_Sombra2019_statistics[,13:24]),
                       ext_Waterbodies2019_sd=t(ext_Waterbodies2019_statistics[,13:24]),
                       ext_BushA2019_min=t(ext_BushA2019_statistics[,25:36]),
                       ext_BushB2019_min=t(ext_BushB2019_statistics[,25:36]),
                       ext_BushC2019_min=t(ext_BushC2019_statistics[,25:36]),
                       ext_BushD2019_min=t(ext_BushD2019_statistics[,25:36]),
                       ext_BushE2019_min=t(ext_BushE2019_statistics[,25:36]),
                       ext_BushF2019_min=t(ext_BushF2019_statistics[,25:36]),
                       ext_BushG2019_min=t(ext_BushG2019_statistics[,25:36]),
                       ext_BushH2019_min=t(ext_BushH2019_statistics[,25:36]),
                       ext_BushI2019_min=t(ext_BushI2019_statistics[,25:36]),
                       ext_BushJ2019_min=t(ext_BushJ2019_statistics[,25:36]),
                       ext_Clearland2019_min=t(ext_Clearland2019_statistics[,25:36]),
                       ext_Forest2019_min=t(ext_Forest2019_statistics[,25:36]),
                       ext_ForestC2019_min=t(ext_ForestC2019_statistics[,25:36]),
                       ext_Herb2019_min=t(ext_Herb2019_statistics[,25:36]),
                       ext_HerbsB2019_min=t(ext_HerbsB2019_statistics[,25:36]),
                       ext_PeatA2019_min=t(ext_PeatA2019_statistics[,25:36]),
                       ext_PeatB2019_min=t(ext_PeatB2019_statistics[,25:36]),
                       ext_Sombra2019_min=t(ext_Sombra2019_statistics[,25:36]),
                       ext_Waterbodies2019_min=t(ext_Waterbodies2019_statistics[,25:36]),
                       ext_BushA2019_max=t(ext_BushA2019_statistics[,37:48]),
                       ext_BushB2019_max=t(ext_BushB2019_statistics[,37:48]),
                       ext_BushC2019_max=t(ext_BushC2019_statistics[,37:48]),
                       ext_BushD2019_max=t(ext_BushD2019_statistics[,37:48]),
                       ext_BushE2019_max=t(ext_BushE2019_statistics[,37:48]),
                       ext_BushF2019_max=t(ext_BushF2019_statistics[,37:48]),
                       ext_BushG2019_max=t(ext_BushG2019_statistics[,37:48]),
                       ext_BushH2019_max=t(ext_BushH2019_statistics[,37:48]),
                       ext_BushI2019_max=t(ext_BushI2019_statistics[,37:48]),
                       ext_BushJ2019_max=t(ext_BushJ2019_statistics[,37:48]),
                       ext_Clearland2019_max=t(ext_Clearland2019_statistics[,37:48]),
                       ext_Forest2019_max=t(ext_Forest2019_statistics[,37:48]),
                       ext_ForestC2019_max=t(ext_ForestC2019_statistics[,37:48]),
                       ext_Herb2019_max=t(ext_Herb2019_statistics[,37:48]),
                       ext_HerbsB2019_max=t(ext_HerbsB2019_statistics[,37:48]),
                       ext_PeatA2019_max=t(ext_PeatA2019_statistics[,37:48]),
                       ext_PeatB2019_max=t(ext_PeatB2019_statistics[,37:48]),
                       ext_Sombra2019_max=t(ext_Sombra2019_statistics[,37:48]),
                       ext_Waterbodies2019_max=t(ext_Waterbodies2019_statistics[,37:48])
                       )

spectra_plots = ggplot(master_df, aes(S2Bands)) + 
    geom_line(aes(y = ext_BushA2019_mean, colour = "BushA")) + 
    geom_line(aes(y = ext_BushB2019_mean, colour = "BushB")) +
    geom_line(aes(y = ext_BushC2019_mean, colour = "BushC")) + 
    geom_line(aes(y = ext_BushD2019_mean, colour = "BushD")) +
    geom_line(aes(y = ext_BushE2019_mean, colour = "BushE")) + 
    geom_line(aes(y = ext_BushF2019_mean, colour = "BushF")) +
    geom_line(aes(y = ext_BushG2019_mean, colour = "BushG")) + 
    geom_line(aes(y = ext_BushH2019_mean, colour = "BushH")) +
    geom_line(aes(y = ext_BushI2019_mean, colour = "BushI")) + 
    geom_line(aes(y = ext_BushJ2019_mean, colour = "BushJ")) +
    geom_line(aes(y = ext_Clearland2019_mean, colour = "Clearland")) + 
    geom_line(aes(y = ext_Forest2019_mean, colour = "Forest")) +
    geom_line(aes(y = ext_ForestB2019_mean, colour = "ForestB")) + 
    geom_line(aes(y = ext_ForestC2019_mean, colour = "ForestC")) +
    geom_line(aes(y = ext_Herb2019_mean, colour = "Herb")) + 
    geom_line(aes(y = ext_HerbsB2019_mean, colour = "HerbsB")) +
    geom_line(aes(y = ext_PeatA2019_mean, colour = "PeatA")) + 
    geom_line(aes(y = ext_PeatB2019_mean, colour = "PeatB")) +
    geom_line(aes(y = ext_Sombra2019_mean, colour = "Sombra")) + 
    geom_line(aes(y = ext_Waterbodies2019_mean, colour = "Waterbodies")) +
    scale_color_manual(values = c("#fffb00", "#6e6d40", "#8a993d",'#e30097','#73d7de','#596104','#c0eb54','#35401a','#d7e6b3','#757219','#a642c7','#00dded','#139ded','#00ffe5','#2fff00','#82ff66','#d4390b','#ff0d00','#6b6a6a','#140096')) +
    scale_y_continuous(name='Reflectance [-]') +
    scale_x_continuous(name='Wavelength [nm]', breaks=master_df$S2Bands, labels=master_df$S2Bands) +
    theme(panel.background = element_rect(fill='white',colour='white'),
      panel.border = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(face="bold", color="black", 
                                        size=15, angle=90),
             axis.text.y = element_text(face="bold", color="black", 
                                        size=15, angle=0))


ggsave(paste0(getwd(),'/images/rplots/spectra.png'),spectra_plots,width=500,height=200,units="mm")
1+1





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



plot1 <- ggplot( aes(v, p)) + 
  geom_line(x=S2Bands, y=t(get(nameobj_statistics)))# +
  #geom_step(data = df2)
#)

for (j in 1:y) {
  for (k in 1:y) {
    # 1. Open jpeg file
    #jpeg(paste0("images/rplots/plot_",spectra_list[j][[1]],'_',spectra_list[k][[1]],".jpg"), width = 350, height = "350")
    separ = separability(x=get(spectra_list_stacked[j][[1]]),y=get(spectra_list_stacked[k][[1]]), plot=FALSE,cols=c("red","blue"),clabs=c(spectra_list[j],spectra_list[k]))
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
