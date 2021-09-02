#install.packages("remotes")
# Import libraries, functions and data -------------------------------------------------------------
library(rgdal)
library(dplyr)
library(spatialEco)
library(rlist)
library(corrplot)
library(ggplot2)
library(matlab)

graph_S2_spectra = function(spectra_df,output_name) {
  S2Bands = c(443,490,560,665,705,740,783,842,865,945,1610,2190)
  S2BandWidth = c(21,66,36,31,15,15,20,103,21,20,91,175)
  S2Bands_low = S2Bands-(S2BandWidth/2)
  S2Bands_high = S2Bands+(S2BandWidth/2)
  spectra_df$S2Bands = S2Bands
  spectra_df$S2Bands_low = S2Bands_low
  spectra_df$S2Bands_high = c(S2Bands_high[1:11],S2Bands[12])
  #cover_palette = c("#040cff", "#2700ff", "#008f40",'#2a4913','#ffb100','#ff9006','#dedad9','#fff81f','#878787','#ff1414','#fc3503')
  #lc_colors =  c("Water1"="#040cff", "Water2"="#2700ff", "Forest1"="#008f40", "Forest2"='#2a4913', "Shrubland1"='#ffb100', "Shrubland2"='#ff9006', "SnowAndGlacier"='#dedad9', "Grassland"='#fff81f', "BareSoil"='#878787', "Peatbog1"='#ff1414', "Peatbog2"='#fc3503')
  cover_palette = c("#040cff", "#2700ff", "#008f40",'#2a4913','#ffb100','#ff9006','#dedad9','#fff81f','#878787','#8c0769','#d400ff')
  lc_colors =  c("Water1"="#040cff", "Water2"="#2700ff", "Forest1"="#008f40", "Forest2"='#2a4913', "Shrubland1"='#ffb100', "Shrubland2"='#ff9006', "SnowAndGlacier"='#dedad9', "Grassland"='#fff81f', "BareSoil"='#878787', "Peatbog1"='#8c0769', "Peatbog2"='#d400ff')
  
  spectra_plots = ggplot(spectra_df, aes(S2Bands)) + 
    geom_ribbon(aes(ymin=Water1-Water1_std, ymax=Water1+Water1_std),fill=cover_palette[1],alpha=0.9) +
    geom_ribbon(aes(ymin=Water2-Water2_std, ymax=Water2+Water2_std),fill=cover_palette[2],alpha=0.9) +
    geom_ribbon(aes(ymin=Forest1-Forest1_std, ymax=Forest1+Forest1_std),fill=cover_palette[3],alpha=0.9) +
    geom_ribbon(aes(ymin=Forest2-Forest2_std, ymax=Forest2+Forest2_std),fill=cover_palette[4],alpha=0.9) +
    geom_ribbon(aes(ymin=Shrubland1-Shrubland1_std, ymax=Shrubland1+Shrubland1_std),fill=cover_palette[5],alpha=0.9) +
    geom_ribbon(aes(ymin=Shrubland2-Shrubland2_std, ymax=Shrubland2+Shrubland2_std),fill=cover_palette[6],alpha=0.9) +
    geom_ribbon(aes(ymin=SnowAndGlacier-SnowAndGlacier_std, ymax=SnowAndGlacier+SnowAndGlacier_std),fill=cover_palette[7],alpha=0.9) +
    geom_ribbon(aes(ymin=Grassland-Grassland_std, ymax=Grassland+Grassland_std),fill=cover_palette[8],alpha=0.9) +
    geom_ribbon(aes(ymin=BareSoil-BareSoil_std, ymax=BareSoil+BareSoil_std),fill=cover_palette[9],alpha=0.9) +
    geom_ribbon(aes(ymin=Peatbog1-Peatbog1_std, ymax=Peatbog1+Peatbog1_std),fill=cover_palette[10],alpha=0.9) +
    geom_ribbon(aes(ymin=Peatbog2-Peatbog2_std, ymax=Peatbog2+Peatbog2_std),fill=cover_palette[11],alpha=0.9) +
    geom_rect(data = spectra_df,
              aes(xmin = S2Bands_low, xmax = S2Bands_high, ymin = -Inf, ymax = +Inf),
              alpha = .3, inherit.aes = FALSE) +
    geom_line(aes(y = Water1, colour = "Water1")) + 
    geom_line(aes(y = Water1, colour = "Water2")) +
    geom_line(aes(y = Forest1, colour = "Forest1")) + 
    geom_line(aes(y = Forest2, colour = "Forest2")) +
    geom_line(aes(y = Shrubland1, colour ="Shrubland1")) + 
    geom_line(aes(y = Shrubland2, colour = "Shrubland2")) +
    geom_line(aes(y = SnowAndGlacier, colour = "SnowAndGlacier")) + 
    geom_line(aes(y = Grassland, colour = "Grassland")) +
    geom_line(aes(y = BareSoil, colour = "BareSoil")) + 
    geom_line(aes(y = Peatbog1, colour = "Peatbog1")) +
    geom_line(aes(y = Peatbog2, colour = "Peatbog2")) + 
    scale_color_manual(values = lc_colors) +
    scale_y_continuous( name='Reflectance [-]') +
    scale_x_continuous( name='Wavelength [nm]', breaks=spectra_df$S2Bands, labels=spectra_df$S2Bands) +
    theme(panel.background = element_rect(fill='white',colour='white'),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(face="bold", color="black", 
                                     size=15, angle=90),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=15, angle=0))
  #spectra_plots
  ggsave(paste0(getwd(),'/tmp/',output_name,'.png'),spectra_plots,width=500,height=200,units="mm")
  return(spectra_plots)
}


graph_indexes = function(indexes_df,xlabelname,ylabelname,output_name) {

  #cover_palette = c("#040cff", "#2700ff", "#008f40",'#2a4913','#ffb100','#ff9006','#dedad9','#fff81f','#878787','#ff1414','#fc3503')
  #lc_colors =  c("Water1"="#040cff", "Water2"="#2700ff", "Forest1"="#008f40", "Forest2"='#2a4913', "Shrubland1"='#ffb100', "Shrubland2"='#ff9006', "SnowAndGlacier"='#dedad9', "Grassland"='#fff81f', "BareSoil"='#878787', "Peatbog1"='#ff1414', "Peatbog2"='#fc3503')
  cover_palette = c("#040cff", "#2700ff", "#008f40",'#2a4913','#ffb100','#ff9006','#dedad9','#fff81f','#878787','#8c0769','#d400ff')
  lc_colors =  c("Water1"="#040cff", "Water2"="#2700ff", "Forest1"="#008f40", "Forest2"='#2a4913', "Shrubland1"='#ffb100', "Shrubland2"='#ff9006', "SnowAndGlacier"='#dedad9', "Grassland"='#fff81f', "BareSoil"='#878787', "Peatbog1"='#8c0769', "Peatbog2"='#d400ff')
  index_names = row.names(indexes_df)
  x = length(index_names)
  indexes_df$x = seq(1,x)
  indexes_plots = ggplot(indexes_df, aes(x)) + 
    geom_ribbon(aes(ymin=Water1-Water1_std, ymax=Water1+Water1_std),fill=cover_palette[1],alpha=0.9) +
    geom_ribbon(aes(ymin=Water2-Water2_std, ymax=Water2+Water2_std),fill=cover_palette[2],alpha=0.9) +
    geom_ribbon(aes(ymin=Forest1-Forest1_std, ymax=Forest1+Forest1_std),fill=cover_palette[3],alpha=0.9) +
    geom_ribbon(aes(ymin=Forest2-Forest2_std, ymax=Forest2+Forest2_std),fill=cover_palette[4],alpha=0.9) +
    geom_ribbon(aes(ymin=Shrubland1-Shrubland1_std, ymax=Shrubland1+Shrubland1_std),fill=cover_palette[5],alpha=0.9) +
    geom_ribbon(aes(ymin=Shrubland2-Shrubland2_std, ymax=Shrubland2+Shrubland2_std),fill=cover_palette[6],alpha=0.9) +
    geom_ribbon(aes(ymin=SnowAndGlacier-SnowAndGlacier_std, ymax=SnowAndGlacier+SnowAndGlacier_std),fill=cover_palette[7],alpha=0.9) +
    geom_ribbon(aes(ymin=Grassland-Grassland_std, ymax=Grassland+Grassland_std),fill=cover_palette[8],alpha=0.9) +
    geom_ribbon(aes(ymin=BareSoil-BareSoil_std, ymax=BareSoil+BareSoil_std),fill=cover_palette[9],alpha=0.9) +
    geom_ribbon(aes(ymin=Peatbog1-Peatbog1_std, ymax=Peatbog1+Peatbog1_std),fill=cover_palette[10],alpha=0.9) +
    geom_ribbon(aes(ymin=Peatbog2-Peatbog2_std, ymax=Peatbog2+Peatbog2_std),fill=cover_palette[11],alpha=0.9) +
    geom_line(aes(y = Water1, colour = "Water1")) + 
    geom_line(aes(y = Water1, colour = "Water2")) +
    geom_line(aes(y = Forest1, colour = "Forest1")) + 
    geom_line(aes(y = Forest2, colour = "Forest2")) +
    geom_line(aes(y = Shrubland1, colour ="Shrubland1")) + 
    geom_line(aes(y = Shrubland2, colour = "Shrubland2")) +
    geom_line(aes(y = SnowAndGlacier, colour = "SnowAndGlacier")) + 
    geom_line(aes(y = Grassland, colour = "Grassland")) +
    geom_line(aes(y = BareSoil, colour = "BareSoil")) + 
    geom_line(aes(y = Peatbog1, colour = "Peatbog1")) +
    geom_line(aes(y = Peatbog2, colour = "Peatbog2")) + 
    scale_color_manual(values = lc_colors) +
    scale_y_continuous( name=ylabelname) +
    scale_x_continuous( name=xlabelname,breaks=1:x, labels=index_names) +
    theme(panel.background = element_rect(fill='white',colour='white'),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(face="bold", color="black", 
                                     size=15, angle=90),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=15, angle=0))
  #spectra_plots
  ggsave(paste0(getwd(),'/tmp/',output_name,'.png'),indexes_plots,width=500,height=200,units="mm")
  return(indexes_plots)
}


# get data for processing ------------------------------------------------
path = paste0(getwd(),'/tmp/data')
dsn = paste0(path,'/','lc_data.shp')
spatial_covers = readOGR(dsn)
#View(spatial_covers@data)
landcover=unique(spatial_covers@data$landcover)
means_lc = aggregate(. ~ landcover, data = spatial_covers@data, mean,na.rm=T)
std_lc = aggregate(. ~ landcover, data = spatial_covers@data, sd,na.rm=T)
min_lc = aggregate(. ~ landcover, data = spatial_covers@data, min,na.rm=T)
max_lc = aggregate(. ~ landcover, data = spatial_covers@data, max,na.rm=T)
x=1
#View(means_lc)
Landcover = c('Water1','Water2','Forest1','Forest2','Shrubland1','Shrubland2','SnowAndGlacier','Grassland','BareSoil','Peatbog1','Peatbog2')
Landcover_std = paste(Landcover, "_std", sep="")
Landcover_min = paste(Landcover, "_min", sep="")
Landcover_max = paste(Landcover, "_max", sep="")

means_matrix=t(means_lc)
means_matrix = means_matrix[-1,]
means_df = data.frame(means_matrix)
names(means_df) = Landcover

std_matrix=t(std_lc)
std_matrix = std_matrix[-1,]
std_df = data.frame(std_matrix)
names(std_df) = Landcover_std

max_matrix=t(max_lc)
max_matrix = max_matrix[-1,]
max_df = data.frame(max_matrix)
names(max_df) = Landcover_max

min_matrix=t(min_lc)
min_matrix = min_matrix[-1,]
min_df = data.frame(min_matrix)
names(min_df) = Landcover_min

master_df = cbind(means_df,std_df,max_df,min_df)
S2summer = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")
spectra_df_summer =  subset(master_df,row.names(master_df) %in% S2summer)

S2autumn = c("B1_a","B2_a","B3_a","B4_a","B5_a","B6_a","B7_a","B8_a","B8A_a","B9_a","B11_a","B12_a")
spectra_df_autumn =  subset(master_df,row.names(master_df) %in% S2autumn)
spectra_df_autumn = spectra_df_autumn[match(S2autumn, rownames(spectra_df_autumn)), ]

S2winter = c("B1_w","B2_w","B3_w","B4_w","B5_w","B6_w","B7_w","B8_w","B8A_w","B9_w","B11_w","B12_w")
spectra_df_winter = subset(master_df,row.names(master_df) %in% S2winter)
spectra_df_winter = spectra_df_winter[match(S2winter, rownames(spectra_df_winter)), ]


S2spring = c("B1_s","B2_s","B3_s","B4_s","B5_s","B6_s","B7_s","B8_s","B8A_s","B9_s","B11_s","B12_s")
spectra_df_spring =  subset(master_df,row.names(master_df) %in% S2spring)
spectra_df_spring = spectra_df_spring[match(S2spring, rownames(spectra_df_spring)), ]

summergraph = graph_S2_spectra(spectra_df_summer,'Summer_S2_2019')
autumngraph = graph_S2_spectra(spectra_df_autumn,'Autumn_S2_2019')
wintergraph = graph_S2_spectra(spectra_df_winter,'Winter_S2_2019')
springgraph = graph_S2_spectra(spectra_df_spring,'Spring_S2_2019')


indexS2 = c("ndvi","ndvi_a","ndvi_w","ndvi_s","seli","seli_a","seli_w","seli_s","ndwi","ndwi_a","ndwi_w","ndwi_s","ndsi","ndsi_a","ndsi_w","ndsi_s")
indexS2_df =  subset(master_df,row.names(master_df) %in% indexS2)
indexS2_df = indexS2_df[match(indexS2, rownames(indexS2_df)), ]
indexS2graph = graph_indexes(indexS2_df,'Spectral indexes','[-]','Spectral_index_S2')

S1Series = c("VH_Jan","VH_Feb","VH_Mar","VH_Apr","VH_May","VH_Jun","VH_Jul","VH_Aug","VH_Sep","VH_Oct","VH_Nov","VH_Dec")
S1Series_df =  subset(master_df,row.names(master_df) %in% S1Series)
S1Series_df = S1Series_df[match(S1Series, rownames(S1Series_df)), ]
S1Seriesgraph = graph_indexes(S1Series_df,'Months','Backscattering [dB]','S1_Backscatter')

indexTopo = c("aspect","slope","twi","tpi","dem")
Topo_df =  subset(master_df,row.names(master_df) %in% indexTopo)
Topo_df = Topo_df[match(indexTopo, rownames(Topo_df)), ]
Topo_dfsgraph = graph_indexes(Topo_df,'Topography','[-]','Topo_V0')


y = length(names(means_df))

df_B <- data.frame(matrix(ncol = y, nrow = y))
df_JM <- data.frame(matrix(ncol = y, nrow = y))
df_M <- data.frame(matrix(ncol = y, nrow = y))
df_D <- data.frame(matrix(ncol = y, nrow = y))
df_TD <- data.frame(matrix(ncol = y, nrow = y))

colnames(df_B) = names(means_df)
rownames(df_B) = names(means_df)
colnames(df_JM) = names(means_df)
rownames(df_JM) = names(means_df)
colnames(df_M) = names(means_df)
rownames(df_M) = names(means_df)
colnames(df_D) = names(means_df)
rownames(df_D) = names(means_df)
colnames(df_TD) = names(means_df)
rownames(df_TD) = names(means_df)


for (j in 1:y) {
  for (k in 1:y) {
    # 1. Open jpeg file
    #jpeg(paste0("images/rplots/plot_",spectra_list[j][[1]],'_',spectra_list[k][[1]],".jpg"), width = 350, height = "350")
    separ = separability(means_df[,j],y=means_df[,k], plot=FALSE,cols=c("red","blue"),clabs=c(names(means_df[j]),names(means_df[k]) ))

    #dev.off()
    #print(spectra_list[j][[1]])
    #print(spectra_list[k][[1]])
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

corrplot_palette = jet.colors(1000)
color =  c(corrplot_palette,corrplot_palette)
corrplot(m_B,col=color, method="circle",is.corr = FALSE)
corrplot(m_JM,col=color, method="circle",is.corr = FALSE)
corrplot(m_M,col=color, method="circle",is.corr = FALSE)
corrplot(m_D,col=color, method="circle",is.corr = FALSE)
corrplot(m_TD,col=color, method="circle",is.corr = FALSE)
