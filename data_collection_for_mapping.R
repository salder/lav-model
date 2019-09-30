#file data collection was used to design the sampling under sthe summer 2019
#this file is used now for maping lichen in the different reindeer herding districts 
#with updated data (satellite, NFI, including laser scanning data as soil type, and clear cut data)

#Sven Adler
#initial 17-09-2019


library(rgdal)
library(raster)
library(mgcv)
library(dplyr)
library(mapview)
library(rgeos)
library(lidR)
library(dynatopmodel)
library(RStoolbox)

#rasterOptions(tmpdir="F:/temp_raster/")
rasterOptions(tmpdir="//YKSI/13_Geodata/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"



#layer that enfolds the entiere vinter area of the reindeer herding districts (according to the reindeer husbandary plan (see script data collection))
t1<-readOGR("F:/Lavproject2019/RBP-lichen_projct_2019","varvinter")
e1<-extent(t1)

las_rutor<-readOGR("//YKSI/13_Geodata/Laser_fjall/las_rutor","rutor")
#all las files that are in the area of intrest
las_files<-crop(las_rutor,e1)$Las_Namn

all_laser<-dir("//YKSI/13_Geodata/Laser_fjall",pattern = ".laz")
ta<-all_laser%in%las_files
files_las_vinter<-all_laser[ta==TRUE]



#Copy the relevante laser files to a local stoage for speed up
for (i in 1:length(files_las_vinter))
     {
       f1<-paste("//YKSI/13_Geodata/Laser_fjall/",files_las_vinter[i],sep="")
       file.copy(from=f1,to="//YKSI/13_Geodata/Laser_vinterbete/")
     }




# laser files 
veg_cover<-function(Z,min=0.05)
{
  l<-length(Z)
  l_over<-sum(ifelse(Z>=min,1,0))
  cover<-l_over/l
  return(cover)
}



b1<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")
altitude<-raster("M:/THUF-Fjall/hojddata2.tif")
length(files_las_vinter)
#41751

#for (i in 1:length(files_las_vinter))     #i=9892
for (i in 1:13917)  
{
  f1<-paste("//YKSI/13_Geodata/Laser_fjall/",files_las_vinter[i],sep="")
  file.copy(from=f1,to="F:/temp_raster/")
  
  las_file<-paste("F:/temp_raster/",files_las_vinter[i],sep="")
  las_square<-readLAS(las_file) 
  las_mean<-(grid_metrics(las_square, mean(Z), 10))
  las_sd<-(grid_metrics(las_square, sd(Z), 10))
  las_cover_over_1_5<-(grid_metrics(las_square, fun=veg_cover(Z,min=1.5), 10))
  las_tree<-(grid_metrics(las_square,.stdmetrics,10))
  proj4string(las_sd)<-projSWEREF
  las_tree95<-las_tree[[27]]
  e_l<-extent(las_sd)
  t1<-crop(b1,e_l)
  alt_1<-crop(altitude,extent(t1))  
  alti<-resample(alt_1,t1,method="bilinear")  
  e_sat<-extent(b1)
  las_mean<-crop(las_mean,e_sat)
  las_sd<-crop(las_sd,e_sat)
  las_cover_over_1_5<-crop(las_cover_over_1_5,e_sat)
  las_tree95<-crop(las_tree95,e_sat)
  alti<-crop(alti,e_sat)
  
  
  res<-addLayer(las_mean,las_sd,las_cover_over_1_5,las_tree95,alti)#,alt_TWI,alt_upslope)
  names(res)<-c("las_mean","las_sd","las_cover_1_5","las95","altitude")
  saveRDS(res,file=paste("//YKSI/13_Geodata/lid_temp/",i,".rds",sep=""))
  file.remove(las_file)
}








