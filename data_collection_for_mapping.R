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

rasterOptions(tmpdir="F:/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"



#layer that enfolds the entiere vinter area of the reindeer herding districts (according to the reindeer husbandary plan (see script data collection))
t1<-readOGR("F:/Lavproject2019/RBP-lichen_projct_2019","varvinter")


las_rutor<-readOGR("//YKSI/13_Geodata/Laser_fjall/las_rutor","rutor")
