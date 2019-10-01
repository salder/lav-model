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


over1_5<-raster("F:/Lavproject2019/laser_old/veg_vinter.tif")
trad_h<-raster("F:/Lavproject2019/laser_old/p95_vinter.tif")


b10<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 




#2. Tax data 2012-20017

tax.data<-read.csv("F:/Boliden/data analysis/ud1846vegdata.csv",sep=";")
tax_lav.data<-tax.data%>%filter(Tackningsart_latin=="Cladina spp.")

tax_lav.sp<-SpatialPointsDataFrame(coords=tax_lav.data[,c("Ostkoordinat","Nordkoordinat")],data=tax_lav.data,proj4string=CRS(projSWEREF))
# #proj4string(hygge_new)<-projSWEREF
# #tax_lav.data$avDatum<-over(tax_lav.sp,hygge_new)$Avvdatum
#

b10<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 




tax_lav.data$b2<-extract(b2,tax_lav.sp)
tax_lav.data$b3<-extract(b3,tax_lav.sp)
tax_lav.data$b4<-extract(b4,tax_lav.sp)
tax_lav.data$b5<-extract(b5,tax_lav.sp)
tax_lav.data$b6<-extract(b6,tax_lav.sp)
tax_lav.data$b7<-extract(b7,tax_lav.sp)
tax_lav.data$b8<-extract(b8,tax_lav.sp)
tax_lav.data$b9<-extract(b9,tax_lav.sp)
tax_lav.data$b10<-extract(b10,tax_lav.sp)
tax_lav.data$b11<-extract(b11,tax_lav.sp)
tax_lav.data$b12<-extract(b12,tax_lav.sp)
tax_lav.data$ndvi<-extract(ndvi,tax_lav.sp)
tax_lav.data$ndci<-extract(ndci,tax_lav.sp)
tax_lav.data$savi<-extract(savi,tax_lav.sp)
tax_lav.data$soil<-extract(soil,tax_lav.sp)
tax_lav.data$over1_5<-extract(over1_5,tax_lav.sp)
tax_lav.data$trad_h<-extract(trad_h,tax_lav.sp)



jordart<-raster("//YKSI/13_Geodata/Jordart/JordartSWEREF99/jordart.tif")
#
tax_lav.data$jordart<-extract(jordart,tax_lav.sp)
tax_lav.data<-subset(tax_lav.data,!is.na(b2))
saveRDS(tax_lav.data,file="tax_lav_map_vintergrasing.rds")








