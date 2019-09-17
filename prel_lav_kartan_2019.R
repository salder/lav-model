#data collection for lichen map
#
#prel lichen map for designing sampling under 2019

#R packete
library(googledrive)
library(raster)
library(dplyr)
library(rgdal)

rasterOptions(tmpdir="L:/DATA/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"



#1. Satellit daten von drive runterladen und mergen


#satellit images
#just one time
 drive_find()                                  #choose "2"   Einloggen erforderlich ueber Browser
 setwd("M:/reindder_lichen_map/GEE")             #hier werden die Sat daten gespeichert

 t1<-drive_find()                              #finden aller files auf dem drive
 files<-grep("lav_vinterbete",t1$name,value=TRUE)   #nur die Gallivare files auswählen
 View(files)

 #files<-files[c(42:53)]
 for (i in 111:length(files))                    # runterladen der files auf M:/reindder_lichen_map/GEE
   drive_download(file=files[i],overwrite=TRUE)


 scene_list<-dir(path="M:/reindder_lichen_map/GEE",pattern=".tif",full.names = TRUE)



 parameter_list<-c(

    "b2_lav_vinterbete"
   ,"b3_lav_vinterbete"
   ,"b4_lav_vinterbete"
   ,"b5_lav_vinterbete"
   ,"b6_lav_vinterbete"
   ,"b7_lav_vinterbete"
   ,"b8_lav_vinterbete"
   ,"b9_lav_vinterbete"
   ,"b10_lav_vinterbete"
   ,"b11_lav_vinterbete"
   ,"b12_lav_vinterbete"
   ,"ndvi_lav_vinterbete"
   ,"ndwi_lav_vinterbete"
   ,"ndci_lav_vinterbete"
   ,"land_water_lav_vinterbete"
   ,"gndvi_lav_vinterbete"
   ,"sipi_lav_vinterbete"
   ,"soil_lav_vinterbete"
   ,"savi_lav_vinterbete"
   
   #,
 )

 for (k in 1:(length(parameter_list)))
 {

   parameter<-parameter_list[k]
   #parameter="ndwi_m7_8"
   scene_list_red<-grep(parameter,scene_list,value = TRUE)
   file.copy(from=scene_list_red, to="F:/Geo-Data/temp_calc/",overwrite=TRUE)            #schneller
   scene_list_red<-dir(path="F:/Geo-Data/temp_calc/",pattern=".tif",full.names = TRUE)

   k1<-raster(scene_list_red[1])
   k2<-raster(scene_list_red[2])

   test_m<-list(k1,k2)

   if (length(scene_list_red)>2)
   {
     for (i in (3):length(scene_list_red))
     {
       k3<-raster(scene_list_red[i])
       test_m<-append(test_m,k3)
     }
   }

   test_m$filename <-"F:/Geo-Data/try1.tif"
   test_m$overwrite <- TRUE
   mm <- do.call(merge, test_m)
   projection(mm)<-projSWEREF
   file_name<-paste("M:/reindder_lichen_map/raster_files_combined/",parameter,".tif", sep="")
   writeRaster(mm, filename=file_name, format="GTiff", overwrite=TRUE)
   file.remove(scene_list_red)
 }


#satellit image for the Gallivare SB
#just one time
 
 
 
scene_list<-dir(path="M:/reindder_lichen_map/raster_files_combined/",pattern=".tif",full.names = TRUE)

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









 
