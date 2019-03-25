#initial: 22/3/2019
#Sven Adler sven.adler@slu.se


#data collection for lichen model building for the hole winter herding area of
#the swedish same people


library(rgdal)
library(raster)
library(mgcv)
library(dplyr)
library(mapview)


#get vinter gazing areas
vinter<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas markanvändningsredovisning/Årstidsland","IRENMARK_DBO_vinter")
#get spring vinter gazing areas
varvinter<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas markanvändningsredovisning/Årstidsland","IRENMARK_DBO_varvinter")
#combine the polygones
t1<-merge(vinter,varvinter)
#merge all reindeer herding districts to one polygone
t1.b<-buffer(t1,width=2000)
mapview(t1.b)

p.df <- data.frame( ID=1:length(t1.b)) 
pid <- sapply(slot(t1.b, "polygons"), function(x) slot(x, "ID")) 
p.df <- data.frame( ID=1:length(t1.b), row.names = pid)    

# Try coersion again and check class
t1.sp <- SpatialPolygonsDataFrame(t1.b, p.df)
writeOGR(obj=t1.sp,dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/lav model", layer="varvinter", driver="ESRI Shapefile")


