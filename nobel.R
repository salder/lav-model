library(raster)
library(rgdal)



lav1_4<-raster("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav_model_south_classes1_4.tif")

testarea<-readOGR("D:/UMEA/Renbruksplan/Nobel/Vardostb","Vardostb")


plot(lav1_4)
plot(testarea,add=T)

e<-extent(testarea)
lav1_4_test<-crop(lav1_4,e)
writeRaster(lav1_4_test, filename="D:/UMEA/Renbruksplan/Lavprojekt_2019/nobel_test_area.tif", format="GTiff", overwrite=TRUE)


