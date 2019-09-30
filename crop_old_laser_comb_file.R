library(raster)
library(rgdal)
rasterOptions(tmpdir="F:/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"

t1<-readOGR("F:/Lavproject2019/RBP-lichen_projct_2019","varvinter")
e1<-extent(t1)
#M:\reindder_lichen_map\raster_files_combined
b1<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")
e1<-extent(b1)

(7619740-6776420)/2
e1<-extent(c(338670,942260,6776420,6776420+ 421660))
e2<-extent(c(338670,942260,6776420+ 421660,7619740))


#q95<-raster("F:/Lavproject2019/laser_old/p95/p95.tif")
#q95.e<-crop(q95,e1)
#plot(q95.e)
#writeRaster(q95.e, filename="F:/Lavproject2019/laser_old/p95_vinter.tif", format="GTiff", overwrite=TRUE)


veg.e<-crop(veg,e1)
b1.e<-crop(b1,e1)
plot(veg.e)
veg.e.res2<-resample(veg.e,b1.e,method="bilinear")
writeRaster(veg.e.res2, filename="F:/Lavproject2019/laser_old/veg_vinter_1.tif", format="GTiff", overwrite=TRUE)

veg.e<-crop(veg,e2)
b1.e<-crop(b1,e2)
veg.e.res<-resample(veg.e,b1,method="bilinear")
writeRaster(veg.e.res, filename="F:/Lavproject2019/laser_old/veg_vinter_0.tif", format="GTiff", overwrite=TRUE)



q95.e<-crop(q95,e1)
b1.e<-crop(b1,e1)
plot(q95.e)
q95.e.res2<-resample(q95.e,b1.e,method="bilinear")
plot(q95.e.res)
writeRaster(q95.e.res2, filename="F:/Lavproject2019/laser_old/p95_vinter_10_10_1.tif", format="GTiff", overwrite=TRUE)

q95.e<-crop(q95,e2)
b1.e<-crop(b1,e2)
q95.e.res<-resample(q95.e,b1,method="bilinear")
writeRaster(q95.e.res, filename="F:/Lavproject2019/laser_old/p95_vinter_10_10_0.tif", format="GTiff", overwrite=TRUE)




 
t1<-raster("F:/Lavproject2019/laser_old/p95_vinter_10_10_0.tif")
t2<-raster("F:/Lavproject2019/laser_old/p95_vinter_10_10_1.tif")

res<-merge(t1,t2)
proj4string(res)<-projSWEREF
writeRaster(res, filename="F:/Lavproject2019/laser_old/p95_vinter.tif", format="GTiff", overwrite=TRUE)


t1<-raster("F:/Lavproject2019/laser_old/veg_vinter_0.tif")
t2<-raster("F:/Lavproject2019/laser_old/veg_vinter_1.tif")

res<-merge(t1,t2)
proj4string(res)<-projSWEREF
writeRaster(res, filename="F:/Lavproject2019/laser_old/veg_vinter.tif", format="GTiff", overwrite=TRUE)


