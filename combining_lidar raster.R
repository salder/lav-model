library(raster)
library(rgdal)
rasterOptions(tmpdir="//YKSI/13_Geodata/temp_raster/")

layer_names<-c("las_mean","las_sd","las_cover_1_5","las95","altitude" )

file_list<-dir("//YKSI/13_Geodata/lid_temp/")
file_list<-paste("//YKSI/13_Geodata/lid_temp/",file_list,sep="")

#2604   needed to change!
start<-1
stop<-2696

file_list<-file_list[start:stop]


for (m in 1:5)
{
  
  k1<-readRDS(file_list[1])   
  k2<-readRDS(file_list[2])  
  k1<-round(k1[[m]],3)
  k2<-round(k2[[m]],3)
  
  test_m<-list(k1,k2)
  
  for (i in 3:length(file_list))
  {
    k3<-readRDS(file_list[i])
    k3<-round(k3[[m]],3)
    test_m<-append(test_m,k3)
  }
  
  
  test_m$filename <-"L:/MAS/try.tif"
  test_m$overwrite <- TRUE
  mm <- do.call(merge, test_m)
  projection(mm)<-projSWEREF
  
  nam<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/",layer_names[m],"_merge_total_",start,"_",stop,sep="")
  nam.tif<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/",layer_names[m],"_merge_total_tif_",start,"_",stop,sep="")
  writeRaster(mm,file=nam,overwrite=T)#,datatype="INT2S")
  writeRaster(mm, filename=nam.tif, format="GTiff", overwrite=TRUE)
  
  
}
  