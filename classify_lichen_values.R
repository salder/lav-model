library(terra)
library(dplyr)

lav<-rast("L:/lichen_map_2020/lichen_map_swe2020_v1_1.tif")

lav.val<-spatSample(lav,size=10000000)
lav.val1<-na.omit(lav.val$lichen_map_swe2020)
lav.class<-ifelse(lav.val1>10,1,0)
prop.table(table(lav.class))
