#Luokta_Mavas



library(terra)

lavkartan<-rast("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav_model_south.tif")
sb.sample<-vect("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/Luokta-Mavas _border.shp")

lav.luokta<-crop(lavkartan,ext(sb.sample))


writeRaster(lav.luokta,file="D:/UMEA/Renbruksplan/Lavprojekt_2019/Luokta_Mavas/lavkartan_Luokta.tif")
