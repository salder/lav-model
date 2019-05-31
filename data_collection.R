#initial: 22/3/2019
#Sven Adler sven.adler@slu.se


#data collection for lichen model building for the hole winter herding area of
#the swedish same people


library(rgdal)
library(raster)
library(mgcv)
library(dplyr)
library(mapview)
library(rgeos)
rasterOptions(tmpdir="D:/UMEA/Renbruksplan/Lavprojekt_2019/temp_files/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"
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
writeOGR(obj=t1.sp,dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/lav model1", layer="varvinter", driver="ESRI Shapefile")

#roads<-readOGR("D:/UMEA/Renbruksplan/fastighetskartan","vl_riks") #tar tid, sparas som rds men tar bort efter projektet är färdig och måste skapas på nyt därefter
#saveRDS(roads,"D:/UMEA/Renbruksplan/roads.rds")




#########################################################################################
#design
#
#
#########################################################################################


lav.model<-raster("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav_model_south.tif")
roads_all<-readRDS("D:/UMEA/Renbruksplan/roads.rds")

t1<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav model1","varvinter")
sameby<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas betesområden","IRENMARK_DBO_sameby")
#combine the polygones

head(sameby@data)
sameby@data$NAMN

sameby.deltagare<-c("MittÃ¥dalen",
                    "HandÃ¶lsdalen",
                    "TÃ¥ssÃ¥sen",
                    "Jijnjevaerie",
                    "Idre",
                    "Ohredahke",
                    "Vilhelmina norra",
                    "Ruvhten sijte",
                    "Maskaure",               #Jokkmokk
                    "MalÃ¥",
                    "Semisjaur-Njarg",
                    "Ã„ngesÃ¥",
                    "TÃ¤rendÃ¶",
                    "SattajÃ¤rvi",
                    "GÃ¤llivare",
                    "JÃ¥hkÃ¥gaska tjiellde",
                    "Korju",
                    "Tuorpon",
                    "KÃ¶nkÃ¤mÃ¤",
                    "Luokta-MÃ¡vas",
                    "Sirges",
                    "Muonio",
                    "Baste cearru",
                    "Saarivuoma",
                    "Gran")




sameby.name<-c("Mittadalen",
               "Mittadalen",
               "Tassasen",
               "Jijnjevaerie",
               "Idre",
               "Ohredahke",
               "Vilhelmina_norra",
               "Ruvhten_sijte",
               "Maskaure",                       #Jokkmokk
               "Mala",
               "Semisjaur_Njarg",
               "Angesa",
               "Tarendo",
               "Sattajarvi",
               "Gallivare",
               "Jahkagaska_tjiellde",
               "Korju",
               "Tuorpon",
               "Konkama",
               "Luokta-Mavas",
               "Sirges",
               "Muonio",
               "Baste cearru",
               "Saarivuoma",
               "Gran")

# #Jockmock
# Sörkaitum 
# Ängeså
# Pirttijärvi
# Jåhkågasska
# Korju
# Maskaure
# Vittangi 
# Kalix
# Girjas
# Gällivare 
# Tärendö
# Saarivuoma
# Luokta Mavas 

# #Sveg
# Mittadalen
# Mittadalen
# Tassasen
# Jijnjevaerie
# Idre
# Ohredahke
# Vilhelmina_norra
# Ruvhten_sijte



ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Ohredahke/Ohredahke.20190516.091129","Ohredahke bete")
ohre_bete <- spTransform(ohre_bete, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
mittadalen.area.for.lav<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","mittadalen_lavinventering_anja")



e_mitt<-extent(c(385984.9 ,448488.6,6870295,6925445))
e_hand<-extent(c(399843.6,485588.8,6856712,6978140))
e_toss<-extent(c(405201.9,500236.7,6875682,6975056))
e_jijin<-extent(c(445821.7,653488,6889579,7149937))
e_idre<-extent(c(346611.8,437680.8,6814134 ,6890618))
e_ohre<-extent(c(486617.3,656234.9, 6929175,7076852))
e_vil.norr<-extent(c(605985.4,730699.8,7002191,7127639))
e_ruv<-NA
  
e1<-list(e_mitt,e_hand,e_toss,e_jijin,e_idre,e_ohre,e_vil.norr,e_ruv)






n.sb<-25

#for (n.sb in c(1:6))
{
sb<-subset(sameby,NAMN==sameby.deltagare[n.sb])
sb_vinterbete<-gIntersection(sb,t1)
#mapview(sb)+mapview(varvinter,col.regions ="red")

plot(sb)
#plot(lav.model,add=T)
plot(sb_vinterbete,add=T,col=2)
if (n.sb==6)
{  
  sb_vinterbete<-gIntersection(sb,ohre_bete)}
 
if (n.sb==1)
{  
  sb_vinterbete<-gIntersection(sb,mittadalen.area.for.lav)
  #writeOGR(sb_vinterbete,"D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar",layer="Mittadalen_urval", driver="ESRI Shapefile")
  }

writeOGR(sb, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border"), driver="ESRI Shapefile")
p.df <- data.frame( ID=1:length(sb_vinterbete)) 
rownames(p.df)
p <- SpatialPolygonsDataFrame(sb_vinterbete, p.df)
writeOGR(p, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border_vinter"), driver="ESRI Shapefile")


 
plot(e1[[n.sb]],add=T)


roads<-crop(roads_all,e1[[n.sb]])
lav.sb<-crop(lav.model,extent(sb))

lav.vinter<-mask(lav.sb,sb_vinterbete)
lav.vinter<-crop(lav.vinter,e1[[n.sb]])


pred<-getValues(lav.vinter)
xy<-xyFromCell(lav.vinter,1:ncell(lav.vinter))

X<-data.frame(xy,pred)
X<-X%>%filter(!is.na(pred))
k<-sample(c(1:length(X[,1])),10000) #set to 10000!!
X1<-X[k,]
sd_ej<-m_ej<-max_ej<-NA
j=1

#fist sample av sample squares
for (j in c(1:dim(X1)[1]))
{
  ej<-extent(c(X1$x[j]-500, X1$x[j]+500, X1$y[j]-500, X1$y[j]+500))
  crop_ej<-crop(lav.vinter,ej)
  roads_sel<-crop(roads,ej)
  #jarnvag_sel<-crop(jarnvag,ej)
  if (is.null(roads_sel)==FALSE)
    crop_ej<-mask(crop_ej,buffer(roads_sel,15),inverse=TRUE)
  #if (is.null(jarnvag_sel)==FALSE)
  #  crop_ej<-mask(crop_ej,buffer(jarnvag_sel,12),inverse=TRUE)
  plot(crop_ej,main=j)
  val_ej<-getValues(crop_ej)
  sd_ej[j]<-sd(val_ej,na.rm=T)
  m_ej[j]<-mean(val_ej,na.rm=T)
  max_ej[j]<-max(val_ej,na.rm=T)
}


X1$m_ej<-m_ej
X1$sd_ej<-sd_ej
X1$max_ej<-max_ej

saveRDS(X1,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_first_selection_data.rds",sep=""))



library(BalancedSampling)

set.seed(12341386)
hist(X1$m_ej)
X1.save<-X1
#X1.save->X1
X1<-subset(X1,m_ej>6)
N<-dim(X1)[1]
n_r=10
p = rep(n_r/N,N)
X1.m<-as.matrix(X1)
s = lpm1(p,X1.m)

plot(X1[,1],X1[,2]); # plot population
points(X1[s,1],X1[s,2], pch=19,col=3); # plot sample





#second sample av sample plots


X1_selected<-X1[s,]
X_slut<-NULL
j=1
for (j in c(1:n_r))
{
  ej<-extent(c(X1_selected$x[j]-500, X1_selected$x[j]+500, X1_selected$y[j]-500, X1_selected$y[j]+500))
  crop_sel<-crop(lav.vinter,ej)
  roads_sel<-crop(roads,ej)
  if (is.null(roads_sel)==FALSE)
    crop_sel<-mask(crop_sel,buffer(roads_sel,25),inverse=TRUE)
  val_sel<-round(getValues(crop_sel),4)
  xy<-xyFromCell(crop_sel,1:ncell(crop_sel))
  X_sel<-data.frame(xy,val_sel)
  
  X_sel<-X_sel%>%filter(val_sel>5)   # kan diskuteras
  
  N<-dim(X_sel)[1]
  n=8
  p = rep(n/N,N)
  X_sel.m<-as.matrix(X_sel)
  s = lpm1(p,X_sel.m)
  
  plot(crop_sel) # plot population
  points(X_sel[s,1],X_sel[s,2], pch=19,col=2); # plot sample
  mapview(crop_sel)
  
  X_sel$ruta<-j
  X_ruta<-X_sel[s,]
  X_slut<-rbind(X_slut,X_ruta)
}
X_slut$py<-c(1:length(X_slut$ruta))
X_slut$id<-paste(X_slut$ruta,"_",X_slut$py,sep="")
View(X_slut)
saveRDS(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".rds",sep=""))



X_slut.sp<-SpatialPointsDataFrame(coords=X_slut[,c("x","y")],data=X_slut,proj4string=CRS(projSWEREF))
writeOGR(obj=X_slut.sp, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=sameby.name[n.sb], driver="ESRI Shapefile")
writeOGR(sb, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border"), driver="ESRI Shapefile")


X_slut.sprt99<-spTransform(X_slut.sp,CRS("+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"))
X_slut$x_rt90<-coordinates(X_slut.sprt99)[,1]
X_slut$y_rt90<-coordinates(X_slut.sprt99)[,2]

dat1.sp<-spTransform(X_slut.sp,CRS("+init=epsg:2400"))
dat2.sp<-spTransform(dat1.sp,CRS("+init=epsg:4326"))


X_slut$x_WGS84<-coordinates(dat2.sp)[,1]
X_slut$yWGS84<-coordinates(dat2.sp)[,2]

saveRDS(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".rds",sep=""))
write.csv(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".csv",sep=""))
View(X_slut)
}






xy<-data.frame(x=X_slut$x_WGS84,y=X_slut$yWGS84,a=X_slut$id)
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_gpxTEST.gpx",sep=""),
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)





X_slut<-readRDS("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/Tassasen.rds")
xy<-data.frame(x=X_slut$x_WGS84,y=X_slut$yWGS84,extention=X_slut$id)
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/","Tassasen","_gpxTEST.gpx",sep=""),
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)







mapview(sb,color="red",col.regions ="white",lwd=2,alpha.regions = 0.1)+mapview(lav.vinter)
mapview(lav.vinter)





karta<-raster("D:/UMEA/Renbruksplan/sr_1302_generalmap.tif")
karta.c<-crop(karta,(extent(X_slut.sp)+10000))
m <- mapview(X_slut.sp)
mapshot(m, file = "D:/UMEA/Renbruksplan/Lavprojekt_2019/Mittadalen_overview_test.jpg",
        remove_controls = c("homeButton", "layersControl"))
m <- mapview(subset(X_slut.sp,ruta==1))+mapview(crop(karta,(extent(subset(X_slut.sp,ruta==1))+1000)))
mapshot(m, file = "D:/UMEA/Renbruksplan/Lavprojekt_2019/Mittadalen_overview_test_ruta_1.jpg",
        remove_controls = c("homeButton", "layersControl"))


m <- mapview(X_slut.sp)
m


