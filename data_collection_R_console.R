
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


lav.model<-raster("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav_model_south.tif")
roads_all<-readRDS("D:/UMEA/Renbruksplan/roads.rds")

t1<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav model1","varvinter")
sameby<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas betesomr?den","IRENMARK_DBO_sameby")
#combine the polygones

head(sameby@data)
sameby@data$NAMN

sameby.deltagare<-c("Mittådalen", #ok
                    "Handölsdalen", #ok
                    "Tåssåsen", #ok
                    "Jijnjevaerie", 
                    "Idre", #ok
                    "Ohredahke", #ok
                    "Vilhelmina norra", #ok
                    "Ruvhten sijte", #de var inte med i Sveg
                    "Maskaure",               #Jokkmokk
                    "Malå",
                    "Semisjaur-Njarg",
                    "Ängeså",
                    "Tärendö",
                    "Sattajärvi",
                    "Gällivare",
                    "Jåhkågaska tjiellde",
                    "Korju",
                    "Tuorpon",
                    "Könkämä",
                    "Luokta-Mávas",
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
# S?rkaitum 
# ?nges?
# Pirttij?rvi
# J?hk?gasska
# Korju
# Maskaure
# Vittangi 
# Kalix
# Girjas
# G?llivare 
# T?rend?
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



#ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Ohredahke/Ohredahke.20190516.091129","Ohredahke bete")
#ohre_bete <- spTransform(ohre_bete, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Ohredake_Vinterbete")
mittadalen.area.for.lav<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","mittadalen_lavinventering_anja")
handalsdalen<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Handalsdalen_Vinterbete")
sirges<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","sirges_Vinterbete_corrected")



#ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Ohredahke/Ohredahke.20190516.091129","Ohredahke bete")
#ohre_bete <- spTransform(ohre_bete, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Ohredake_Vinterbete")
mittadalen.area.for.lav<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","mittadalen_lavinventering_anja")
handalsdalen<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Handalsdalen_Vinterbete")
sirges<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","sirges_Vinterbete_corrected")
sirges<-spTransform(sirges,CRS=projSWEREF)
tuorpons<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","tuorpons_vinterbete_update")
tuorpons<-spTransform(tuorpons,CRS=projSWEREF)

#Jahkagaska Tjiellde merge beteslandindelning!

jahka1<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde atgard")
jahka2<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde bete")
jahka3<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde karn")
jahka4<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde lagutn")
jahka5<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde nyckel")

t1<-bind(jahka1,jahka2,jahka3,jahka4,jahka5)






e_mitt<-extent(c(385984.9 ,448488.6,6870295,6925445))
e_hand<-extent(c(389990.9 ,483724.7, 6857244,6967488))
e_toss<-extent(c(405201.9,500236.7,6875682,6975056))
e_jijin<-extent(c(445821.7,653488,6889579,7149937))
e_idre<-extent(c(346611.8,437680.8,6814134 ,6890618))
e_ohre<-extent(c(487039.2 ,628809.1, 6932550,7111450))
e_vil.norr<-extent(c(605985.4,730699.8,7002191,7127639))
e_ruv<-NA
e_mask<-extent(c(714013.7,825326.4,7138222,7225436))
e_mala<-NA
e_semi<-extent(c(730468.8,806924.6,7203201,7316929))
e_anges<-NA
e_taren<-NA
e_sattaj<-extent(c(813374,888677.8 ,7442014,7500965))
e_galliv<-NA
e_jahka<-extent(c(618910.7,801196.8 ,7306712,7442952))
e_korju<-NA
e_tuorpon<-NA
e_kanka<-NA
e_luokta<-NA
e_sirges<-NA
e_muoni<-NA
e_baste<-NA
e_saar<-NA
e_gran<-NA









  
e1<-list(e_mitt,
         e_hand,
         e_toss,
         e_jijin,
         e_idre,
         e_ohre,
         e_vil.norr,
         e_ruv,
         e_mask,
         e_mala,
         e_semi,
         e_anges,
         e_taren,
         e_sattaj,
         e_galliv,
         e_jahka,
         e_korju,
         e_tuorpon,
         e_kanka,
         e_luokta,
         e_sirges,
         e_muoni,
         e_baste,
         e_saar,
         e_gran
)



#angeså==12
n.sb<-12

#for (n.sb in c(1:6))
#{
sb<-subset(sameby,NAMN== sameby.name[n.sb])
sb<-subset(sameby,NAMN== "Ã„ngesÃ¥")
#kontroll t1!!!!
sb_vinterbete<-gIntersection(sb,t1)
#Ängeså:
if (n.sb==12)
sb_vinterbete<-sb

plot(sb)
plot(sb_vinterbete,add=T,col=2)

if (n.sb==6)  #ohredake
         {sb_vinterbete<-gIntersection(sb,ohre_bete)}
 
if (n.sb==1)  #Mittådalen
         {sb_vinterbete<-gIntersection(sb,mittadalen.area.for.lav)}

if (n.sb==2)  #Handalsdalen
         {sb_vinterbete<-gIntersection(sb,handalsdalen)}

if (n.sb==4)  #Jijnjevaerie
         {sb_vinterbete<-gDifference(sb_vinterbete,ohre_bete)}

if (n.sb==14)
{sb_vinterbete<-sb}
  
if (n.sb==21)  #sirges
         {sb_vinterbete<-gIntersection(sb,sirges)}



plot(sb)
plot(sb_vinterbete,add=T,col=2)

writeOGR(sb, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border"), driver="ESRI Shapefile")
p.df <- data.frame( ID=1:length(sb_vinterbete)) 
rownames(p.df)
p <- SpatialPolygonsDataFrame(sb_vinterbete, p.df)
writeOGR(p, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border_vinter"), driver="ESRI Shapefile")


if (n.sb==12)
  e1[[n.sb]]<-extent(sb)
plot(e1[[n.sb]],add=T)


roads<-crop(roads_all,e1[[n.sb]])
lav.sb<-crop(lav.model,extent(sb))

lav.vinter<-mask(lav.sb,sb_vinterbete)
lav.vinter<-crop(lav.vinter,e1[[n.sb]])


pred<-getValues(lav.vinter)
xy<-xyFromCell(lav.vinter,1:ncell(lav.vinter))

X<-data.frame(xy,pred)
X<-X%>%filter(!is.na(pred))
dim(X)
k<-sample(c(1:length(X[,1])),10000) #set to 10000!!
X1<-X[k,]
sd_ej<-m_ej<-max_ej<-NA

roads.terra<-terra::vect(roads)
lav.vinter.terra<-terra::rast(lav.vinter)
roads.r<-terra::rasterize(roads.terra,lav.vinter.terra)
terra::writeRaster(roads.r,"sb_roads.tif",overwrite=TRUE)
roads.r<-raster("sb_roads.tif")

#fist sample av sample squares
for (j in c(1:dim(X1)[1]))    #8546
{
  ej<-extent(c(X1$x[j]-500, X1$x[j]+500, X1$y[j]-500, X1$y[j]+500))
  ej.1<-extent(c(X1$x[j]-450, X1$x[j]+450, X1$y[j]-450, X1$y[j]+450))
  
  crop_ej<-crop(lav.vinter,ej,warning=FALSE)
  roads_sel.r<-crop(roads.r,ej.1)
  if(length(unique(getValues(roads_sel.r)))>1)
     {                         
  roads_sel<-crop(roads,ej)
  #jarnvag_sel<-crop(jarnvag,ej)
  if (is.null(roads_sel)==FALSE)
    crop_ej<-mask(crop_ej,buffer(roads_sel,15),inverse=TRUE)
    } 
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

saveRDS(X1,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_first_selection_data_1.rds",sep=""))

X1<-readRDS(paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_first_selection_data_1.rds",sep=""))

library(BalancedSampling)

set.seed(12341)
hist(X1$m_ej)
X1.save<-X1
#X1.save->X1
X1<-subset(X1,m_ej>2)
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
  roads_sel.r<-raster::crop(roads.r,ej)
  if(length(unique(getValues(roads_sel.r)))>1)
  {                         
    roads_sel<-crop(roads,ej)
    crop_sel<-mask(crop_sel,buffer(roads_sel,25),inverse=TRUE)
  }
   val_sel<-round(getValues(crop_sel),4)
  xy<-xyFromCell(crop_sel,1:ncell(crop_sel))
  X_sel<-data.frame(xy,val_sel)
  
  X_sel<-X_sel%>%filter(val_sel>15)   # kan diskuteras
  
  N<-dim(X_sel)[1]
  n=8
  p = rep(n/N,N)
  X_sel.m<-as.matrix(X_sel)
  s = lpm1(p,X_sel.m)
  
  plot(crop_sel) # plot population
  points(X_sel[s,1],X_sel[s,2], pch=19,col=2); # plot sample
  #mapview(crop_sel)
  
  X_sel$ruta<-j
  X_ruta<-X_sel[s,]
  X_slut<-rbind(X_slut,X_ruta)
}
X_slut$py<-c(1:length(X_slut$ruta))
X_slut$id<-paste(X_slut$ruta,"_",X_slut$py,sep="")
View(X_slut)
saveRDS(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021/",sameby.name[n.sb],"2021.rds",sep=""))



X_slut.sp<-SpatialPointsDataFrame(coords=X_slut[,c("x","y")],data=X_slut,proj4string=CRS(projSWEREF))
writeOGR(obj=X_slut.sp, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021", layer=sameby.name[n.sb], driver="ESRI Shapefile",overwrite_layer = T)
writeOGR(sb, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021", layer=paste(sameby.name[n.sb],"_border"), driver="ESRI Shapefile", overwrite_layer = T)


X_slut.sprt99<-spTransform(X_slut.sp,CRS("+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"))
X_slut$x_rt90<-coordinates(X_slut.sprt99)[,1]
X_slut$y_rt90<-coordinates(X_slut.sprt99)[,2]

dat1.sp<-spTransform(X_slut.sp,CRS("+init=epsg:2400"))
dat2.sp<-spTransform(dat1.sp,CRS("+init=epsg:4326"))


X_slut$x_WGS84<-coordinates(dat2.sp)[,1]
X_slut$yWGS84<-coordinates(dat2.sp)[,2]

saveRDS(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021/",sameby.name[n.sb],".rds",sep=""))
write.csv(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021/",sameby.name[n.sb],".csv",sep=""))
View(X_slut)
}






xy<-data.frame(x=X_slut$x_WGS84,y=X_slut$yWGS84,name=X_slut$id)
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
latslongs<-subset(latslongs,select=name)
writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar_2021/",sameby.name[n.sb],"_gpx.gpx",sep=""),
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)







