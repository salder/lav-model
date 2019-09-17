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
t1<-spTransform(t1,CRS=projSWEREF)
sameby<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas betesområden","IRENMARK_DBO_sameby")
sameby<-spTransform(sameby,CRS=projSWEREF)
#combine the polygones

head(sameby@data)
sameby@data$NAMN

sameby.deltagare<-c("MittÃ¥dalen", #ok
                    "HandÃ¶lsdalen", #ok
                    "TÃ¥ssÃ¥sen", #ok
                    "Jijnjevaerie", 
                    "Idre", #ok
                    "Ohredahke", #ok
                    "Vilhelmina norra", #ok
                    "Ruvhten sijte", #de var inte med i Sveg
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




sameby.name<-c("Mittadalen",          #1
               "Mittadalen",          #2
               "Tassasen",            #3
               "Jijnjevaerie",        #4
               "Idre",                #5
               "Ohredahke",           #6
               "Vilhelmina_norra",    #7
               "Ruvhten_sijte",       #8
               "Maskaure",            #9            #Jokkmokk
               "Mala",                #10
               "Semisjaur_Njarg",     #11
               "Angesa",              #12
               "Tarendo",             #13
               "Sattajarvi",          #14
               "Gallivare",           #15
               "Jahkagaska_tjiellde", #16
               "Korju",               #17
               "Tuorpon",             #18
               "Konkama",             #19
               "Luokta-Mavas",        #20
               "Sirges",              #21
               "Muonio",              #22
               "Baste cearru",        #23
               "Saarivuoma",          #24
               "Gran")                #25

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


#extra tillägg till RenGIS innformation

#ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Ohredahke/Ohredahke.20190516.091129","Ohredahke bete")
#ohre_bete <- spTransform(ohre_bete, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Ohredake_Vinterbete")
mittadalen.area.for.lav<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","mittadalen_lavinventering_anja")
handalsdalen<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Handalsdalen_Vinterbete")
sirges<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","sirges_Vinterbete_corrected")
sirges<-spTransform(sirges,CRS=projSWEREF)
tuorpons<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","tuorpons_vinterbete_new")
tuorpons<-spTransform(tuorpons,CRS=projSWEREF)
nmd<-raster("D:/UMEA/NMD-nya SMD/NMD/nmd2018bas_ogeneraliserad_v1_0.tif")

korju<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Korju_vinter_update")
korju<-spTransform(korju,CRS=projSWEREF)
muonio<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Muonio_vinterbete_update")
muonio<-spTransform(muonio,CRS=projSWEREF)
#Jahkagaska Tjiellde merge beteslandindelning!

jahka1<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde atgard")
jahka2<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde bete")
jahka3<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde karn")
jahka4<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde lagutn")
jahka5<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Jahkagaska Tjiellde","Jahkagaska Tjiellde nyckel")

t1_jahka<-bind(jahka1,jahka2,jahka3,jahka4,jahka5)



e_mitt<-extent(c(385984.9 ,448488.6,6870295,6925445))
e_hand<-extent(c(389990.9 ,483724.7, 6857244,6967488))
e_toss<-extent(c(405201.9,500236.7,6875682,6975056))
e_jijin<-extent(c(445821.7,653488,6889579,7149937))
e_idre<-extent(c(346611.8,437680.8,6814134 ,6890618))
e_ohre<-extent(c(487039.2 ,628809.1, 6932550,7111450))
e_vil.norr<-extent(c(605985.4,730699.8,7002191,7127639))
e_ruv<-NA
e_mask<-extent(c(714013.7,825326.4,7138222,7225436))
e_mala<-extent(c(621034.7,807957.9,7122773,7286090))
e_semi<-extent(c(730468.8,806924.6,7203201,7316929))
e_anges<-extent(c( 807654.4,849444.4,7365560,7429720))
e_taren<-extent(c(793474.9,850586.3,7427071,7501031))
e_sattaj<-extent(c(813374,888677.8 ,7442014,7500965))
e_galliv<-extent(c(721328.8 ,861580.9,7305555,7460853 ))
e_jahka<-extent(c(618910.7,801196.8 ,7306712,7442952))
e_korju<-extent(c(841505.9,894003.5,7378307,7463467))
e_tuorpon<-extent(c(615935.7,849349.8,7257338,7423976 ))
e_kanka<-NA
e_luokta<-extent(c(626227.7,793875.4,7276367,7379750))
e_sirges<-extent(c(655442.7,852279.8,7250623,7457384))
e_muoni<-extent(c(819974.6,859695.1,7517150,7560684 ))
e_baste<-extent(c(709967.2,815457.6,7425960 ,7503951))
e_saar<-NA
e_gran<-extent(c(600211,791409.3 ,7103866,7292041))









  
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






n.sb<-15

#for (n.sb in c(1:6))
#{
sameby.deltagare[n.sb]
sb<-subset(sameby,NAMN==sameby.deltagare[n.sb])
sb_vinterbete<-gIntersection(sb,t1)

plot(sb)
plot(sb_vinterbete,add=T,col=2)


if (n.sb==6)  #ohredake
         {sb_vinterbete<-gIntersection(sb,ohre_bete)}
 
if (n.sb==1)  #MittÃ¥dalen
         {sb_vinterbete<-gIntersection(sb,mittadalen.area.for.lav)}

if (n.sb==2)  #Handalsdalen
         {sb_vinterbete<-gIntersection(sb,handalsdalen)}

if (n.sb==4)  #Jijnjevaerie
         {sb_vinterbete<-gDifference(sb_vinterbete,ohre_bete)}

if (n.sb==14)
{sb_vinterbete<-sb}

if (n.sb==21)  #Sirges Sameby
{sb_vinterbete<-(gDifference(subset(sirges,id==1),subset(sirges,id==2)))}

 
if (n.sb==18)  #Tuorpons
{sb_vinterbete<-gIntersection(sb,tuorpons)}


if (n.sb==17)  #Korju
{sb_vinterbete<-gIntersection(sb,korju)}


if (n.sb==22)  #Korju
{sb_vinterbete<-gIntersection(sb,muonio)}


plot(sb)
plot(sb_vinterbete,add=T,col=3)

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
ex.vinter<-extent(lav.vinter)
nmd.vinter<-crop(nmd,ex.vinter)

pred<-getValues(lav.vinter)
xy<-xyFromCell(lav.vinter,1:ncell(lav.vinter))

X<-data.frame(xy,pred)
X<-X%>%filter(!is.na(pred))
dim(X)
k<-sample(c(1:length(X[,1])),10000) #set to 10000!!
X1<-X[k,]
sd_ej<-m_ej<-max_ej<-NA

if (n.sb==22)
  X1<-X1[-8873,]
#fist sample av sample squares
for (j in c(1:dim(X1)[1]))
{
  ej<-extent(c(X1$x[j]-500, X1$x[j]+500, X1$y[j]-500, X1$y[j]+500))
  crop_ej<-crop(lav.vinter,ej)
  roads_sel<-crop(roads,ej)
  nmd_sel<-crop(nmd.vinter,ej)
  nmd.val<-getValues(nmd_sel)
  lav.val<-getValues(crop_ej)
  lav.val<-ifelse(nmd.val==2,NA,lav.val)
  crop_ej<-setValues(crop_ej,lav.val)
  
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

X1<-readRDS(paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_first_selection_data.rds",sep=""))

# #fi n.sb==15
# X1.sp<-SpatialPointsDataFrame(coords=X1[,c("x","y")],data=X1,proj4string=CRS(projSWEREF))
# x2.sp<-intersect(X1.sp,sb_vinterbete)
# X1<-x2.sp@data


library(BalancedSampling)

set.seed(12341386)
hist(X1$m_ej)
X1.save<-X1
#X1.save->X1
X1<-subset(X1,m_ej>0.5)
N<-dim(X1)[1]
n_r=10
p = rep(n_r/N,N)
X1.m<-as.matrix(X1)
s = lpm1(p,X1.m)

#plot(X1[,1],X1[,2]); # plot population

plot(sb)
plot(sb_vinterbete,add=T,col=3)
points(X1[s,1],X1[s,2], pch=19,col=2); # plot sample
#points(X1[,1],X1[,2], pch=19,col=2)




#second sample av sample plots


X1_selected<-X1[s,]
X_slut<-NULL
j=1
for (j in c(1:n_r))
{
  ej<-extent(c(X1_selected$x[j]-500, X1_selected$x[j]+500, X1_selected$y[j]-500, X1_selected$y[j]+500))
  crop_sel<-crop(lav.vinter,ej)
  roads_sel<-crop(roads,ej)
  nmd_sel<-crop(nmd.vinter,ej)
  nmd.val<-getValues(nmd_sel)
  lav.val<-getValues(crop_sel)
  lav.val<-ifelse(nmd.val==2,NA,lav.val)
  crop_sel<-setValues(crop_sel,lav.val)
  if (is.null(roads_sel)==FALSE)
    crop_sel<-mask(crop_sel,buffer(roads_sel,25),inverse=TRUE)
  val_sel<-round(getValues(crop_sel),4)
  xy<-xyFromCell(crop_sel,1:ncell(crop_sel))
  X_sel<-data.frame(xy,val_sel)
  
  X_sel<-X_sel%>%filter(val_sel>10)   # kan diskuteras
  
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
#}






xy<-data.frame(x=X_slut$x_WGS84,y=X_slut$yWGS84,a=X_slut$id)
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_gpxTEST.gpx",sep=""),
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)





X_slut<-readRDS("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/Tassasen.rds")
xy<-data.frame(x=X_slut$x_WGS84,y=X_slut$yWGS84,extention=X_slut$id)
latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/","Tassasen","_gpxTEST.gpx",sep=""),
         dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)




#############################################################################################################
#############################################################################################################
#Feedback från SB 16

X_slut<-readRDS(X_slut,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".rds",sep=""))

if (n.sb==16)
{
  n.sb16<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Njal_och_Sudok")
  n.sb16<-spTransform(n.sb16,CRS=projSWEREF)
  nya_rutor<-data.frame(coordinates(n.sb16))
  names(nya_rutor)<-c("x","y")
  X_slut.1<-NULL
  for (j in c(1:2))
  {
    ej<-extent(c(nya_rutor$x[j]-500, nya_rutor$x[j]+500, nya_rutor$y[j]-500, nya_rutor$y[j]+500))
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
    
    X_sel$ruta<-10+j
    X_ruta<-X_sel[s,]
    X_slut.1<-rbind(X_slut.1,X_ruta)
  }
  X_slut.1$py<-c(1:length(X_slut.1$ruta))
  X_slut.1
  X_slut.1$id<-paste(X_slut.1$ruta,X_slut.1$py,sep="_")
  X_slut.1.sp<-SpatialPointsDataFrame(coords=X_slut.1[,c("x","y")],data=X_slut.1,proj4string=CRS(projSWEREF))
  writeOGR(obj=X_slut.1.sp, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],".1",sep=""), driver="ESRI Shapefile")
  writeOGR(sb, dsn="D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar", layer=paste(sameby.name[n.sb],"_border"), driver="ESRI Shapefile")
  
  
  X_slut.1.sprt99<-spTransform(X_slut.1.sp,CRS("+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"))
  X_slut.1$x_rt90<-coordinates(X_slut.1.sprt99)[,1]
  X_slut.1$y_rt90<-coordinates(X_slut.1.sprt99)[,2]
  
  dat1.sp<-spTransform(X_slut.1.sp,CRS("+init=epsg:2400"))
  dat2.sp<-spTransform(dat1.sp,CRS("+init=epsg:4326"))
  
  
  X_slut.1$x_WGS84<-coordinates(dat2.sp)[,1]
  X_slut.1$yWGS84<-coordinates(dat2.sp)[,2]
  
  saveRDS(X_slut.1,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".rds",sep=""))
  write.csv(X_slut.1,paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],".csv",sep=""))
  View(X_slut.1)
  X_slut.1$id<-paste(X_slut.1$ruta,X_slut.1$py,sep="_")
  xy<-data.frame(x=X_slut.1$x_WGS84,y=X_slut.1$yWGS84,a=X_slut.1$id)
  latslongs <- SpatialPointsDataFrame(coords=xy[,c(1,2)],data=xy,proj4string =CRS("+proj=longlat + ellps=WGS84")) 
  writeOGR(latslongs, dsn=paste("D:/UMEA/Renbruksplan/Lavprojekt_2019/till_samebyar/",sameby.name[n.sb],"_add_gpxTEST.gpx",sep=""),
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  
}

#############################################################################################################
#############################################################################################################
#Feedback från SB 16
