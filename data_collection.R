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




sameby<-readOGR("C:/Users/Public/Documents/RenGIS/iRenMark/LstGIS.2018-02-19/Samebyarnas betesområden","IRENMARK_DBO_sameby")
#combine the polygones

head(sameby@data)
sameby@data$NAMN
sb<-subset(sameby,NAMN=="MittÃ¥dalen")
sb<-subset(sameby,NAMN=="HandÃ¶lsdalen")
sb<-subset(sameby,NAMN=="Vilhelmina norra")
sb_vinterbete<-gIntersection(sb,vinter)
mapview(sb)+mapview(sb_vinterbete,col.regions ="red")

e_mitt<-extent(c(401362.1,459576.7,6859037,6925422))
e_hand<-
e_vil.norr<-
e_
e1<-list(e_mitt)



lav.model<-raster("D:/UMEA/Renbruksplan/Lavprojekt_2019/lav_model_south.tif")
#roads<-readOGR("D:/UMEA/Renbruksplan/fastighetskartan","vl_riks") #tar tid, sparas som rds men tar bort efter projektet är färdig och måste skapas på nyt därefter
#saveRDS(roads,"D:/UMEA/Renbruksplan/roads.rds")
roads<-readDRS("D:/UMEA/Renbruksplan/roads.rds")
plot(lav.model)
plot(sb,add=T)

roads<-crop(roads,e1[[1]])



lav.sb<-crop(lav.model,extent(sb))
sb_vinterbete<-gIntersection(sb,vinter)
lav.vinter<-mask(lav.sb,sb_vinterbete)
lav.vinter<-crop(lav.vinter,e1[[1]])


pred<-getValues(lav.vinter)
xy<-xyFromCell(lav.vinter,1:ncell(lav.vinter))

X<-data.frame(xy,pred)
X<-X%>%filter(!is.na(pred))
k<-sample(c(1:length(X[,1])),100) #set to 10000!!
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
  plot(crop_ej)
  val_ej<-getValues(crop_ej)
  sd_ej[j]<-sd(val_ej,na.rm=T)
  m_ej[j]<-mean(val_ej,na.rm=T)
  max_ej[j]<-max(val_ej,na.rm=T)
}


X1$m_ej<-m_ej
X1$sd_ej<-sd_ej
X1$max_ej<-max_ej

library(BalancedSampling)

set.seed(12341386)
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
  
  X_sel<-X_sel%>%filter(val_sel>0.05)
  
  N<-dim(X_sel)[1]
  n=8
  p = rep(n/N,N)
  X_sel.m<-as.matrix(X_sel)
  s = lpm1(p,X_sel.m)
  
  plot(crop_sel) # plot population
  points(X_sel[s,1],X_sel[s,2], pch=19,col=2); # plot sample
  X_sel$ruta<-j
  X_slut<-rbind(X_slut,X_sel[s,])
}
X_slut$py<-c(1:length(X_slut$ruta))
X_slut$id<-paste(X_slut$ruta,"_",X_slut$py,sep="")
saveRDS(X_slut,"F:/Härjedalen/Geo-Data/X1_slut_Sven.rds")





