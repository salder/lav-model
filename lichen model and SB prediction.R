
#lichen map based on NFI data+data collected by reindeer herding districs
#the maps are produced step by step for all districts that were includet in the project (23 in 2019)
#
#the produced maps based on the combination of files data collected by the NFI and the different reindeer herding districts
#with remote sensing data and soil type data
#
#NFI coordinates as grasing areas are secrete and are not uploadet! contact sven.adler@slu.se
#
#the code for producing the satellit images can be found in the file GEE_SAT2018.R
#laser scannings data are open ->landmateriest.se
#all analysis were done in the projection SWEREF99
#Sven Adler 5/10/2013



#used libraries
library(mgcv)
library(raster)
library(usdm)
library(dplyr)
library(rgdal)
library(mapview)
library(maptools)
library(spatial)

#projection and raster options
rasterOptions(tmpdir="F:/temp_raster")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"


######################################################################################################
#function definition

#drop_cont calculates the relative importance of the singele variables within a gam regresssion analysis
drop_cont<-function(form=form,data_use=dat_mod,fam="quasibinomial",method="REML")
{
  var_all<-as.character(attr(terms(form),"variables"))[-1]
  response<-var_all[1]
  predictors<-var_all[-1]
  form0<-as.formula(paste(response,1,sep="~"))
  fit0<-gam(form0,data=data_use,family=fam)#,method=method)
  fit_tot<-gam(form,data=data_use,family=fam)#,method=method)
  d0<-deviance(fit0)
  d_tot<-deviance(fit_tot)
  var_explain<-predictor_explain<-prop_var_explain<-NA
  for (i in 1:length(predictors))
  {
    predictor_sub<-paste(predictors[-i],collapse="+")
    form1<-as.formula(paste(response,predictor_sub,sep="~"))
    fit_red<-gam(form1,data=data_use,family=fam,sp=fit_tot$sp[-i])#,method=method)
    predictor_explain[i]<-predictors[i]
    var_explain[i]<-(deviance(fit_red)-d_tot)/d0
    
    
  }
  prop_var_explain<-round(var_explain/sum(var_explain),3)
  #prop_var_explain<-prop_var_explain[order(prop_var_explain[,3],decreasing = F),]
  res<-data.frame(predictor_explain,var_explain,prop_var_explain)
  #res_a<-res[order(res$prop_var_explain,decreasing=T),]
  res<-list(fit_tot,summary(fit_tot),res)
}
########################################################################################################


#reading in the different winter grasing lands from different reindeer herding district
#updatera länk!
t1<-readOGR("F:/Lavproject2019/RBP-lichen_projct_2019","varvinter")
t1<-spTransform(t1,CRS=projSWEREF)
sameby<-readOGR("F:/Lavproject2019","IRENMARK_DBO_sameby")
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



#extra tillägg till RenGIS innformation

#ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019/Ohredahke/Ohredahke.20190516.091129","Ohredahke bete")
#ohre_bete <- spTransform(ohre_bete, CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
ohre_bete<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Ohredake_Vinterbete")
mittadalen.area.for.lav<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","mittadalen_lavinventering_anja")
#handalsdalen<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","Handalsdalen_Vinterbete")
handalsdalen<-readOGR("M:/reindder_lichen_map","Handalsdalen_Vinterbete")

sirges<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","sirges_Vinterbete_corrected")
sirges<-spTransform(sirges,CRS=projSWEREF)
#tuorpons<-readOGR("D:/UMEA/Renbruksplan/Lavprojekt_2019","tuorpons_vinterbete_new")
tuorpons<-readOGR("M:/reindder_lichen_map","tuorpons_vinterbete_new")
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


      
sb<-subset(sameby,NAMN==sameby.deltagare[3])
tassosen<-rgeos::gIntersection(sb,t1)
plot(tassosen)


sb<-subset(sameby,NAMN==sameby.deltagare[23])
baste<-rgeos::gIntersection(sb,t1)
plot(baste)



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








#model based only on the NFI data
tax_model_data<-readRDS("tax_lav_map_vintergrasing.rds")
  tax_model_data$lav_tackning<-tax_model_data$Tackningsarea/tax_model_data$Provyteareal

predictors<-tax_model_data@data%>%
  select(b2,b3,b4,b5,b6,b7,b8,b9,b11,b12,ndvi,ndci,soil,savi, over1_5,trad_h,jordart)
vif(predictors)
vifcor(predictors,th=0.8)
vifstep(predictors)
# 8 variables from the 17 input variables have collinearity problem: 
#   
#   soil savi b4 b6 b8 b12 b3 b7 
# 
# After excluding the collinear variables, the linear correlation coefficients ranges between: 
#   min correlation ( jordart ~ over1_5 ):  0.007913409 
# max correlation ( b11 ~ b5 ):  0.8936102 
# 
# ---------- VIFs of the remained variables -------- 
#   Variables      VIF
# 1        b2 5.253047
# 2        b5 9.623497
# 3        b9 2.191594
# 4       b11 6.495839
# 5      ndvi 3.090612
# 6      ndci 3.797915
# 7   over1_5 2.068530
# 8    trad_h 1.693302
# 9   jordart 1.021308


#neets to update
#e_reg<-extent(c(338670,942260, 6776420, 7130669  )) #south
#e_reg<-extent(c(338670,942260, 7289755, 7623202  )) #north

tax_model_data.rest<-crop(tax_model_data,e_reg)

form.vif<-as.formula(lav_tackning ~
                   s(b2)
                 +s(b3)
                 +s(b5)
                 +s(b4)
                 +s(b9)
                 +s(b11)
                 +s(ndvi)
                 +s(ndci)
                 +s(over1_5)
                 +s(trad_h)
                 +as.factor(jordart)
                  )

#tax data
fit.gam<-gam(form.vif,data=tax_model_data.rest,"quasibinomial")
summary(fit.gam)
plot(fit.gam,pages=1,scale=F, shade=T,all.terms=T)
gam.check(fit.gam)


dropvar_all<-drop_cont(form.vif,tax_model_data.rest,method="GCV.Cp",fam="quasibinomial")
dropvar_all



#combining with data collected by the reindeer herding districts

#read data from the reindeer herding district



#Tuorpons
# sb_data<-readOGR("F:/Lavproject2019/incommande data/Tuorpons","LavInv112 Tuorpons sameby.20190919.225341")
# sb_data<-spTransform(sb_data,CRS=projSWEREF)
# plot(sb_data)
# mapview(sb_data)

#Baste
# sb_data<-readOGR("F:/Lavproject2019/incommande data/Baste","LavInv108 Baste Cearru.20191002.200157")
# sb_data<-spTransform(sb_data,CRS=projSWEREF)
# plot(sb_data)
# mapview(sb_data)


#Tassosen
# sb_data<-readOGR("F:/Lavproject2019/incommande data/Tassasen","LavInv158 Tassasens sameby.20190906.104044")
# sb_data<-spTransform(sb_data,CRS=projSWEREF)
# plot(sb_data)
# mapview(sb_data)

#Handolsdalen
sb_data<-readOGR("F:/Lavproject2019/incommande data/Handolsdalen","LavInv157 Handolsdalens sameby.20190821.213948")
sb_data<-spTransform(sb_data,CRS=projSWEREF)
plot(sb_data)
mapview(sb_data)


b10.r<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11.r<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12.r<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2.r<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3.r<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4.r<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5.r<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6.r<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7.r<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8.r<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9.r<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi.r<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci.r<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi.r<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi.r<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi.r<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi.r<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil.r<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 
jordart.r<-raster("M:/reindder_lichen_map/jordart.tif")
over1_5.r<-raster("F:/Lavproject2019/laser_old/veg_vinter.tif")
trad_h.r<-raster("F:/Lavproject2019/laser_old/p95_vinter.tif")






sb_data$b2<-extract(b2.r,sb_data)
sb_data$b3<-extract(b3.r,sb_data)
sb_data$b4<-extract(b4.r,sb_data)
sb_data$b5<-extract(b5.r,sb_data)
sb_data$b6<-extract(b6.r,sb_data)
sb_data$b7<-extract(b7.r,sb_data)
sb_data$b8<-extract(b8.r,sb_data)
sb_data$b9<-extract(b9.r,sb_data)
sb_data$b10<-extract(b10.r,sb_data)
sb_data$b11<-extract(b11.r,sb_data)
sb_data$b12<-extract(b12.r,sb_data)
sb_data$ndvi<-extract(ndvi.r,sb_data)
sb_data$ndci<-extract(ndci.r,sb_data)
sb_data$savi<-extract(savi.r,sb_data)
sb_data$soil<-extract(soil.r,sb_data)
sb_data$over1_5<-extract(over1_5.r,sb_data)
sb_data$trad_h<-extract(trad_h.r,sb_data)
sb_data$jordart<-extract(jordart.r,sb_data)

ymin<-extent(sb_data)[3]-110000
ymax<-extent(sb_data)[4]+110000
e_tax<-extent(tax_model_data)
xmin<-e_tax[1]
xmax<-e_tax[2]
e1<-extent(c(xmin,xmax,ymin,ymax))
tax_model_data.e1<-crop(tax_model_data,e1)
plot(tax_model_data)
plot(e1,add=T,col=4)
dim(tax_model_data.e1)

#avverkning<-readOGR("M:/reindder_lichen_map","sksUtfordAvverk")
#avverkning<-spTransform(avverkning,CRS=projSWEREF)
#saveRDS(avverkning,"M:/reindder_lichen_map/avverkning.rds")
avverkning<-readRDS("M:/reindder_lichen_map/avverkning.rds")
avverkning<-crop(avverkning,e1)
t2<-extract(avverkning,tax_model_data.e1)
t2<-t2 %>% select(point.ID,AVVDATUM)
t2<-t2[duplicated(t2)==FALSE,]
tax_model_data.e1$averkning<-t2$AVVDATUM
sb_data$averkning<-extract(avverkning,sb_data)$AVVDATUM



tax_model<-tax_model_data.e1@data %>% 
                      select(b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,ndvi,ndci,savi,soil,over1_5,trad_h,jordart,averkning,lav_tackning)%>% 
                      mutate(trad_h=ifelse(is.na(trad_h),0,trad_h)) %>%
                      mutate(inv=1)
sb_model<-sb_data@data %>% 
              select(b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,ndvi,ndci,savi,soil,over1_5,trad_h,jordart,averkning,MARKLAVAKT) %>% 
              mutate(inv=2) %>%
              rename(lav_tackning=MARKLAVAKT) %>% 
              mutate(trad_h=ifelse(is.na(trad_h),0,trad_h)) %>% 
              mutate(lav_tackning=lav_tackning/100)



model_data<-rbind(sb_model,tax_model)
strsplit(as.character(model_data$averkning),sep="/")

model_data$averkning<-as.numeric(format(as.Date(model_data$averkning, format="%Y/%m/%d"),"%Y"))
model_data$averkning<-ifelse(is.na(model_data$averkning),0,model_data$averkning)
model_data$trad_h<-ifelse(model_data$averkning>2013,0,model_data$trad_h)
model_data$over1_5<-ifelse(model_data$averkning>2013,0,model_data$over1_5)

View(model_data)
predictors<-model_data%>%
  select(b2,b3,b4,b5,b6,b7,b8,b9,b11,b12,ndvi,ndci,soil,savi, over1_5,trad_h,jordart)
vif(predictors)
vifcor(predictors,th=0.8)
vifstep(predictors)





form.vif<-as.formula(lav_tackning ~
                       s(b2)
                     +s(b3)
                     +s(b4)
                     +s(b5)
                     +s(b7)
                     +s(b9)
                     +s(b11)
                     +s(ndvi)
                     +s(ndci)
                     +s(over1_5)
                     +s(trad_h)
                    # +jordart
                    # +as.factor(jordart)
)




#tax data
fit.gam<-gam(form.vif,data=model_data,"quasibinomial")
summary(fit.gam)
plot(fit.gam,pages=1,scale=F, shade=T,all.terms=T)
gam.check(fit.gam,col=model_data$inv,pch=19)
dropvar_all<-drop_cont(form.vif,model_data,method="GCV.Cp",fam="quasibinomial")
dropvar_all







##########################################################################################################################################################



#prediction

#cut the maps

sb_name<-"tuorpon"
e_sb<-e_tuorpon
pol_sb<-tuorpons

sb_name<-"baste"
e_sb<-e_baste
pol_sb<-baste


sb_name<-"tassosen"
e_sb<-e_toss
pol_sb<-tassosen

sb_name<-"handolsdalen"
e_sb<-e_hand
pol_sb<-handalsdalen

b2.e<-crop(b2.r,e_sb)
b3.e<-crop(b3.r,e_sb)
b4.e<-crop(b4.r,e_sb)
b5.e<-crop(b5.r,e_sb)
b6.e<-crop(b6.r,e_sb)
b7.e<-crop(b7.r,e_sb)
b8.e<-crop(b8.r,e_sb)
b9.e<-crop(b9.r,e_sb)
b10.e<-crop(b10.r,e_sb)
b11.e<-crop(b11.r,e_sb)
b12.e<-crop(b12.r,e_sb)
ndvi.e<-crop(ndvi.r,e_sb)
ndci.e<-crop(ndci.r,e_sb)
savi.e<-crop(savi.r,e_sb)
soil.e<-crop(soil.r,e_sb)
over1_5.e<-crop(over1_5.r,e_sb)
trad_h.e<-crop(trad_h.r,e_sb)
jordart.e<-crop(jordart.r,e_sb)
    jordart.e<-resample(jordart.e,trad_h.e,method="ngb")




out <- b2.e

bs <- blockSize(out,chunksize=100000)
out <- writeStart(out, filename="lichen_temp", overwrite=TRUE)
i<-1
for (i in 1:bs$n) {
 
  
  b2 <- getValues(b2.e, row=bs$row[i], nrows=bs$nrows[i] )
  b3 <- getValues(b3.e, row=bs$row[i], nrows=bs$nrows[i] )
  b4 <- getValues(b4.e, row=bs$row[i], nrows=bs$nrows[i] )
  b5 <- getValues(b5.e, row=bs$row[i], nrows=bs$nrows[i] )
  b6 <- getValues(b6.e, row=bs$row[i], nrows=bs$nrows[i] )
  b7 <- getValues(b7.e, row=bs$row[i], nrows=bs$nrows[i] )
  b8 <- getValues(b8.e, row=bs$row[i], nrows=bs$nrows[i] )
  b9 <- getValues(b9.e, row=bs$row[i], nrows=bs$nrows[i] )
  b10 <- getValues(b10.e, row=bs$row[i], nrows=bs$nrows[i] )
  b11 <- getValues(b11.e, row=bs$row[i], nrows=bs$nrows[i] )
  b12 <- getValues(b12.e, row=bs$row[i], nrows=bs$nrows[i] )
  ndvi <- getValues(ndvi.e, row=bs$row[i], nrows=bs$nrows[i] )
  ndci <- getValues(ndci.e, row=bs$row[i], nrows=bs$nrows[i] )
  savi <- getValues(savi.e, row=bs$row[i], nrows=bs$nrows[i] )
  soil <- getValues(soil.e, row=bs$row[i], nrows=bs$nrows[i] )
  over1_5 <- getValues(over1_5.e, row=bs$row[i], nrows=bs$nrows[i] )
  trad_h <- getValues(trad_h.e, row=bs$row[i], nrows=bs$nrows[i] )
  #jordart <- getValues(jordart.e, row=bs$row[i], nrows=bs$nrows[i] )
  
  pred.data<-data.frame(b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,ndvi,ndci,savi,soil,over1_5,trad_h)#,jordart)
  pre.val<-predict(fit.gam,pred.data,type="response")
  out <- writeValues(out, pre.val, bs$row[i])
  

}
out <- writeStop(out)

file_name<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/result/",sb_name,".tif",sep="")
writeRaster(out, filename=file_name, format="GTiff", overwrite=TRUE)



out.m<-mask(out,pol_sb)
plot(out.m)
plot(pol_sb,add=T)
file_name<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/result/",sb_name,"_mask.tif",sep="")
writeRaster(out.m, filename=file_name, format="GTiff", overwrite=TRUE)



#betestyp



out.bt <- out.m
out.perc <- out.m
bs <- blockSize(out.bt,chunksize=100000)
out.bt <- writeStart(out.bt, filename="lichen_temp", overwrite=TRUE)
out.perc <- writeStart(out.perc, filename="lichen_temp1", overwrite=TRUE)
i<-1
for (i in 1:bs$n) {
  
  
  lav <- getValues(out.m, row=bs$row[i], nrows=bs$nrows[i] )
  lav.df<-data.frame(lav)
  lav.df<-lav.df%>%mutate(lav_class=case_when(
    lav >=0 & lav<0.1~"1",
    lav >=0.1 & lav<0.25~"2",
    lav >=0.25 & lav<0.5~"3",
    lav >=0.5 & lav<=1~"4"))%>%mutate(lav_class=as.factor(lav_class))
  lav<-round(lav*100,0)
  
  out.bt <- writeValues(out.bt, lav.df$lav_class, bs$row[i])
  out.perc <- writeValues(out.perc, lav, bs$row[i])
  
}
out.bt <- writeStop(out.bt)
out.perc <- writeStop(out.perc)

file_name<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/result/",sb_name,"_mask_bt.tif",sep="")
writeRaster(out.bt, filename=file_name, format="GTiff", overwrite=TRUE,datatype="INT4S")

file_name<-paste("F:/Lavproject2019/RBP-lichen_projct_2019/result/",sb_name,"_mask_perc.tif",sep="")
writeRaster(out.perc, filename=file_name, format="GTiff", overwrite=TRUE, datatype="INT4S")


