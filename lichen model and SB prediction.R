
library(mgcv)
library(raster)
library(usdm)
library(dplyr)
library(rgdal)
library(mapview)

rasterOptions(tmpdir="F:/temp_raster")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"


######################################################################################################
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


sb_data<-readOGR("F:/Lavproject2019/incommande data/Tuorpons","LavInv112 Tuorpons sameby.20190919.225341")
sb_data<-spTransform(sb_data,CRS=projSWEREF)
plot(sb_data)
mapview(sb_data)


b10<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 
jordart<-raster("M:/reindder_lichen_map/jordart.tif")
over1_5<-raster("F:/Lavproject2019/laser_old/veg_vinter.tif")
trad_h<-raster("F:/Lavproject2019/laser_old/p95_vinter.tif")






sb_data$b2<-extract(b2,sb_data)
sb_data$b3<-extract(b3,sb_data)
sb_data$b4<-extract(b4,sb_data)
sb_data$b5<-extract(b5,sb_data)
sb_data$b6<-extract(b6,sb_data)
sb_data$b7<-extract(b7,sb_data)
sb_data$b8<-extract(b8,sb_data)
sb_data$b9<-extract(b9,sb_data)
sb_data$b10<-extract(b10,sb_data)
sb_data$b11<-extract(b11,sb_data)
sb_data$b12<-extract(b12,sb_data)
sb_data$ndvi<-extract(ndvi,sb_data)
sb_data$ndci<-extract(ndci,sb_data)
sb_data$savi<-extract(savi,sb_data)
sb_data$soil<-extract(soil,sb_data)
sb_data$over1_5<-extract(over1_5,sb_data)
sb_data$trad_h<-extract(trad_h,sb_data)
sb_data$jordart<-extract(jordart,sb_data)

ymin<-extent(sb_data)[3]-90000
ymax<-extent(sb_data)[4]+90000
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
tax_model_data.e1$averkning<-extract(avverkning,tax_model_data.e1)
sb_data$averkning<-extract(avverkning,tax_model_data.e1)



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
                     +as.factor(jordart)
)




#tax data
fit.gam<-gam(form.vif,data=model_data,"quasibinomial")
summary(fit.gam)
plot(fit.gam,pages=1,scale=F, shade=T,all.terms=T)
gam.check(fit.gam)

