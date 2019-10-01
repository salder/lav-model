
library(mgcv)
library(raster)
library(usdm)
library(dplyr)



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



tax_model_data<-readRDS(tax_lav.data,file="tax_lav_map_vintergrasing.rds")
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



e_reg<-extent(c(338670,942260, 6776420, 7130669  )) #south
e_reg<-extent(c(338670,942260, 7289755, 7623202  )) #north

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


