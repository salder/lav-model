library(sp)
library(sf)
library(dplyr)
library(ggplot2)

angesa.sf<-st_read("D:/UMEA/Renbruksplan/Lavprojekt_2019/incommande data/Angesa_2021_inventering/LavInv/LavInv131 Angesa sameby.20220124.115157.shp")
angesa.df<-angesa.sf %>% as.data.frame()


for (i in 1:length(angesa.df$INVDATUM))
angesa.df$år[i]<-strsplit(as.character(angesa.df$INVDATUM[i]),split="-")[[1]][1]

angesa.sf$ar<-angesa.df$år

library(mapview)
mapview(angesa.sf,cex = "MARKLAVAKT",zcol = "ar")

angesa.df$år<-ifelse(angesa.df$år==2011,2020,angesa.df$år)
angesa.df$YTA2<-as.numeric(angesa.df$YTA)


table(angesa.df$år[!is.na(angesa.df$YTA2)])


ggplot(angesa.df[!is.na(angesa.df$YTA2),], aes(x=MARKLAVAKT, color=år)) +
  geom_histogram(fill="white", alpha=0.5, position="identity",bins = 30)+scale_color_brewer(palette="Paired") +
  theme_minimal()+theme_classic()+labs(y="antal provytor",x="täckning av marklav")+
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))
 
 geom_vline(data=angesa.df, aes(xintercept=MARKLAVAKT, color=ar),
             linetype="dashed")


hist(angesa.df$MARKLAVAKT[angesa.df$ar==2020],nclass=50)
hist(angesa.df$MARKLAVAKT[angesa.df$ar==2022],nclass=50,add=T)
