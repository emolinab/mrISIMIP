library(mrcommons)
library(ggplot2)
library(ggrepel)

LandTypes<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE,nclasses = "six",country_level = FALSE, selectyears = "past")

LandTotal<-dimSums(LandTypes,dim=3)
LandTypesCountry<-dimSums(LandTypes,dim=1.2)
LandTypesCountry<-setNames(dimSums(calcOutput("Croparea",sectoral = "kcr", physical = TRUE, cellular = FALSE,aggregate=FALSE,
                         cells = "magpiecell", irrigation = FALSE),dim=3),"crop")

mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")

IAM_1995<-readMAgPIE(ssp="ssp126",gcm="GFDL-ESM4",type="states",subtype=NULL,year=c(1995))
IAM_2000<-readMAgPIE(ssp="ssp126",gcm="GFDL-ESM4",type="states",subtype=NULL,year=c(2000))


IAM2_1995<-readIMAGE(ssp="1_26",gcm="GFDL",type="STATES",subtype=NULL,year=1995)
IAM2_2000<-readIMAGE(ssp="1_26",gcm="GFDL",type="STATES",subtype=NULL,year=2000)


IAM<-mbind(IAM_1995,IAM_2000)
IAM2<-mbind(IAM2_1995,IAM2_2000)

years<-intersect(getYears(LandTotal),getYears(IAM))

IAM_land<-IAM[,years,]*LandTotal[,years,]
IAM_land<-dimSums(IAM_land,dim=1.2)

IAM_final<-mbind(setNames(IAM_land[,,c("cropland","grazing")],c("crop","past")),
                 setNames(dimSums(IAM_land[,,c("primf","secdf")],dim=3),"forest"),
                 setNames(IAM_land[,,c("timber")],c("forestry")))

IAM2_land<-IAM2[,years,]*LandTotal[,years,]
IAM2_land<-dimSums(IAM2_land,dim=1.2)

IAM2_final<-mbind(setNames(dimSums(IAM2_land[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3),c("crop")),
                  setNames(dimSums(IAM2_land[,,c("pastr","range")],dim=3),c("past")),
                 setNames(dimSums(IAM2_land[,,c("primf","secdf")],dim=3),"forest"),
                 setNames(IAM2_land[,,c("timber")],c("forestry")))

IAM2_final[!is.finite(IAM2_final)]<-0

names<-intersect(getNames(LandTypesCountry),getNames(IAM_final))
cells<-intersect(getCells(LandTypesCountry),getCells(IAM_final))

LandChangeCountryFAO<- as.ggplot(setYears(LandTypesCountry[cells,2000,names]-LandTypesCountry[cells,1995,names],2000))
LandChangeCountryIAM<- as.ggplot(setYears(IAM_final[cells,2000,names]-IAM_final[cells,1995,names],2000))
LandChangeCountryIAM$Model<-"MAgPIE"
LandChangeCountryIAM2<- as.ggplot(setYears(IAM2_final[cells,2000,names]-IAM2_final[cells,1995,names],2000))
LandChangeCountryIAM2$Model<-"IMAGE"

Total_land<-as.ggplot(LandTypesCountry[cells,2000,names])
Total_land<-Total_land[,c("Region","Year","Data1","Value")]
colnames(Total_land)<-c("Region","Year","Type","Land")


plot_landChange<-merge(LandChangeCountryFAO,LandChangeCountryIAM,by=c("Region","Year","Data1","Scenario"))
colnames(plot_landChange)<-c("Region","Year","Type","Scenario","FAO","IAM","Model")


plot_landChange<-merge(plot_landChange,Total_land,by=c("Region","Year","Type"))
plot_landChange$label<-plot_landChange$Region
plot_landChange[!(plot_landChange$FAO<(-2.5) | plot_landChange$FAO>(2.5) |
                  plot_landChange$IAM<(-2.5) | plot_landChange$IAM>(2.5)),"label"]<-""

plot_landChange2<-merge(LandChangeCountryFAO,LandChangeCountryIAM2,by=c("Region","Year","Data1","Scenario"))
colnames(plot_landChange2)<-c("Region","Year","Type","Scenario","FAO","IAM","Model")


plot_landChange2<-merge(plot_landChange2,Total_land,by=c("Region","Year","Type"))
plot_landChange2$label<-plot_landChange2$Region
plot_landChange2[!(plot_landChange2$FAO<(-2.5) | plot_landChange$FAO>(2.5) |
                    plot_landChange2$IAM<(-2.5) | plot_landChange$IAM>(2.5)),"label"]<-""


out_dir<-"/p/projects/landuse/users/mbacca/ISIMIP_OUT/Statistics_raw/"
LandChange_GLO<-ggplot(rbind(plot_landChange,plot_landChange2),aes(x=FAO,y=IAM))+theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 20),
        axis.title.x = element_text(color = "grey20", size = 22),
        axis.text.y = element_text(color = "grey20", size = 20),
        axis.title.y = element_text(color = "grey20", size = 22),
        plot.title = element_text(hjust = 0.5,size=24),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.background = element_blank())+
  ylab("MAgPIE \n (mio. ha)")+xlab("FAO cropland change \n (mio. ha)")+labs(color = "Source",size="Cropland size (y2000) \n mio. ha")+ggtitle(paste0("Cropland change 2000-1995"))+
  geom_point(aes(color=Model,size=Land))+geom_abline(intercept =0 , slope = 1)+ xlim(-15,15)+ylim(-15,15)#+
#  geom_text_repel(aes(label = label),size=5)

png(file = (paste0(out_dir,"LandUseChange_",getYears(plot_landChange),"_global.png")),
    width = 800,
    height = 600)

print(LandChange_GLO)
dev.off()
