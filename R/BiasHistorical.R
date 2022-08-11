

MAgPIE<-readMAgPIE(ssp="ssp585",gcm="GFDL-ESM4",type="states",year=2000)
MAgPIE[MAgPIE>0&MAgPIE<1e-3]<-0
IIASA<-readIMAGE(ssp="5_85",gcm="GFDL",type="STATES",subtype=NULL,year=2000)
MAgPIE[MAgPIE>0&MAgPIE<1e-4]<-0

MAgPIE<-statisticsHistoricalGrid(MAgPIE, LUM="MAgPIE",GCM="GFDL-ESM4",ssp="ssp585",variables=NULL,source="LUH",plot = TRUE)
IIASA<-statisticsHistoricalGrid(IIASA, LUM="IIASA",GCM="GFDL-ESM4",ssp="ssp585",variables=NULL,source="LUH",plot = TRUE)

IAMS<-rbind(MAgPIE,IIASA)



var_reg<-ggplot(IAMS,aes(y=LUM,x=item))+
  geom_tile(aes(fill=bias))+scale_fill_gradient2(low="#ff6c5f",mid="white",high="#2dde98",
                                                  breaks=c(-0.12,0,0.12),
                                                  labels=c(-0.12,0,0.12),
                                                  limits=c(-0.2,0.2),
                                                  trans="pseudo_log"
  )+theme_bw()+
  ylab("")+xlab("")+labs(color = "percent")+#facet_wrap(~Region,ncol=4)+
  theme(axis.text.x = element_text(color = "grey20", size = 22,angle = 45,vjust = 0.95, hjust=1),
        axis.title.x = element_text(color = "grey20", size = 24),
        axis.text.y = element_text(color = "grey20", size = 22),
        axis.title.y = element_text(color = "grey20", size = 24),
        plot.title = element_text(hjust = 0.5,size=26),
        legend.text=element_text(size=20),
        legend.title=element_text(size=22),
        legend.background = element_blank(),
        strip.text.x = element_text(size = 15))+labs(fill = "Bias (2000)")+
  #ggtitle(paste0(scenario,"\n",gcm[g],"\n",2100))+
  geom_text(aes(label=(round(bias,2))),size=4,color="#5a5a5a")

png(file = (paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Statistics_raw/Bias.png")),
    width = 900,
    height = 300)

print(var_reg)
dev.off()
