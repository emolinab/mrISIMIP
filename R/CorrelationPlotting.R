source("/p/projects/landuse/users/mbacca/Paper2/mrISIMIP/R/readMAgPIE.R")
source("/p/projects/landuse/users/mbacca/Paper2/mrISIMIP/R/readIMAGE.R")
source("/p/projects/landuse/users/mbacca/Paper2/mrISIMIP/R/readGLOBIOM.R")
source("/p/projects/landuse/users/mbacca/Paper2/mrISIMIP/R/statisticsHistoricalGrid.R")


LUH<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE,nclasses = "six",country_level = FALSE, selectyears = 2010)
LUH<-mbind(LUH[,,c("crop","past","urban","other")],
           setNames(dimSums(LUH[,,c("forestry","forest")],dim=3),"forestry"))

MAgPIE<-readMAgPIE(ssp="ssp126",gcm="GFDL-ESM4",type="states",subtype=NULL,year=2010)*dimSums(LUH,dim=3)
MAgPIE<-mbind(setNames(MAgPIE[,,c("cropland","grazing")],c("crop","past")),
               setNames(dimSums(MAgPIE[,,c("primf","secdf","timber")],dim=3),"forest"),
               setNames(dimSums(MAgPIE[,,c("primn","secdn")],dim=3),"other"),
               setNames((MAgPIE[,,c("urban")]),"urban"))


IMAGE<-readIMAGE(ssp="1_26",gcm="GFDL",type="STATES",subtype=NULL,year=2010)*dimSums(LUH,dim=3)
IMAGE[!is.finite(IMAGE)]<-0

IMAGE<-mbind(setNames(dimSums(IMAGE[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3),c("crop")),
                  setNames(dimSums(IMAGE[,,c("pastr","range")],dim=3),c("past")),
                  setNames(dimSums(IMAGE[,,c("primf","secdf","timber")],dim=3),"forest"),
                  setNames(dimSums(IMAGE[,,c("primn","secdn")]),c("other")),
                  setNames((IMAGE[,,c("urban")]),"urban"))

GLOBIOM_c<-setNames(readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Cropland",year=2010),"crop")
GLOBIOM_g<-setNames(readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Grassland",year=2010),"past")
GLOBIOM_f<-setNames(readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Forest",year=2010),"forest")
GLOBIOM_o<-setNames(readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Other",year=2010),"other")
GLOBIOM_u<-setNames(readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Urban",year=2010),"urban")

GLOBIOM<-mbind(GLOBIOM_c,GLOBIOM_g,GLOBIOM_f,GLOBIOM_o,GLOBIOM_u)*dimSums(LUH,dim=3)
