library(terra)
library(mrcommons)

#### states (others)
#MAgPIE

LUH <-calcOutput("LUH2v2",landuse_types = "LUH2v2", irrigation = FALSE,
                 cellular = TRUE, cells = "magpiecell", selectyears = "past",aggregate=FALSE)
LUH <- LUH/dimSums(LUH,dim=3)
LUH[!is.finite(LUH)] <-0
LUH[LUH>0 & LUH<1e-3] <-0

MAgPIE<-readMAgPIE(ssp="ssp126",gcm="GFDL-ESM4",type="states",subtype=NULL,year=2010)
statisticsHistoricalGrid(round(LUH,3), round(MAgPIE,3), LUM="MAgPIE",GCM="GFDL-ESM4",ssp="ssp126",variables="states",source="LUH",plot = TRUE)

#IMAGE
IMAGE<-readIMAGE(ssp="1_26",gcm="GFDL",type="STATES",subtype=NULL,year=2010)
statisticsHistoricalGrid(round(LUH,3), round(IMAGE,3), LUM="IMAGE",GCM="GFDL-ESM4",ssp="ssp126",variables="states",source="LUH",plot = TRUE)


#### Large land types #########


LUH <-calcOutput("LUH2v2",landuse_types = "magpie", irrigation = FALSE,
                 cellular = TRUE, cells = "magpiecell", selectyears = "past",aggregate=FALSE)
LUH <- LUH/dimSums(LUH,dim=3)
LUH[!is.finite(LUH)] <-0
LUH[LUH>0 & LUH<1e-3] <-0

#### GLOBIOM

#Forest
x<-setNames(LUH[,2010,"forest"],"Forest")
y<-readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Forest",year=2010)

statisticsHistoricalGrid(x, y, LUM="GLOBIOM",GCM="GFDL-ESM4",ssp="ssp126",variables="Forest",source="LUH",plot = TRUE)

#Cropland
x<-setNames(LUH[,2010,"crop"],"Cropland")
y<-readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Cropland",year=2010)

statisticsHistoricalGrid(x, y, LUM="GLOBIOM",GCM="GFDL-ESM4",ssp="ssp126",variables="Cropland",source="LUH",plot = TRUE)

#Grassland
x<-setNames(LUH[,2010,"past"],"Grassland")
y<-readGLOBIOM(ssp="1",SPA="1",RCP="2p6",gcm="GFDL",type="Grassland",year=2010)

statisticsHistoricalGrid(x, y, LUM="GLOBIOM",GCM="GFDL-ESM4",ssp="ssp126",variables="Grassland",source="LUH",plot = TRUE)

######## IMAGE

x<-setNames(LUH[,2010,c("forest","crop","past")],c("Forest","Cropland","Grassland"))

y<-x
y[,,"Forest"]<-dimSums(IMAGE[,,c("primf","secdf")],dim=3)
y[,,"Cropland"]<-dimSums(IMAGE[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3)
y[,,"Grassland"]<-dimSums(IMAGE[,,c("pastr","range")],dim=3)

statisticsHistoricalGrid(x, y, LUM="IMAGE",GCM="GFDL-ESM4",ssp="ssp126",variables="LandType",source="LUH",plot = TRUE)

######## MAgPIE

y<-x
y[,,"Forest"]<-dimSums(MAgPIE[,,c("primf","secdf")],dim=3)
y[,,"Cropland"]<-dimSums(MAgPIE[,,c("c3per","c4ann","c3ann","c3nfx","c4per")],dim=3)
y[,,"Grassland"]<-dimSums(MAgPIE[,,c("pastr","range")],dim=3)

statisticsHistoricalGrid(x, y, LUM="MAgPIE",GCM="GFDL-ESM4",ssp="ssp126",variables="LandType",source="LUH",plot = TRUE)
