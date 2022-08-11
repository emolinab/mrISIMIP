library(ncdf4) # package for netcdf manipulation

convert2NC<-function(folder,type){
 
  ncpath <- paste0("/p/projects/landuse/users/mbacca/ISIMIP_IMAGE/",folder,"/")
  ncname <- paste0("LUH2_",type,"_IMAGE")  
  
  if(!file.exists(paste0(ncpath,ncname,".nc"))){  
  ncin<-nc_open(paste0("/p/projects/magpie/transfers/ISIMIP3b_IMAGE_LUH_220725/ISIMIP3b_LUH_IMAGE_dataexchange/",folder,"/LUH2_",type,"_15MIN.NC"))
  lon<-ncvar_get(ncin,"lon")
  lat<-ncvar_get(ncin,"lat")
  time<-ncvar_get(ncin,"time")
  time<-round(time/365,0)+1970
  
  NAR<- if(type=="STATES") "NARSTATES" else if(type=="PROTECTED_AREA") "NARPROT" else if(type %in% c("NITROGEN_FERTILIZER","IRRIGATION")) "NLUMIPC" else if(type=="BIOENERGY") "NARBIOEN" else ""
  
  if(type!="FLOOD"){
    varnames<- ncvar_get(ncin,NAR)
    varnames<-gsub(" ", "", varnames, fixed = TRUE)
    }
  variables<- ncvar_get(ncin)
  
  #creating the new nc

  if(!dir.exists(ncpath)) dir.create(ncpath)
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  dname <- NAR
  
  londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
  timedim <- ncdim_def("time","Years",as.double(time))
  
  if(type!="FLOOD"){
    
    var_out<-list()
    fillvalue<-0
    for (v in 1:length(varnames)){
      var_out[[varnames[v]]]<-ncvar_def(varnames[v],"fraction of grid cell",list(londim,latdim,timedim),fillvalue,varnames[v],prec="single")
    }
    
    ncout<-nc_create(ncfname,var_out)
    for (v in 1:length(varnames)){
      ncvar_put(ncout,varnames[v],variables[v,,,])
    }
    
    ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
    ncatt_put(ncout,"lat","axis","Y")
    ncatt_put(ncout,"time","axis","T")
    
    nc_close(ncout)
  }else{
    var_out<-list()
    fillvalue<-0
    var_out[["FLOOD"]]<-ncvar_def("FLOOD","fraction of grid cell",list(londim,latdim,timedim),fillvalue,"FLOOD",prec="single")
    ncout<-nc_create(ncfname,var_out)
    ncvar_put(ncout,"FLOOD",variables)
    
    ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
    ncatt_put(ncout,"lat","axis","Y")
    ncatt_put(ncout,"time","axis","T")
    
    nc_close(ncout)
    
  }
  }
  
}

folders<-list.dirs(path = "/p/projects/magpie/transfers/ISIMIP3b_IMAGE_LUH_220725/ISIMIP3b_LUH_IMAGE_dataexchange/", full.names = FALSE, recursive = TRUE)

folders<-folders[2:length(folders)]
types<-c("STATES","PROTECTED_AREA","NITROGEN_FERTILIZER","IRRIGATION","FLOOD","BIOENERGY")

for(f in folders){
  for(t in types){
    
    convert2NC(f,t)
  }
}
