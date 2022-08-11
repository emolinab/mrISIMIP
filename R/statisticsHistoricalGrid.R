#' @title       Statistics of historical values
#' @description R2, MAE and Bias of the LUMs compared to historical values
#'
#' @export
#'
#' @param x MAgPIE object with the states for the different
#' @param
#' @param
#' @return data frame with statistical values of fit (R2,MAE,bias)
#'
#' @author Edna Molina Bacca
#'
#' @importFrom calcLUH2V2 mrcommons
#' @importFrom
#' @examples
#'
#'   \dontrun{
#'     x <- statisticsHistorical(LUM="MAgPIE",GCM="GFDL-ESM4",ssp="ssp126",variables=NULL,Source="LUH")
#'   }
#'

statisticsHistoricalGrid <- function(x, y, LUM="MAgPIE",GCM="GFDL-ESM4",ssp="ssp126",variables="",source="LUH",plot = TRUE){

  path<-paste0("/p/projects/landuse/users/mbacca/ISIMIP_OUT/Statistics_raw/",LUM,"_",ssp,"/")

  LUH <- x
  states<- y
  states[!is.finite(states)]<-0

  names <- intersect(getNames(LUH),getNames(states))
  years <- intersect(getYears(LUH),getYears(states))
  folder_out<-paste0(path)

  if(!dir.exists(folder_out)) dir.create(folder_out)
  all<-NULL


  for(n in names){
    for(y in years){

      if(plot){
        a <- plotCorrHist2D(LUH[,y,n], states[,y,n], title = NULL, xlab = source, ylab = LUM, bins = 100, folder = folder_out, file=paste0(LUM,"_",GCM,"_",ssp),  fontLabel = 5.5, breaks=c(2,20,200,2000))
      }

      a_aux<-as.data.frame(t(as.numeric(a[[2]][2,])))
      a_aux$item<-n
      a_aux$LUM<-LUM
      a_aux$GCM<-GCM
      a_aux$ssp<-ssp
      colnames(a_aux)<-c("year","r2","MAE","will ref","bias","item","LUM","GCM","ssp")

      all<-rbind(all,a_aux)


    }
  }

 #all$bias<-(-all$bias)
 write.csv(all,paste0(folder_out,"/",variables,"_",LUM,"_",GCM,"_",ssp,".csv"))
  return(all)
}
