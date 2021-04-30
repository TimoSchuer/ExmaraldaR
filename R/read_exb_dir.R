#' Read entire directory of exb files annotated with the same annotation specification
#'
#' @param pathDir path of the directory
#'@param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#'@param readAnn Logical Value, whetaer annotation tiers should be read and sorted
#'@param annotation "linear" or multilayer. See vignette for further information
#'@param addDescription logical value wheter description tiers should be inclouded
#' @return returns dataframe that contains all events, an IP number and formatted annotations
#' @export
#'
#' @examples
#' path <- system.file("extdata\\read_dir", package = "ExmaraldaR", mustWork = TRUE)
#' read_exb_dir(path, addMetaData = TRUE, readAnn = TRUE, annotation = "linear")
read_exb_dir <- function(pathDir, addMetaData= FALSE, readAnn=TRUE,annotation= c("linear", "multilayer"),addDescription= FALSE){
  files <- list.files(pathDir,".\\.exb")
  addMetaDataDir <- addMetaData
  readAnnDir <- readAnn
  AnnotationDir <- annotation
  addDescriptionDir <- addDescription
  exb <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[1])),annotation= AnnotationDir, addDescription= addDescriptionDir, addMetaData=addMetaDataDir, readAnn= readAnnDir , sortMetaData=FALSE)
  k <- 2
  for (k in 2:length(files)) {
    help <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[k])), addMetaData=addMetaDataDir, readAnn= readAnnDir, sortMetaData=FALSE, annotation= AnnotationDir)
    exb <- dplyr::bind_rows(exb,help)
  }
  if(addMetaDataDir==TRUE){
    startMetaData <- which(colnames(exb)=="sex")
    k <- ncol(exb)
    l <- startMetaData-1
    n <- k-startMetaData

    exb <- exb[,c(1:6,startMetaData:k,7:l)]
  }
  exb <- dplyr::mutate(exb, IPId=paste(File, IpNumber, sep= "_"))
  exb <- dplyr::mutate(exb, EventId=paste(File, EventId, sep= "_"))
  exb <- dplyr::bind_cols(IPId=exb$IPId, exb[,1:ncol(exb)-1])
  return(exb)
}
