#' Read entire directory of exb files annotated with the same annotation specification
#'
#' @param pathDir path of the directory
#'@param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#' @return returns dataframe that contains all events, an IP number and formatted annotations
#' @export
#'
#' @examples read_exb_dir(pathDir)
read_exb_dir <- function(pathDir, addMetaData= FALSE){
  files <- list.files(pathDir,".\\.exb")
  addMetaDataDir <- addMetaData
  exb <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[1])), addMetaData=addMetaDataDir, sortMetaData=FALSE)
  k <- 2
  for (k in 2:length(files)) {
    help <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[k])), addMetaData=addMetaDataDir, sortMetaData=FALSE)
    exb <- dplyr::bind_rows(exb,help)
  }
  if(addMetaDataDir==TRUE){
    startMetaData <- which(colnames(exb)=="sex")
    k <- ncol(exb)
    l <- startMetaData-1
    n <- k-startMetaData

    exb <- exb[,c(1:6,startMetaData:k,7:l)]
  }
  return(exb)
}
