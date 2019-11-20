#' Read entire directory of exb files annotated with the same annotation specification
#'
#' @param pathDir path of the directory
#' @param PathTagSet path of the annotation specification used for the annotation
#'@param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#' @return returns dataframe that contains all events, an IP number and formatted annotations
#' @export
#'
#' @examples read_exb_dir(pathDir, PathTagSet)
read_exb_dir <- function(pathDir, PathTagSet,addMetaData= FALSE){
  files <- list.files(pathDir,".\\.exb")

  exb <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[1])),PathTagSet, addMetaData=addMetaData, sortMetaData=FALSE)
  k <- 2
  for (k in 2:length(files)) {
    help <- read_exb_file(path= unlist(stringr::str_c(pathDir,"\\",files[k])),PathTagSet, addMetaData=addMetaData, sortMetaData=FALSE)
    exb <- dplyr::bind_rows(exb,help)
  }
  if(addMetaData==TRUE){
    startMetaData <- which(colnames(exb)=="sex")
    k <- ncol(exb)
    l <- startMetaData-1
    n <- k-startMetaData

    exb <- exb[,c(1:6,startMetaData:k,7:l)]
  }
  return(exb)
}
