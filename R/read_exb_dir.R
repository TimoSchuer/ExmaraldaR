read_exb_dir <- function(pathDir, readAnn=TRUE,addDescription= FALSE, addMetaData= FALSE, addIPNumber=TRUE,IPEndSign= c("|",".",";",",",",","?","=","-")){
  files <- list.files(pathDir,".\\.exb", full.names = TRUE)
  exb <- read_exb_file(files[1],readAnn,addDescription, addMetaData=FALSE, addIPNumber, IPEndSign)
  for (k in 2:length(files)) {
    help <- read_exb_file(path=,files[k],readAnn,addDescription, addMetaData=FALSE, addIPNumber, IPEndSign)
    exb <- dplyr::bind_rows(exb,help)
  }
  if(addMetaData==TRUE){
    exb2 <- data.frame()
    for (i in 1:length(files)){
      metaData <- read_metadata(files[i] %>% xml2::read_xml()) %>%  dplyr::mutate(File= stringr::str_remove(basename(files[i]), "\\.exb"))
      exb_help <- exb %>% dplyr::filter(File== stringr::str_remove(basename(files[i]), "\\.exb")) %>%  dplyr::left_join(metaData, by=c("Speaker", "File"),suffix=c("","_y") )
      exb2 <- dplyr::bind_rows(exb2,exb_help)
    }
    #metaCols <-
    exb <- exb2 %>% dplyr::select(1:Name, dplyr::setdiff(names(exb2), names(exb)),Text:dplyr::last_col())

  }
  exb <- dplyr::mutate(exb, IPId=paste(File, IPNumber, sep= "_")) %>%  dplyr::mutate(exb, EventID=paste(File, EventID, sep= "_")) %>% dplyr::select(IPId,1:dplyr::last_col(offset = 1))
  return(exb)
}
