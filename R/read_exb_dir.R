#' Title
#'
#' @param pathDir path of directory with .exb files
#' @param readAnn logical, if annotation tiers should be read
#' @param addDescription logical, if description tiers should be read NOT YET POSSIBLE
#' @param addMetaData logical, if information from speaker table should be read
#' @param addIPNumber logical, if there should be an nummeration of intonation phrases
#' @param IPEndSign character, spezifies characters that indicate the end of a intonation unit
#' @importFrom rlang :=
#'
#' @return data.frame
#' @export
read_exb_dir <- function(pathDir,
                         readAnn=TRUE,
                         addDescription= FALSE,
                         addMetaData= FALSE,
                         addIPNumber=TRUE,
                         IPEndSign= c("|",".",";",",",",","?","=","-"),
                         verbose=TRUE){
  files <- list.files(pathDir,".\\.exb", full.names = TRUE)
  list_exb <- list()
  IsList <- rep(FALSE,length(files))
  exb <- read_exb_file(files[1],readAnn,addDescription, addMetaData=FALSE, addIPNumber, IPEndSign)
  list_exb <- append(list_exb,list(exb))
  if(is.list(exb)){
    IsList[1] <- TRUE
  }
  if (verbose==TRUE){
    print(paste(Sys.time()," 1/ ",length(files),"...",basename(files[1]),"...done"))
  }
  for (k in 2:length(files)) {
    help <- read_exb_file(path=,files[k],readAnn,addDescription, addMetaData=FALSE, addIPNumber, IPEndSign)
    if(is.list(help)){
    help[[2]] %>% dplyr::mutate(File=basename(files[k]) %>% stringr::str_remove("\\.exb"))
    list_exb <- append(list_exb,list(help))
    IsList[k] <- TRUE
    }else{
      list_exb <- append(list_exb,help)
    }
    #exb <- dplyr::bind_rows(exb,help)

    if (verbose==TRUE){
      print(paste(Sys.time()," ", k,"/ ",length(files),"...",basename(files[k]),"...done"))
    }
  }
  if(any(IsList)){
    print("Adding MetaData not possible as there are problems with assigning annotations.")
    print("Returns list with annotations and transcriptions per file, where annotations could not be allignend. Else alligned data.frame.")
    return(list_exb)
  }else{
    exb <- data.frame()
    for (n in 1:length(list_exb)) {
      exb <- dplyr::bind_rows(exb,list_exb[[1]])
    }
  }
  if(addMetaData==TRUE & !any(IsList)){
    exb2 <- data.frame()
    for (i in 1:length(files)){
      metaData <- read_metadata(files[i] %>% xml2::read_xml()) %>%  dplyr::mutate(File= stringr::str_remove(basename(files[i]), "\\.exb"))
      exb_help <- exb %>% dplyr::filter(File== stringr::str_remove(basename(files[i]), "\\.exb")) %>%  dplyr::left_join(metaData, by=c("Speaker", "File"),suffix=c("","_y") )
      exb2 <- dplyr::bind_rows(exb2,exb_help)
    }
    #metaCols <-
    exb <- exb2 %>% dplyr::select(1:Name, dplyr::setdiff(names(exb2), names(exb)),Text:dplyr::last_col())

  }


 # exb <- dplyr::mutate(exb, IPId=paste(File, IPNumber, sep= "_")) %>%  dplyr::mutate(exb, EventID=paste(File, EventID, sep= "_")) %>% dplyr::select(IPId,1:dplyr::last_col(offset = 1))
  return(exb)
}
