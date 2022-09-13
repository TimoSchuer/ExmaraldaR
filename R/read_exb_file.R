#' Title
#'
#' @param path path of .exb file
#' @param readAnn logical, if annotation tiers should be read
#' @param addDescription logical, if description tiers should be read NOT YET POSSIBLE
#' @param addMetaData logical, if information from speaker table should be read
#' @param addIPNumber logical, if there should be an nummeration of intonation phrases
#' @param IPEndSign character, spezifies characters that indicate the end of a intonation unit
#' @importFrom rlang :=
#' @return data.frame
#' @export

read_exb_file <- function(path, readAnn=TRUE,addDescription= FALSE, addMetaData= FALSE, addIPNumber=TRUE,IPEndSign= c("|",".",";",",",",","?","=","-")){
  if(stringr::str_ends(path, "\\.exb")== FALSE){
    return("File is not an .exb file")
  }
    file <- xml2::read_xml(path, encoding="UTF-8")
    timeline <- xml2::xml_attrs(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1))) %>% dplyr::bind_rows()
    events <- ExmaraldaR:::read_events(file, path) %>%  #read events
      dplyr::left_join(., timeline[,1:2], by= c("Start" ="id"), suffix= c("",".y") ) %>% dplyr::rename(Start_time= time)%>% dplyr::mutate(Start_time=as.double(Start_time)) %>%  #allocate absoulute times to time stamps
      dplyr::left_join(., timeline[,1:2], by= c("End" ="id"), suffix= c("",".y") ) %>% dplyr::rename(End_time= time) %>% dplyr::mutate(End_time= as.double(End_time)) %>%
      .[,c("File","Speaker", "TierID",  "Start","End", "Start_time", "End_time","Name","Text")] # nice and tidy order
    events <-ExmaraldaR:::sort_events(events, addIPNumber= addIPNumber, IPEndSign= IPEndSign)
    if(readAnn==TRUE & length(xml2::xml_find_all(file,".//tier[@type='a']"))!=0){
      exb <- ExmaraldaR:::read_annotations(file,events)
      if(is.null(exb)){
        return(NULL)
      }
    }else{
      exb <- events
    }

  ##clean doubled lines and coerce annotations ##TODO: Verhalten beobachen
  exb <-  exb %>% dplyr::group_by(EventID) %>% dplyr::summarise(dplyr::across(tidyselect::everything(), ~dplyr::first(na.omit(.x)))) %>% dplyr::ungroup() %>% dplyr::arrange(IPNumber)

  if(addDescription==TRUE & length(xml2::xml_find_all(file,".//tier[@type='d']"))!=0){
      exb <- ExmaraldaR:::read_descriptions(file,exb)
      if(is.null(exb)){
        return(NULL)
      }
  }
  if(addMetaData==TRUE){
    metaData <- read_metadata(file)
    exb <- dplyr::left_join(exb,metaData, by="Speaker", suffix= c("",".y") ) %>% dplyr::select(1:Name, names(metaData),Text:tidyselect::last_col())
  }
  #add EventID
  exb <- exb %>% dplyr::mutate(EventID= seq(1:nrow(exb))) %>% as.data.frame()
  return(exb)
}

