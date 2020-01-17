#' Write_back_to Exb
#' writes back changes in annotations to an exb
#'
#' @param CsvFile File or object with data.frame as created by read_exb_file or read_exb_dir
#' @param sep seperator in Csv file
#' @param PathExb Path of the ExbFile
#' @param PathNewFile Directory where the new file is saved
#'
#' @return
#' @export
#'
#' @examples
write_back_to_exb <- function(CsvFile,sep=",", PathExb, PathNewFile = dirname(PathExb), suffix="_new"){
  file <- xml2::read_xml(PathExb) #Read transcription
  if(is.data.frame(CsvFile)){
    annotations <- CsvFile
  }else{
    annotations <- read.delim(CsvFile, header = TRUE,sep=sep, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }
  #annotations <- unique(annotations)
  #save attributs of annotation tears
  annotationTiers <-xml2::xml_attrs(xml2::xml_find_all(file,".//tier[@type='a']"))
  #tier <- xml2::xml_find_all(file,".//tier[@type='a']")
  for (r in 1:length(annotationTiers)) {
    #replace annotation tier with empty annotation tier with the same attributs
    XPath <- stringr::str_c(".//tier[@id='", annotationTiers[[r]][["id"]], "']")
    # XPath <- shQuote(XPath)
    xml2::xml_remove(xml2::xml_find_first(file, XPath))
    xml2::xml_add_sibling(xml2::xml_child(xml2::xml_child(file, 2), 2),"tier", id= annotationTiers[[r]][["id"]], speaker= annotationTiers[[r]][["speaker"]], category =annotationTiers[[r]][["category"]], type =annotationTiers[[r]][["type"]])
    xml2::xml_set_attr(xml2::xml_find_first(file, XPath), "display-name",annotationTiers[[r]][["display-name"]] )
    events <- dplyr::filter(annotations ,Speaker== annotationTiers[[r]][["speaker"]] & is.na(annotations[,which(colnames(annotations)=="Variable")])==FALSE)
    ColNum <- which(colnames(events)=="Variable")
    TextAnn <- dplyr::select(events, ColNum:ncol(events))
    colnames <- colnames(TextAnn)
    if(nrow(events)!=0){
      # rebuild tags -------------------------------------------------------------
      tag <- character(0)
      for (k in 1:nrow(events)) {
        tag[k] <- stringr::str_c("1",":",TextAnn[k,1],"_")
        for (l in 2:ncol(TextAnn)) {
          if(is.na(TextAnn[k,l])==FALSE){
            tag[k] <- stringr::str_c(tag[k],l,":", colnames[l],":",TextAnn[k,l],"_")
          }else{next()}
        }
        tag[k] <- stringr::str_replace(tag[k], "_$",";")
      }
      events$tag =tag

      # merge annotations that belong to the same word together -----------------
      if(nrow(events)>1){
        u <- 2
        doubleRow <- numeric(0)
        for (u in 2:nrow(events)) {
          if(events[u,"Start"]==events[u-1,"Start"]){
            events[u,"tag"] <- stringr::str_c(events[u-1,"tag"],events[u,"tag"])
            doubleRow <- c(doubleRow, u-1)
          }
        }
        events <- events[-(doubleRow),]
      }
      # add events to tier ------------------------------------------------------
      if(nrow(events)!=0){
        for (i in 1:nrow(events)) {
          xml2::xml_add_child(xml2::xml_find_first(file,XPath) ,"event", start= events[i,"Start"], end=events[i,"End"])
          xml2::xml_set_text(xml2::xml_child(xml2::xml_find_first(file,".//tier[@type='a']"),i), events[i,"tag"])
        }
      }
    }
  }
  fileName <- stringr::str_remove(basename(PathExb), "\\.exb")
  PathNewFile <- stringr::str_glue(PathNewFile,"\\",stringr::str_trim(fileName),suffix,".exb")
  xml2::write_xml(file, PathNewFile)
}
