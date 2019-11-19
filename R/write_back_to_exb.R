write_back_to_exb <- function(CsvFile,sep=",", PathExb, PathNewFile = PathExb){
file <- xml2::read_xml(PathExb) #Read transcription
annotations <- read.delim(CsvFile, header = TRUE,sep=sep, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
#annotations <- unique(annotations)
#save attributs of annotation tears
annotationTiers <-xml2::xml_attrs(xml2::xml_find_all(file,".//tier[@type='a']"))
#tier <- xml2::xml_find_all(file,".//tier[@type='a']")
for (r in 1:length(annotationTiers)) {
  #replace annotation tier with empty annotation file with the same attributs
  XPath <- stringr::str_c(".//tier[@id='", annotationTiers[[r]][["id"]], "']")
 # XPath <- shQuote(XPath)
  xml_remove(xml2::xml_find_first(file, XPath))
  xml2::xml_add_sibling(xml2::xml_child(xml2::xml_child(file, 2), 2),"tier", id= annotationTiers[[r]][["id"]], speaker= annotationTiers[[r]][["speaker"]], category =annotationTiers[[r]][["category"]], type =annotationTiers[[r]][["type"]])
  xml2::xml_set_attr(xml2::xml_find_first(file, XPath), "display-name",annotationTiers[[r]][["display-name"]] )
  events <- dplyr::filter(annotations ,Speaker== annotationTiers[[r]][["speaker"]] & is.na(annotations[,10])==FALSE)
  TextAnn <- dplyr::select(events, 10:ncol(events))
  colnames <- colnames(TextAnn)
  # rebuild tags -------------------------------------------------------------
  tag <- character(0)
  for (k in 1:nrow(events)) {
  tag[k] <- stringr::str_c(colnames[1],":",TextAnn[k,1],"_")
    for (l in 2:ncol(TextAnn)) {
      if(is.na(TextAnn[k,l])==FALSE){
        tag[k] <- stringr::str_c(tag[k], colnames[l],":",TextAnn[k,l],"_")
      }
      tag[k] <- stringr::str_replace_all(tag[k], ":\\s",":")
      tag[k] <- stringr::str_replace(tag[k], "_$",";")
    }
  }
  events$tag =tag

# merge annotations that belong to the same word together -----------------
  u <- 2
  doubleRow <- numeric(0)
  for (u in 2:nrow(events)) {
    if(events[u,"Start"]==events[u-1,"Start"]){
      events[u,"tag"] <- stringr::str_c(events[u-1,"tag"],events[u,"tag"])
      doubleRow <- c(doubleRow, u-1)
    }
  }
  events <- events[-(doubleRow),]
  # add events to tier ------------------------------------------------------
  for (i in 1:nrow(events)) {
    xml2::xml_add_child(xml2::xml_find_first(file,XPath) ,"event", start= events[i,"Start"], end=events[i,"End"])
    xml2::xml_set_text(xml2::xml_child(xml2::xml_find_first(file,".//tier[@type='a']"),i), events[i,"tag"])
  }

}
 write_xml(file, PathNewFile)
}
