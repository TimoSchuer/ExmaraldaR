sort_annotations_multilayer <- function(file, AnnotationTiers,events_sorted){
  AnnText <- xml2::xml_text( xml2::xml_children(AnnotationTiers[1]))
  time <-  xml2::xml_attrs(xml2::xml_children(AnnotationTiers[[1]]))
  time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
  Speaker <-  xml2::xml_attr(AnnotationTiers[[1]], "speaker")
  AnnTier <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= AnnText)
  AnnTier <- `colnames<-`(AnnTier, c("Speaker","Start","End",xml2::xml_attr(AnnotationTiers[1], "display-name")))
  exb <-  suppressWarnings(dplyr::left_join(events_sorted, AnnTier))
  if(length(AnnotationTiers)>1){
    for (k in 2:length(AnnotationTiers)) {
      AnnText <- xml2::xml_text( xml2::xml_children(AnnotationTiers[k]))
      time <-  xml2::xml_attrs(xml2::xml_children(AnnotationTiers[[k]]))
      time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
      Speaker <-  xml2::xml_attr(AnnotationTiers[[k]], "speaker")
      AnnTier <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= AnnText)
      AnnTier <- `colnames<-`(AnnTier, c("Speaker","Start","End",xml2::xml_attr(AnnotationTiers[k], "display-name")))
      exb <- suppressWarnings(dplyr::left_join(exb, AnnTier, by=c("Speaker", "Start", "End")))
    }
  }
}
