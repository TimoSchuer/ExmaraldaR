read_annotations <- function(file){
  AnnotationTiers <- xml2::xml_find_all(file,".//tier[@type='a']") #findet alle Annotationsspuren
  annotations <- data.frame()
  for (n in 1:length(AnnotationTiers)) {
    ann_help <- data.frame()
    ann_help <- AnnotationTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()%>% dplyr::rename(Start= start, End= end) %>%
      dplyr::mutate(Annotation=AnnotationTiers[n]  %>% xml2::xml_children() %>% xml2::xml_text()) %>%
      dplyr::mutate(Speaker= xml2::xml_attrs(AnnotationTiers[n])[[1]][["speaker"]]) %>%
      dplyr::mutate(TierID= xml2::xml_attrs(AnnotationTiers[n])[[1]][['id']]) %>%
      dplyr::mutate(Name=xml2::xml_attrs(AnnotationTiers[n])[[1]][['display-name']])
      annotations <- dplyr::bind_rows(annotations, ann_help)
  }
  return(annotations)
}
