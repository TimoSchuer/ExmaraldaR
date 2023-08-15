read_events <- function(file, path, addPaths=FALSE){
  transcriptions <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='t']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn
  events <- data.frame()
  for (n in 1:length(transcriptions)) {
    events_help <- data.frame()
    events_help <- transcriptions[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()
    events_help[,"Text"] <-  transcriptions[n] %>% xml2::xml_children() %>% xml2::xml_text()
    attrs <- transcriptions[n] %>% xml2::xml_attrs() %>% as.data.frame() %>% dplyr::mutate(rownames=rownames(.)) %>% tidyr::pivot_wider(names_from = rownames, values_from = 1)
    events_help <- events_help %>% dplyr::mutate(File= stringr::str_remove(basename(path), "\\.exb")) %>%
      dplyr::mutate(Speaker= dplyr::if_else("speaker" %in% names(attrs), attrs$speaker,NA_character_)) %>%
      dplyr::mutate(TierID= dplyr::if_else("id" %in% names(attrs), attrs$id,NA_character_)) %>%
      dplyr::mutate(Name= dplyr::if_else("display-name" %in% names(attrs), attrs$`display-name`,NA_character_)) %>%
      dplyr::mutate(TierCategory= dplyr::if_else("category" %in% names(attrs), attrs$category,NA_character_))
    events <- dplyr::bind_rows(events, events_help)
  }

  events <-events %>% dplyr::rename(Start= start, End= end) %>% .[,c("File", "Speaker", "TierID","TierCategory", "Name", "Start", "End","Text")]
  if(addPaths=TRUE){
    events <- events %>% dplyr::mutate(pathFile=path, .after=File) %>% dplyr::mutate(pathAudio=xml2::xml_find_all(file,"//referenced-file") %>% xml2::xml_attr("url") %>% c(), .after=pathFile)
  }
  return(events)
}
