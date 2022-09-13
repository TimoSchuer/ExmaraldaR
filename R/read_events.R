#' Helper function reads events
#'
#' @param file
#' @param path
#'

read_events <- function(file, path){
  transcriptions <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='t']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn
  events <- data.frame()
  for (n in 1:length(transcriptions)) {
    events_help <- data.frame()
    events_help <- transcriptions[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()
    events_help[,"Text"] <-  transcriptions[n] %>% xml2::xml_children() %>% xml2::xml_text()
    events_help <- events_help %>% dplyr::mutate(File= stringr::str_remove(basename(path), "\\.exb")) %>%
      dplyr::mutate(Speaker= xml2::xml_attrs(transcriptions[n])[[1]][["speaker"]]) %>%
      dplyr::mutate(TierID= xml2::xml_attrs(transcriptions[n])[[1]][['id']]) %>%
      dplyr::mutate(Name=xml2::xml_attrs(transcriptions[n])[[1]][['display-name']])
    events <- dplyr::bind_rows(events, events_help)
  }

  events <-events %>% dplyr::rename(Start= start, End= end) %>% .[,c("File", "Speaker", "TierID", "Name", "Start", "End","Text")]
  return(events)
}
