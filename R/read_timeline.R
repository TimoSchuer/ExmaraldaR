#' read_timeline()
#'
#' Reads timestamps from an exb Transcription file
#'
#' @param file Requires the xml-object of an .exb-file
#'
#' @return Returns a Vector of the timeline in an EXMARaLDA transcription file
#' @export
#'
#' @examples
#' read_timeline(file)
read_timeline <- function(file) {
    timeline <- xml2::xml_attrs(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1))) %>% do.call(rbind, .)
    timeline <- timeline[,1]
  return(timeline)
}
