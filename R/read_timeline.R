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
  timeline <- c()
  n <- 1
  for (n in 1:length(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1)))) {
    timeline[n] <- xml2::xml_attrs(xml2::xml_child(xml2::xml_child(xml2::xml_child(file, 2), 1), n))[["id"]]
    n <- n+1
  }
  return(timeline)
}
