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
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE)
#' file <- xml2::read_xml(path, encoding="UTF-8")
#' read_timeline(file)
read_timeline <- function(file) {
   # read timeline and supress a meaningless warning
  withCallingHandlers( timeline <- xml2::xml_attrs(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1))) %>% do.call(rbind, .), warning =  function(w) if(grepl("number of columns of result is not a multiple of vector length", w, fixed= TRUE)) invokeRestart( "muffleWarning" ))
  timeline <- as.data.frame(timeline[,1:2])
  return(timeline)
}
