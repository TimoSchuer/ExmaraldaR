
#' Read_annotations()
#'
#' Reads anntotations from an exb transcription file
#'
#' @param file
#'
#' Needs the xml object of an exb transcription file
#'
#' @return
#' Returns a data.frame
#' @export
#'
#' @examples
#' read_annotations(file)
read_annotations_exb <- function(file) {
  AnnotationTiers <- xml2::xml_find_all(file,".//tier[@type='a']") #findet alle Annotationsspuren

  # Definiert Variablen -----------------------------------------------------
  Start <- character(0)
  End <- character(0)
  Annot <- character(0)
  Speaker <- character(0)
  for (k in 1:length(AnnotationTiers)) {
    AnnSp <- xml2::xml_attrs(AnnotationTiers[[k]])[["speaker"]] #Annotierte Transkriptionsspur auslesen
    l <- 1
    s <-character(0)
    e <- character(0)
    Ann <- character(0)
    AnnSpeaker <- character(0)
    if(length(xml2::xml_children(AnnotationTiers[k]))!=0){
    for (l in 1:length(xml2::xml_children(AnnotationTiers[[k]]))) {
        s[l] <- xml2::xml_attrs(xml2::xml_child(AnnotationTiers[[k]], l))[["start"]] #Startzeitpunkt auslesen
        e[l] <- xml2::xml_attrs(xml2::xml_child(AnnotationTiers[[k]], l))[["end"]] #Endzeitpunkt auslesen
        Ann[l] <- xml2::xml_text(xml2::xml_child(AnnotationTiers[[k]], l)) #Annotation auslesen
        AnnSpeaker[l] <- AnnSp #Annotierten Sprecher als Liste schreiben, um diesen vor jeder Annotation zu haben
      }
    Start <- c(Start,s)
    End <- c(End,e)
    Annot <- c(Annot,Ann)
    Speaker <- c(Speaker,AnnSpeaker)
    }
  }
  Annotation <- data.frame(Speaker = Speaker,Start= Start,End= End,Annotation = Annot, stringsAsFactors = FALSE) # Dataframe erzeugen
  return(Annotation)
}
