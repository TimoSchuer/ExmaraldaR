#' Reads and sorts annotations in multilayer format
#'
#' @param file object returned by xml2::read_xml() of an .exb-file
#' @param AnnotationTiers vector containing the annotation tiers
#' @param events_sorted data.frame returned by sort_events()
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_multi.exb", package = "ExmaraldaR", mustWork = TRUE)
#' file <- xml2::read_xml(path, encoding="UTF-8")
#' events_sorted <- sort_events(events= read_events(file, path), timeline= read_timeline(file))
#' events_sorted <- add_IpNumber(events_sorted)
#' sort_annotations_multilayer(file, AnnotationTiers= xml2::xml_find_all(file,".//tier[@type='a']"), events_sorted)
sort_annotations_multilayer <- function(file, AnnotationTiers,events_sorted){
  AnnText <- xml2::xml_text( xml2::xml_find_all(AnnotationTiers[1], ".//event"))
  time <-  xml2::xml_attrs(xml2::xml_children(AnnotationTiers[[1]]))
  time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
  Speaker <-  xml2::xml_attr(AnnotationTiers[[1]], "speaker")
  AnnTier <- data.frame(Speaker= paste(Speaker), Start= time[,1], End= time[,2],AnnText= AnnText)
  AnnTier <- `colnames<-`(AnnTier, c("Speaker","Start","End",xml2::xml_attr(AnnotationTiers[1], "display-name")))
  exb <-  suppressWarnings(dplyr::left_join(events_sorted, AnnTier, by=c("Speaker", "Start", "End")))
  MultiEventAnn <- dplyr::anti_join( AnnTier,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
  if(nrow(MultiEventAnn)!=0){
    #exb <- events_sorted
    for (n in 1:nrow(MultiEventAnn)) {
      a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
      b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
      exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
    }
  }
    if(length(AnnotationTiers)>1){
    for (k in 2:length(AnnotationTiers)) {
      AnnText <- xml2::xml_text( xml2::xml_children(AnnotationTiers[k]))
      AnnText[AnnText==""] <- NA #replace empty events with NA
      if(length(AnnText)>0){#control for empty annotation tiers
        time <-  xml2::xml_attrs(xml2::xml_children(AnnotationTiers[[k]]))
        time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
        Speaker <-  xml2::xml_attr(AnnotationTiers[[k]], "speaker")
        AnnTier <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= AnnText)
        AnnTier<- `colnames<-`(AnnTier, c("Speaker","Start","End",xml2::xml_attr(AnnotationTiers[k], "display-name")))
        exb <- suppressWarnings(dplyr::left_join(exb, AnnTier, by=c("Speaker", "Start", "End")))
        MultiEventAnn <- dplyr::anti_join( AnnTier,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
        if(nrow(MultiEventAnn)!=0){
          for (n in 1:nrow(MultiEventAnn)) {
            a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
            b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
            exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
          }
        }
      }
    }
    }
  return(exb)
}
