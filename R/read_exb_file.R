#' read_exb_file()
#'
#' Function that reads an exb transcription file
#'
#' @param path Path of an exb transcription file
#' @param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#' @param sortMetaDate Logical value, wheter metadata should be sorted directly after the speaker name or at the end
#' @param readAnn Logical Value, whetaer annotation tiers should be read and sorted
#'
#' @return Returns a data frame that contains the transcription and the annotations
#' @export
#'
#' @examples
#' read_exb_file(path, addMetaData=True)
read_exb_file <- function(path, readAnn=TRUE,annotation= c("linear", "multilayer"),addMetaData= FALSE,sortMetaData=TRUE){
  if(check_exb(path)){
    file <- xml2::read_xml(path, encoding="UTF-8")
    timeline <- read_timeline(file)
    events <- read_events(file, path)
    events[,5] <- stringr::str_trim(events[,5])
    events_sorted <- sort_events(events, timeline)
    events_sorted <- add_IpNumber(events_sorted)
    AnnotationTiers <- xml2::xml_find_all(file,".//tier[@type='a']") #findet alle Annotationsspuren
    if(readAnn==TRUE & length(AnnotationTiers) !=0){
      if(annotation=="linear"){
        annotations <- read_annotations_linar(file)
        exb <- dplyr::left_join(events_sorted, annotations,by = c("Speaker", "Start", "End"))
        MultiEventAnn <- dplyr::anti_join( annotations,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
        if(nrow(MultiEventAnn)!=0){
          for (n in 1:nrow(MultiEventAnn)) {
            a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
            b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
            exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
          }
        }

        exb <- sort_anntotations_linear(exb)
      }else if(annotation=="multilayer"){
        exb <- sort_annotations_multilayer(file, AnnotationTiers, events_sorted)
      }
    }else{
      exb <- events_sorted
    }

    if(addMetaData==TRUE){
      MetaData <- read_metadata(path)
      exb2 <- dplyr::left_join(exb, MetaData, by= "Speaker")
      if(sortMetaData==TRUE){
        n <- ncol(exb2)-ncol(exb)
        k <- ncol(exb2)
        l <- k-n
        m <- l+1
        exb <- exb2[,c(1:6,m:k,7:l)]
      }else{
        exb <- exb2
      }
    }
    exb <- dplyr::rename(exb, IpNumber="IP Number")
    return(exb)
  } else {
    stop("File has to be an EXMARaLDA basis-transcription (.exb)")
  }
}
