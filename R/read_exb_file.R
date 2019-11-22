#' read_exb_file()
#'
#' Function that reads an exb transcription file
#'
#' @param path Path of an exb transcription file
#' @param PathTagSet Path of the Tag Set used to annotate the file
#' @param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#'
#' @return Returns a data frame that contains the transcription an the annotations
#' @export
#'
#' @examples
#' read_exb_file(path.PathTagSet)
read_exb_file <- function(path,PathTagSet, addMetaData= FALSE,sortMetaData=TRUE){
  if(check_exb(path)){
    file <- xml2::read_xml(path, encoding="UTF-8")
    timeline <- read_timeline(file)
    events <- read_events(file, path)
    events[,5] <- stringr::str_trim(events[,5])
    annotations <- read_annotations_exb(file)
    events_sorted <- sort_events(events, timeline)
    events_sorted <- add_IpNumber(events_sorted)
    exb <- dplyr::left_join(events_sorted, annotations,by = c("Speaker", "Start", "End"))
    exb <- sort_anntotations_linear(exb,PathTagSet)
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
    return(exb)
  } else {
    stop("File has to be an EXMARaLDA basis-transcription (.exb)")
  }
}
