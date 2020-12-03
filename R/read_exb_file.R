#' read_exb_file()
#'
#' Function that reads an exb transcription file
#'
#' @param path Path of an exb transcription file
#' @param addMetaData Logical Value, wheter Metadata should be read from the speakertable
#' @param sortMetaData Logical value, wheter metadata should be sorted directly after the speaker name or at the end
#' @param readAnn Logical Value, whetaer annotation tiers should be read and sorted
#' @param annotation "linear" or multilayer. See vignette for further information
#' @param addDescription logical value wheter description tiers should be inclouded
#'
#' @return Returns a data frame that contains the transcription and the annotations
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE) # for a linear annotation
#' example_linear <- read_exb_file(path, readAnn = TRUE, annotation= "linear", addMetaData = TRUE)
#' path <- system.file("extdata", "Example_multi.exb", package = "ExmaraldaR", mustWork = TRUE) # for a multilayer annotation
#' example_multi <- read_exb_file(path, readAnn = TRUE, annotation= "multilayer", addMetaData = TRUE)
#'
read_exb_file <- function(path, readAnn=TRUE,annotation= c("linear", "multilayer"),addDescription= FALSE, addMetaData= FALSE,sortMetaData=TRUE){
  if(check_exb(path)){
    file <- xml2::read_xml(path, encoding="UTF-8")
    timeline <- read_timeline(file)
    events <- read_events(file, path)
    events[,5] <- stringr::str_trim(events[,5])
    events_sorted <- sort_events(events, timeline)
    events_sorted <- dplyr::left_join(events_sorted,timeline, by=c("Start" = "id")) %>% dplyr::rename(Start_time = time) #Add absolute timepoints for start
    events_sorted <- dplyr::left_join(events_sorted,timeline, by=c("End" = "id")) %>% dplyr::rename(End_time = time) #Add absolute timepoints for start
    events_sorted <- add_IpNumber(events_sorted)
    if(addDescription == TRUE &
       length(xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='d']")) != 0  ){
      descriptions <- read_description(file, timeline)
      ##check for annotations over more than one tier
      #MultiAnn <- dplyr::anti_join( descriptions,events_sorted, by= c("Start", "End", "Start_time", "End_time"))
      #MultiAnn<- MultiAnn[which((MultiAnn$Start %in% events_sorted$Start)|(MultiAnn$End %in% events_sorted$End)),]
      events_sorted <- dplyr::full_join(events_sorted, descriptions, by= c("Start", "End", "Start_time", "End_time"), suffix= c("", "_yy")) %>% dplyr::select(!dplyr::ends_with("_yy")) %>%dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%  dplyr::mutate_at(dplyr::vars(Start_time,End_time), as.numeric) %>% dplyr::arrange(Start_time)
      }
    events_sorted <- add_IpNumber(events_sorted)
    AnnotationTiers <- xml2::xml_find_all(file,".//tier[@type='a']") #findet alle Annotationsspuren
    if(readAnn==TRUE & length(AnnotationTiers) !=0){
      if(annotation=="linear"){
        annotations <- read_annotations_linear(file)
        exb <- dplyr::left_join(events_sorted, annotations,by = c("Speaker", "Start", "End"))
        MultiEventAnn <- dplyr::anti_join( annotations,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
        if(nrow(MultiEventAnn)!=0){
          for (n in 1:nrow(MultiEventAnn)) {
            a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
            b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
            exb[seq(a:b),colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
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
      MetaData <- read_metadata(file)
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
    return(as.data.frame(exb))
  } else {
    stop("File has to be an EXMARaLDA basis-transcription (.exb)")
  }
}
