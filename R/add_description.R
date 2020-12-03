

#' Title
#'
#' @param file
#' @param timeline
#'
#' @return
#' @export
#'
#' @examples
#'
read_description<- function(file, timeline){
  descriptionTiers <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='d']")
  DesText <- xml2::xml_text( xml2::xml_children(descriptionTiers[1]))
  time <-  xml2::xml_attrs(xml2::xml_children(descriptionTiers[[1]]))
  time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
  Speaker <-  xml2::xml_attr(descriptionTiers[[1]], "speaker")
  desTier <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= DesText)
  desTier <- `colnames<-`(desTier, c("Speaker","Start","End",paste("Desc_",xml2::xml_attr(descriptionTiers[1], "display-name"))))
  for (k in 2:length(descriptionTiers)) {
    desTier_help <- data.frame()
    if(length(xml2::xml_children(descriptionTiers[k]))!=0){##check for empty tiers
      DesText <- xml2::xml_text( xml2::xml_children(descriptionTiers[k]))
      time <-  xml2::xml_attrs(xml2::xml_children(descriptionTiers[[k]]))
      time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
      Speaker <-  xml2::xml_attr(descriptionTiers[[k]], "speaker")
      desTier_help <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= DesText)
      desTier_help <- `colnames<-`(desTier_help, c("Speaker","Start","End",paste("Descr_",xml2::xml_attr(descriptionTiers[k], "display-name"))))
      desTier <- dplyr::full_join(desTier, desTier_help, by= c("Start", "End"), suffix= c("", "_yyy")) %>% dplyr::select(!ends_with("_yyy"))
      }else{
        dummy <- character(nrow(desTier))
        desTier <- cbind(desTier, dummy)
        desTier <- `colnames<-`(desTier,c(colnames(desTier)[1:ncol(desTier)-1], paste("Descr_",xml2::xml_attr(descriptionTiers[k], "display-name"))))
      }
  }
  desTier <- dplyr::left_join(desTier, timeline, by=c("Start" = "id"))%>% dplyr::rename(Start_time = time)
  desTier <- dplyr::left_join(desTier, timeline, by=c("End" = "id"))%>% dplyr::rename(End_time = time)
  order <- c(1:3, seq(ncol(desTier)-1, ncol(desTier)),seq(4,ncol(desTier)-2))
  desTier <- desTier[,order]
  desTier <- desTier %>% dplyr::mutate_all(dplyr::na_if, "")
  desTier <- dplyr::arrange(desTier, Start_time)
  return(desTier)
}
# exb <-  suppressWarnings(dplyr::left_join(events_sorted, desTier, by=c("Speaker", "Start", "End")))
# MultiEventAnn <- dplyr::anti_join( desTier,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
# if(nrow(MultiEventAnn)!=0){
#   exb <- events_sorted
#   for (n in 1:nrow(MultiEventAnn)) {
#     a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
#     b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
#     exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
#   }
# }
# if(length(descriptionTiers)>1){
#   for (k in 2:length(descriptionTiers)) {
#     DesText <- xml2::xml_text( xml2::xml_children(descriptionTiers[k]))
#     DesText[DesText==""] <- NA #replace empty events with NA
#     if(length(DesText)>0){#control for empty annotation tiers
#       time <-  xml2::xml_attrs(xml2::xml_children(descriptionTiers[[k]]))
#       time <- data.frame(matrix(unlist(time), ncol = max(lengths(time)), byrow = TRUE))
#       Speaker <-  xml2::xml_attr(descriptionTiers[[k]], "speaker")
#       desTier <- data.frame(Speaker= Speaker, Start= time[,1], End= time[,2],AnnText= DesText)
#       desTier<- `colnames<-`(desTier, c("Speaker","Start","End",xml2::xml_attr(descriptionTiers[k], "display-name")))
#       exb <- suppressWarnings(dplyr::left_join(exb, desTier, by=c("Speaker", "Start", "End")))
#       MultiEventAnn <- dplyr::anti_join( desTier,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
#       if(nrow(MultiEventAnn)!=0){
#         for (n in 1:nrow(MultiEventAnn)) {
#           a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
#           b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
#           exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
#         }
#       }
#     }
#   }
# }

#' #' Title
#' #'
#' #' @param file
#' #' @param path
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' read_description <- function(file, path){
#'   descriptions <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='d']") # findet alle Deskriptionsspuren; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn
#'   event <- character(0)
#'   start <- character(0)
#'   end <- character(0)
#'   text <- character(0)
#'   speaker <- character(0)
#'   tierId <- character(0)
#'   name <- character(0)
#'   filename <- stringr::str_remove(basename(path), "\\.exb")
#'   for(tier in descriptions){
#'     event <- xml2::xml_children(tier)
#'     text_help <- xml2::xml_text(event, trim= TRUE) #Transkriptionstext auslesen
#'     #text_help <- stringr::str_trim(text_help)
#'     tierAttrs <- xml2::xml_attrs(tier) #TierId, Sprecher und Sprechername auslesen
#'     # Start und Endzeitpunkte auslesen ----------------------------------------
#'     start_help <- xml2::xml_attrs(event) %>% sapply('[','start') %>% unname()
#'     end_help <- xml2::xml_attrs(event) %>% sapply('[','end') %>% unname()
#'     start <- c(start,start_help)
#'     end <- c(end,end_help)
#'     text <- c(text, text_help)
#'     speaker_help <- character(length(text_help))
#'     if("speaker" %in% names(tierAttrs)){speaker_help <- paste(speaker_help, tierAttrs[['speaker']])}
#'     speaker <- c(speaker, speaker_help)
#'     tierId_help <-  character(length(text_help))
#'     tierId_help <- paste(tierId_help, tierAttrs[['id']])
#'     tierId <- c(tierId,tierId_help)
#'     name_help <-  character(length(text_help))
#'     name_help <-  paste(name_help,tierAttrs[['display-name']])
#'     name <- c(name,name_help)
#'     filename_help <-  character(length(start))
#'     filename_help <- paste(filename_help, filename)
#'   }
#'   speaker <- stringr::str_trim(speaker)
#'   if(length(length(end)!=length(tierId))){#if theres an dummy element at the end of a splitted transcription adjust lenght
#'     name <- name[1:length(end)]
#'     tierId <- tierId[1:length(end)]
#'     speaker <- speaker[1:length(end)]
#'   }
#'   events <- data.frame(File = filename_help, Speaker = speaker, TierID = tierId, Name= name,Text= text,Start =unlist(unname(start)),End=unlist(unname(end)), stringsAsFactors = FALSE)
#'
#'   return(events)
#' }
