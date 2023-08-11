#' Write_back_to Exb
#' writes back changes in annotations to an exb
#'WARNING: All annotation tiers that are not present in the dataframe will be removed in the new file. New files are no problem. Not present description tiers may cause errors.
#' @param exb File or object with data.frame as created by read_exb_file or read_exb_dir
#' @param sep seperator in Csv file
#' @param PathExb Path of the ExbFile
#' @param PathNewFile Directory where the new file is saved
#' @param suffix suffix to be added to the new files, default is "_new"
#' @param transcription_text specify column with transcription text description
#' @param annotation_colums specify names of annotaion columns
#' @param overwrite_annotations if true old annotaitons will be deleted, AT THE MOMENT ONLY WORKS IF TRUE
#' @param assignSpeakerAnnotation if TRUE one annotation tier per category per speaker will be added, AT THME MOMENT ONLY WORKS IF FALSE

#' @return NULL
#' @export

write_back_to_exb <-
  function(exb,
           sep = ",",
           PathExb,
           PathNewFile = dirname(PathExb),##TODO Parameter für Text spalte
           suffix = "_new",
           transcription_text= "Text",
           annotation_colums,
           overwrite_annotations=TRUE,
           assignSpeakersAnnotation=FALSE) {
    file <- xml2::read_xml(PathExb) #Read transcription
    if (is.data.frame(exb)) {
      exb <- exb
    } else{
      exb <-utils::read.delim(
          exb,
          header = TRUE,
          sep = sep,
          row.names = 1,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
    }
    ##create new timeline
    #check if all tiers are in dataframe
    timeline_old <- data.frame(row.names = seq(from=1,to=xml2::xml_find_all(file,"//common-timeline") %>%
                                                 xml2::xml_children() %>%
                                                 xml2::xml_attr("id") %>%
                                                 length())) %>%
      mutate(id= file %>% xml2::xml_find_all("//common-timeline") %>%
               xml2::xml_children() %>%
               xml2::xml_attr("id")) %>%
      mutate(time=file %>% xml2::xml_find_all("//common-timeline") %>%
               xml2::xml_children() %>%
               xml2::xml_attr("time") %>%
               as.double() %>%
               round(6)) %>%
      mutate(type=file %>% xml2::xml_find_all("//common-timeline") %>%
               xml2::xml_children() %>%
               xml2::xml_attr("type") )
    timeline_new <- c(exb$Start_time,exb$End_time) %>%
      round(6) %>%
      unique() %>%
      as.double() %>%
      data.frame(time=.)%>%
      arrange(time) %>%
      distinct() %>%
      dplyr::anti_join(timeline_old, by="time") %>%
      mutate(id=stringr::str_extract(timeline_old$id,"\\d+") %>% as.numeric() %>% max()+ row_number()) %>%
      mutate(id= paste("T", id, sep="")) %>%
      dplyr::mutate(type="intp")
    timeline <- dplyr::bind_rows(timeline_old, timeline_new) %>% dplyr::arrange(time)

    if(nrow(timeline_new!=0)){
      exb <- exb %>%
        dplyr::left_join(bind_rows(timeline_old,timeline_new), by= c("Start_time"="time")) %>%
        rename(Start_new=id)
      exb <- exb %>%
        dplyr::left_join(bind_rows(timeline_old,timeline_new), by= c("End_time"="time")) %>%
        rename(End_new=id)

      common_timeline <- timeline %>%
        dplyr::mutate(tli=if_else(is.na(type),
                           paste0('<tli id="',id,'" time="',time,'"/>',sep=""),
                           paste0('<tli id="',id,'" time="',time,'" type="intp"/>', sep = ""))) %>%
        pull(tli) %>%
        as.character() %>%
        paste0(collapse = "") %>%
        paste0("<common-timeline>",.,"</common-timeline>", collapse = "") %>%
        xml2::read_xml()
      file %>% xml2::xml_find_all("//common-timeline") %>% xml2::xml_replace(common_timeline)
    }else{
      exb <- exb %>%
        dplyr::left_join(bind_rows(timeline_old,timeline_new), by= c("Start_time"="time")) %>%
        rename(Start_new=id)
      exb <- exb %>%
        dplyr::left_join(bind_rows(timeline_old,timeline_new), by= c("End_time"="time")) %>%
        rename(End_new=id)
    }

    for (t in unique(exb$TierID)) {
      TranscriptionTier <- exb %>% filter(TierID==t)
      if(length(xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")))!=0){
        attrs <- xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")) %>% xml2::xml_attrs()
        TierAttrs <- c()
        for (k in 1:length(names(attrs[[1]]))) {
         TierAttrs <- c(TierAttrs, paste0(names(attrs[[1]])[k],'="',attrs[[1]][[k]],'"'))
        }
        TierAttrs <- paste0(TierAttrs, collapse = " ")

        tier <- TranscriptionTier %>%
          dplyr::mutate(Event=paste0('<event start="',Start_new,'" end="',End_new,'">',.data[[transcription_text]],'</event>' )) %>%
          pull(Event) %>% paste0(collapse = "") %>%
          as.character() %>%
          paste0(paste0('<tier ',TierAttrs,'>', collapse = " "),
                 .,"</tier>", collapse = " ")%>%
          xml2::read_xml()
        xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']"))  %>% xml2::xml_remove(free = TRUE)
        xml2::xml_child(file, 2) %>%
          xml2::xml_add_child(tier)
        remove(tier)
        }
    }





    ##write annotations back

    if(overwrite_annotations==TRUE){
      xml2::xml_find_all(file,"//tier[@type='a']") %>% xml2::xml_remove(free = TRUE)
    }
    if(!any(is.na(annotation_colums))){
      for(ann in annotation_colums){
        if(ann %in% names(exb)){
          if(assignSpeakersAnnotation==FALSE) {
            tierNumbers <- xml2::xml_find_all(file,"//tier") %>% xml2::xml_attr("id") %>% stringr::str_extract("\\d+") %>% as.numeric() %>% max(na.rm = TRUE) +1
            tierId <-  paste0("TIE",tierNumbers, collapse = "")
            AnnTier <- exb %>% filter(!is.na(.data[[ann]])) %>% select(Start_new,End_new, {{ann}}) %>% dplyr::distinct()
            tier <- paste(paste('<tier id="',tierId,'" ', 'type="a"','category="',ann,'"',">"),AnnTier %>% mutate(Event=paste0('<event start="',Start_new,'" end="',End_new,'">',.data[[ann]],'</event>' )) %>% pull(Event) %>% paste0(collapse = ""),"</tier>") %>% as.character() %>% xml2::read_xml(tier)
            xml2::xml_child(file, 2) %>%
              xml2::xml_add_child(tier)
            remove(tier)
          }else if(assignSpeakersAnnotation==TRUE){
            annCat <- exb %>% filter(!is.na(.data[[ann]]))
            for (sp in unique(annCat$Name)) {
              tierNumbers <- xml2::xml_find_all(file,"//tier") %>% xml2::xml_attr("id") %>% stringr::str_extract("\\d+") %>% as.numeric() %>% max(na.rm = TRUE) +1
              tierId <-  paste0("TIE",tierNumbers, collapse = "")
               AnnTier <- annCat %>% filter(Name==sp)
               tier <- paste(
             paste0('<tier id="',tierId,'" ',
                   'type="a" ',
                   'category="',paste(ann,"_",stringr::str_extract(sp,"\\[.*\\]") %>% stringr::str_remove_all("\\[|\\]")),'" ',
                   'display-name="',sp %>% stringr::str_remove("\\[.*\\]") %>%
                     paste0("[",paste(ann,"_",stringr::str_extract(sp,"\\[.*\\]")%>% stringr::str_remove_all("\\[|\\]"),sep=""),"]",sep=""),
                   '" speaker="',unique(AnnTier$Speaker),'"',">"),
                 AnnTier %>%
                   mutate(Event=paste0('<event start="',Start_new,'" end="',End_new,'">',.data[[ann]],'</event>' )) %>%
                   pull(Event) %>%
                   paste0(collapse = ""),
                 "</tier>", collapse = "") %>%
                 as.character() %>%
                 xml2::read_xml()
               xml2::xml_child(file, 2) %>%
                  xml2::xml_add_child(tier)
               remove(tier)
                    }
          }
        }
      }
    }

    fileName <- stringr::str_remove(basename(PathExb), "\\.exb")
    PathNewFile <-      stringr::str_glue(PathNewFile,
                        "/",
                        stringr::str_trim(fileName),
                        suffix,
                        ".exb")
    xml2::write_xml(file, PathNewFile)
  }




# write_back_to_exb <- function(exb,sep=",", PathExb, PathNewFile = dirname(PathExb), suffix="_new"){
#   file <- xml2::read_xml(PathExb) #Read transcription
#   if(is.data.frame(exb)){
#     annotations <- exb
#   }else{
#     annotations <- read.delim(exb, header = TRUE,sep=sep, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
#   }
#   #annotations <- unique(annotations)
#   #save attributs of annotation tears
#   annotationTiers <-xml2::xml_attrs(xml2::xml_find_all(file,".//tier[@type='a']"))
#   #tier <- xml2::xml_find_all(file,".//tier[@type='a']")
#   for (r in 1:length(annotationTiers)) {
#     #replace annotation tier with empty annotation tier with the same attributs
#     XPath <- stringr::str_c(".//tier[@id='", annotationTiers[[r]][["id"]], "']")
#     # XPath <- shQuote(XPath)
#     xml2::xml_remove(xml2::xml_find_first(file, XPath))
#     xml2::xml_add_sibling(xml2::xml_child(xml2::xml_child(file, 2), 2),"tier", id= annotationTiers[[r]][["id"]], speaker= annotationTiers[[r]][["speaker"]], category =annotationTiers[[r]][["category"]], type =annotationTiers[[r]][["type"]])
#     xml2::xml_set_attr(xml2::xml_find_first(file, XPath), "display-name",annotationTiers[[r]][["display-name"]] )
#     events <- dplyr::filter(annotations ,Speaker== annotationTiers[[r]][["speaker"]] & is.na(annotations[,which(colnames(annotations)=="Variable")])==FALSE)
#     ColNum <- which(colnames(events)=="Variable")
#     TextAnn <- dplyr::select(events, ColNum:ncol(events))
#     colnames <- colnames(TextAnn)
#     if(nrow(events)!=0){
#       # rebuild tags -------------------------------------------------------------
#       tag <- character(0)
#       for (k in 1:nrow(events)) {
#         tag[k] <- stringr::str_c("1",":",TextAnn[k,1],"_")
#         for (l in 2:ncol(TextAnn)) {
#           if(is.na(TextAnn[k,l])==FALSE){
#             tag[k] <- stringr::str_c(tag[k],l,":", colnames[l],":",TextAnn[k,l],"_")
#           }else{next()}
#         }
#         tag[k] <- stringr::str_replace(tag[k], "_$",";")
#       }
#       events$tag =tag
#
#       # merge annotations that belong to the same word together -----------------
#       if(nrow(events)>1){
#         u <- 2
#         doubleRow <- numeric(0)
#         for (u in 2:nrow(events)) {
#           if(events[u,"Start"]==events[u-1,"Start"]){
#             events[u,"tag"] <- stringr::str_c(events[u-1,"tag"],events[u,"tag"])
#             doubleRow <- c(doubleRow, u-1)
#           }
#         }
#         events <- events[-(doubleRow),]
#       }
#       # add events to tier ------------------------------------------------------
#       if(nrow(events)!=0){
#         for (i in 1:nrow(events)) {
#           xml2::xml_add_child(xml2::xml_find_first(file,XPath) ,"event", start= events[i,"Start"], end=events[i,"End"])
#           xml2::xml_set_text(xml2::xml_child(xml2::xml_find_first(file,".//tier[@type='a']"),i), events[i,"tag"])
#         }
#       }
#     }
#   }
#   fileName <- stringr::str_remove(basename(PathExb), "\\.exb")
#   PathNewFile <- stringr::str_glue(PathNewFile,"\\",stringr::str_trim(fileName),suffix,".exb")
#   xml2::write_xml(file, PathNewFile)
# }



# just possible if events are the same
# for (i in 1:nrow(exb)) {
#   xml2::xml_set_text(xml2::xml_find_all(
#     file,
#     paste0(
#       "/basic-transcription/basic-body/tier[@id=\'",
#       stringr::str_trim(exb[i, "TierID"]),
#       "\']/event[@start=\'",
#       stringr::str_trim(exb[i, "Start"]),
#       "\']"
#     )
#   ), as.character(exb[i, "Text"]))
#}
##write back annotations
#only works if name of tiers still are columnames
# for (k in  names(exb)[names(exb) %in% xml2::xml_attr(xml2::xml_find_all(file, ".//tier[@type='a']"),
#                                                "display-name")]) {
#   ann <- exb %>% dplyr::filter(!is.na(.data[[k]]))
#   for (p in 1:nrow(ann)) {
#     xml2::xml_set_text(xml2::xml_find_all(
#       file,
#       paste0(
#         "/basic-transcription/basic-body/tier[@display-name=\'",
#         stringr::str_trim(k),
#         "\']/event[@start=\'",
#         stringr::str_trim(ann[p, "Start"]),
#         "\']"
#       )
#     ), as.character(ann[p, k]))
#   }
# }
