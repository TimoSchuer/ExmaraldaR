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
      annotations <- exb
    } else{
      annotations <-
        utils::read.delim(
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
    if(file %>% xml2::xml_find_all("//tier[@type='t']") %>% xml2::xml_attr("id") %>% setdiff(exb$TierID %>% unique()) %>% length()!= 0){
      tIDs <- c()
      for (l in file %>% xml_find_all("//tier[@type='t']") %>% xml2::xml_attr("id") %>% setdiff(exb$TierID %>% unique())) {
       tIDs <-  file %>% xml2::xml_find_all(paste0("//tier[@id='",l,"']", sep="")) %>% xml2::xml_children() %>% xml2::xml_attr("start") %>% c(tIDs,.)
       tIDs <-  file %>% xml2::xml_find_all(paste0("//tier[@id='",l,"']", sep="")) %>% xml2::xml_children() %>% xml2::xml_attr("end") %>% c(tIDs,.) %>% unique()
      }
      #d <- file %>% xml2::xml_find_all("//common-timeline") %>% xml_children() %>% xml_attr("id") %>% length()
    timeline_old <- data.frame(row.names = seq(1:xml2::xml_find_all(file,"//common-timeline") %>% xml_children() %>% xml_attr("id") %>% length())) %>%
      mutate(id= file %>% xml2::xml_find_all("//common-timeline") %>% xml_children() %>% xml_attr("id")) %>%
      mutate(time=file %>% xml2::xml_find_all("//common-timeline") %>% xml_children() %>% xml_attr("time") %>% as.double())
    timeline_old$time <- timeline_old$time %>% as.numeric() %>% round(6)
    timeline_new <- c(exb$Start_time,exb$End_time) %>%
      round(6) %>%
      unique() %>%
      as.double() %>%
      data.frame(time=.)%>%
      arrange(time) %>%
      distinct() %>%
      dplyr::anti_join(timeline_old, by="time") %>%
      mutate(id=stringr::str_extract(timeline_old$id,"\\d+") %>% as.numeric() %>% max()+ row_number()) %>%
      mutate(id= paste("T", id, sep=""))
    timeline <- bind_rows(timeline_old, timeline_new) %>% arrange(time) %>%
      mutate(id_new= paste0("T",dplyr::row_number()-1))
      exb <- exb %>% mutate(Start_time= round(Start_time,6)) %>%  left_join(timeline[,2:3], by= c("Start_time"="time")) %>% rename(Start_new=id_new)
      exb <- exb %>% mutate(End_time = round(End_time,6)) %>% left_join(timeline[,2:3], by= c("End_time"="time")) %>% rename(End_new= id_new)
      file %>% xml2::xml_find_all("//common-timeline") %>% xml2::xml_children() %>% xml2::xml_remove(free = TRUE)
      purrr::walk2(timeline$time,timeline$id, .f = \(x,y)  {xml2::xml_add_child(xml2::xml_find_all(file,"//common-timeline"), "tli", .copy = FALSE) %>% xml2::xml_set_attrs(c("id"=y, "time"=x))})
      purrr::walk2(timeline$id,timeline$id_new, .f=\(x,y) {xml_find_all(file,paste("//event[@start='",x,"']", sep="")) %>% xml_set_attr("start", y)})
      purrr::walk2(timeline$id,timeline$id_new, .f=\(x,y) {xml_find_all(file,paste("//event[@end='",x,"']", sep="")) %>% xml_set_attr("end", y)})
    }else{
      timeline <- c(exb$Start_time,exb$End_time) %>% round(6) %>% unique() %>% as.double() %>% data.frame(time=.)%>% arrange(time) %>% distinct() %>% mutate(id=paste("T",row_number(), sep=""))
    #assign new stime stamps
      exb <- exb %>% mutate(Start_time= round(Start_time,6)) %>%  left_join(timeline, by= c("Start_time"="time")) %>% rename(Start_new=id)
      exb <- exb %>% mutate(End_time = round(End_time,6)) %>% left_join(timeline, by= c("End_time"="time")) %>% rename(End_new= id)
       file %>% xml2::xml_find_all("//common-timeline") %>% xml2::xml_children() %>% xml2::xml_remove(free = TRUE)

      purrr::walk2(timeline$time,timeline$id, .f = \(x,y)  {xml2::xml_add_child(xml2::xml_find_all(file,"//common-timeline"), "tli", .copy = FALSE) %>% xml2::xml_set_attrs(c("id"=y, "time"=x))})
    }
    ##write back transcription tiers
    ##all transcription tiers not present in the dataset will be removed TODO: Lösung finden

    for (t in unique(exb$TierID)) {
      tier <- exb %>% filter(TierID==t)
      if(length(xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")))!=0){
        xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")) %>% xml2::xml_children() %>% xml2::xml_remove(free = TRUE)

        purrr::walk2(tier$Start_new,tier$End_new, .f = \(x,y)  {xml2::xml_add_child(xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")), "event", .copy=FALSE) %>%
            xml2::xml_set_attrs(c("start"=x, "end"=y))})

        xml2::xml_find_all(file,paste0("//tier[@id=","'",t,"']")) %>% xml2::xml_children() %>% xml2::xml_set_text(tier %>% pull({{transcription_text}})) #%>% xml_set_attrs(c("start"= tier$Start_new, "end"=tier$End_new))
        }
    }





    ##write annotations back
    #annotation_colums <- exb %>% select(14:18) %>% names()


    if(overwrite_annotations==TRUE){
      xml2::xml_find_all(file,"//tier[@type='a']") %>% xml2::xml_remove(free = TRUE)
    }
    #extraxt Tier numbers to assign unique Tier numbers
    #tierNumbers <- xml2::xml_find_all(file,"//tier") %>% xml2::xml_attr("id") %>% stringr::str_extract("\\d+")
    if(!any(is.na(annotation_colums))){
      for(ann in annotation_colums){
        tierNumbers <- xml2::xml_find_all(file,"//tier") %>% xml2::xml_attr("id") %>% stringr::str_extract("\\d+") %>% as.numeric() %>% max(na.rm = TRUE) +1
        tierId <-  paste0("TIE",tierNumbers, collapse = "")
        if(assignSpeakersAnnotation==FALSE) {
          xml2::xml_child(file, 2) %>%
            xml2::xml_add_child("tier") %>%
            xml2::xml_set_attrs(c("id"=tierId, "type"="a", "category"=ann))
          AnnTier <- exb %>% filter(!is.na(.data[[ann]]))
          purrr::walk2(AnnTier$Start_new,AnnTier$End_new, .f = \(x,y)  {xml2::xml_add_child(xml2::xml_find_all(file,paste0("//tier[@id=","'",tierId,"']")), "event", .copy=FALSE) %>%
               xml2::xml_set_attrs(c("start"=x, "end"=y))})
          xml2::xml_find_all(file,paste0("//tier[@id=","'",tierId,"']")) %>% xml2::xml_children() %>% xml2::xml_set_text(AnnTier %>% pull({{ann}})) #%>% xml_set_attrs(c("start"= tier$Start_new, "end"=tier$End_new))
       }
      }
    }

    fileName <- stringr::str_remove(basename(PathExb), "\\.exb")
    PathNewFile <-      stringr::str_glue(PathNewFile,
                        "\\",
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
