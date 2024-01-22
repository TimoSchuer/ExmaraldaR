#' Title
#'
#' @param path path of .exb file
#' @param readAnn logical, if annotation tiers should be read
#' @param addDescription logical, if description tiers should be read NOT YET POSSIBLE
#' @param addMetaData logical, if information from speaker table should be read
#' @param addIPNumber logical, if there should be an nummeration of intonation phrases
#' @param addPaths adding path of file and referenced audio
#' @param IPEndSign character, spezifies characters that indicate the end of a intonation unit
#'
#' @importFrom rlang :=
#' @return data.frame
#' @export

read_exb_file <- function(path,
                          readAnn=TRUE,
                          addDescription= FALSE,
                          addMetaData= FALSE,
                          addIPNumber=TRUE,
                          IPEndSign= c("|",".",";",",",",","?","=","-"),
                          addPaths=FALSE){
  if(stringr::str_ends(path, "\\.exb")== FALSE){
    return("File is not an .exb file")
  }else{
    file <- xml2::read_xml(path, encoding="UTF-8")
    timeline <- xml2::xml_attrs(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1))) %>%
      dplyr::bind_rows()
    events <- ExmaraldaR:::read_events(file, path) %>%  #read events
      dplyr::left_join(., timeline[,1:2], by= c("Start" ="id"), suffix= c("",".y") ) %>% dplyr::rename(Start_time= time)%>% dplyr::mutate(Start_time=as.double(Start_time)) %>%  #allocate absoulute times to time stamps
      dplyr::left_join(., timeline[,1:2], by= c("End" ="id"), suffix= c("",".y") ) %>% dplyr::rename(End_time= time) %>% dplyr::mutate(End_time= as.double(End_time)) %>%
      .[,c("File","Speaker", "TierID","TierCategory",  "Start","End", "Start_time", "End_time","Name","Text")] # nice and tidy order
    if (addPaths==TRUE) {
      events <- events %>%
        dplyr::mutate(pathFile=path, .after=File) %>%
        dplyr::mutate(pathAudio=xml2::xml_find_all(file,"//referenced-file") %>%
                        xml2::xml_attr("url") %>% stringr::str_flatten(), .after=pathFile)
    }
    events <- ExmaraldaR:::sort_events(events, addIPNumber= addIPNumber, IPEndSign= IPEndSign)
    if(readAnn==TRUE & length(xml2::xml_find_all(file,".//tier[@type='a']"))!=0){
      AnnotationTiers <- xml2::xml_find_all(file,".//tier[@type='a']") #findet alle Annotationsspuren
      annotations <- data.frame()
    for (n in 1:length(AnnotationTiers)) {
        ann_help <- data.frame()
        ##TODO: nur event children Berücksichtigendas
        if(AnnotationTiers[n] %>% xml2::xml_children() %>% length()==0|AnnotationTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows() %>% names() %>% length()==0){
          next
        }else if(!"speaker" %in% names(xml2::xml_attrs(AnnotationTiers[n])[[1]])){ ##check for annTiers without speaker
          ann_help <- AnnotationTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()%>% dplyr::rename(Start= start, End= end) %>%
            dplyr::mutate(Annotation=AnnotationTiers[n]  %>% xml2::xml_children() %>% xml2::xml_text()) %>%
            dplyr::mutate(Speaker= NA) %>%
            dplyr::mutate(TierID= xml2::xml_attrs(AnnotationTiers[n])[[1]][['id']]) %>%
            dplyr::mutate(Name=xml2::xml_attrs(AnnotationTiers[n])[[1]][['display-name']])
          annotations <- dplyr::bind_rows(annotations, ann_help)
        }else{
          ann_help <- AnnotationTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()%>% dplyr::rename(Start= start, End= end) %>%
            dplyr::mutate(Annotation = AnnotationTiers[n]  %>% xml2::xml_children() %>% xml2::xml_text()) %>%
            dplyr::mutate(Speaker= xml2::xml_attrs(AnnotationTiers[n])[[1]][["speaker"]]) %>%
            dplyr::mutate(TierID= xml2::xml_attrs(AnnotationTiers[n])[[1]][['id']]) %>%
            dplyr::mutate(Name=xml2::xml_attrs(AnnotationTiers[n])[[1]][['display-name']])
          annotations <- dplyr::bind_rows(annotations, ann_help)
        }
      }
      if(nrow(annotations)==0){
        exb <- events%>% dplyr::ungroup() %>%  dplyr::mutate(EventID= seq(1:nrow(.))) %>% as.data.frame()
        if(addIPNumber==TRUE){
          exb <- exb %>% dplyr::group_by(IPNumber) %>% dplyr::arrange(Start_time, .by_group = TRUE) %>% dplyr::ungroup()
        }else{
          exb <- exb %>% dplyr::arrange(Start_time)
        }
        return(exb%>% mutate(IPId=paste(File,IPNumber,sep = "_"))%>%  dplyr::mutate(exb, EventID=paste(File, EventID, sep= "_")) %>% dplyr::select(IPId,1:dplyr::last_col(offset = 1)))
      }
      if(events %>% dplyr::group_by(Speaker,TierCategory) %>%
         dplyr::count() %>%
         dplyr::ungroup() %>%
          dplyr::group_by(Speaker) %>%
          dplyr::count()%>%
         dplyr::filter(n>1) %>%
         nrow()>1){#check if multiple transcription tiers per speaker, that make allignment of annotations difficult;
        #if yes: return dataframes for transcription and annotations so that user can manually allign them
        print("There are multiple transcription tiers per Speaker, which might cause problems when alligning annotations. Return list with Dataframe for annotation and events to manually allign them.")
        if(addMetaData==TRUE){
          metaData <- read_metadata(path)
          exb <- dplyr::left_join(events,metaData, by="Speaker", suffix= c("",".y") ) %>% dplyr::select(1:Name, names(metaData),Text:tidyselect::last_col())
        }
        #add EventID
        if(addIPNumber==TRUE){
          exb <- events %>% dplyr::group_by(IPNumber) %>% dplyr::arrange(Start_time, .by_group = TRUE) %>% dplyr::ungroup()
        }else{
          exb <- events %>% dplyr::arrange(Start_time)
        }
        exb <- exb %>% dplyr::mutate(IPId=paste(File,IPNumber,sep = "_"), .before=1 ) %>% as.data.frame()
        exb <- list(transcription=exb,annotation=annotations)
        return(exb)
      }
      annotations <- annotations%>%
        tidyr::pivot_wider(names_from = Name, values_from = Annotation, names_repair = "universal") %>%
        dplyr::select(!TierID) %>% dplyr::filter(!is.na(Start))
      ##join annnotations that are alligned by Start,End and Speaker
      #check if annotations are per spekaer or not speaker assigned

      if(length(dplyr::intersect(unique(events$Speaker), unique(annotations$Speaker)))==0){##keine Sprecher gleich
        exb <- dplyr::left_join(events, annotations, by=c("Start", "End"),multiple="first", suffix= c("",".y") ) %>% dplyr::select(-Speaker.y)
      }else if(dplyr::setequal(unique(events$Speaker), unique(annotations$Speaker))|length(dplyr::setdiff(unique(annotations$Speaker), unique(events$Speaker)))==0){ ##alle Sprecher gleich oder alle Sprecher der Annotationsspuren in Transkripionsspuren
         exb <- dplyr::left_join(events, annotations, by=c("Start", "End","Speaker"), suffix= c("",".y"),multiple="first")
      }else{##teils teils
        ##Speaker that have a annotation tier but not a transcription tier
        diff <- dplyr::setdiff(unique(annotations$Speaker), unique(events$Speaker))
        exb <- annotations %>%  dplyr::filter(Speaker %in% diff) %>% dplyr::select(-Speaker) %>% dplyr::left_join(events, ., by=c("Start", "End"), suffix= c("",".y") ) #assign them by time
        if ("Speaker.y" %in% names(exb)){ # some cosmetics
          exb <- exb%>% dplyr::select(-Speaker.y)
        }
        exb2 <- annotations %>%  dplyr::filter(!Speaker %in% diff)  %>% dplyr::left_join(events, ., by=c("Start", "End","Speaker"),multiple="first", suffix= c("",".y") ) # assinge them by time and speaker
        exb <- dplyr::bind_rows(exb,exb2) %>% dplyr::arrange(EventID)
      }


      ##check for annotations that are left out
      if(nrow(dplyr::anti_join(annotations,events, by=c("Start", "End")))!=0){
        ##Möglichkeiten: ID vergeben und dann mergen oder nested tibble
        ##ID VERGEBEN:
        ##ToDO: ID vor Annotation setzen statt ID Spalte
        ##Idee: mutate across !Start,End;Speaker & !is.na; str_glue(ID,inhalt)
        ## ABER SUCHE DANN IRGENDWIE kompliziert...besser ID-Spalte pro Annotation?
       multiAnn <-  dplyr::anti_join(annotations,events, by=c("Start", "End")) #%>% mutate(AnnID= seq(1:nrow(.)))
     #  exb <-exb %>%  mutate(AnnID=NA)
       annCols <- multiAnn %>% dplyr::select(where(~!all(is.na(.)))) %>%  names() %>% dplyr::intersect(names(exb)) %>% .[which(!stringr::str_detect(.,"Speaker|Start|End"))]
       multiAnn_help <- data.frame()
       for (n in 1:length(annCols)) {
        varname <- stringr::str_glue(annCols[n],"_ID")
        multiAnn_help2 <- data.frame()
        multiAnn_help2 <- multiAnn  %>%  dplyr::filter(.,!is.na(.data[[annCols[n]]])) %>% dplyr::mutate("{varname}":=seq(1:nrow(.)), .after= {annCols[n]})
        multiAnn_help <- dplyr::bind_rows(multiAnn_help, multiAnn_help2)
       }
       multiAnn <- multiAnn_help
       remove(multiAnn_help,multiAnn_help2)
       for (k in 1:nrow(multiAnn)) {
         annCols <- multiAnn[k,] %>% dplyr::select(!where(is.na)) %>% dplyr::select(-c(Start, End)) %>%  names()
         a <- which(exb$Start==as.character( multiAnn[k,"Start"])) %>% max()
         b <- which(exb$End==as.character(multiAnn[k,"End"])) %>% max()
         exb[a:b,annCols] <- multiAnn[k,annCols]
       }
      }


    }else{
      exb <- events
    }
  }
  ##clean doubled lines and coerce annotations ##TODO: Verhalten beobachen
  exb <-  exb %>% dplyr::group_by(EventID) %>% dplyr::summarise(dplyr::across(tidyselect::everything(), ~dplyr::first(na.omit(.x)))) %>% dplyr::ungroup() %>% dplyr::arrange(IPNumber)
  if(addMetaData==TRUE){
    metaData <- read_metadata(file)
    exb <- dplyr::left_join(exb,metaData, by="Speaker", suffix= c("",".y") ) %>% dplyr::select(1:Name, names(metaData),Text:tidyselect::last_col())
  }
  #add EventID
  exb <- exb %>% dplyr::mutate(EventID= seq(1:nrow(exb))) %>% as.data.frame()
  if(addIPNumber==TRUE){
    exb <- exb %>% dplyr::group_by(IPNumber) %>% dplyr::arrange(Start_time, .by_group = TRUE) %>% dplyr::ungroup()
  }else{
    exb <- exb %>% dplyr::arrange(Start_time)
  }
  return(exb%>% dplyr::mutate(IPId=paste(File,IPNumber,sep = "_"))%>%  dplyr::mutate( EventID=paste(File, EventID, sep= "_")) %>% dplyr::select(IPId,1:dplyr::last_col(offset = 1)))
}

