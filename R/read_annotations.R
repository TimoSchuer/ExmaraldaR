read_annotations <- function(file,events){
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
    exb <- events
    #return(exb)
  }
  annotations <- annotations%>% tidyr::pivot_wider(names_from = Name, values_from = Annotation, names_repair = "universal") %>% dplyr::select(!TierID) %>% dplyr::filter(!is.na(Start))
  ##join annnotations that are alligned by Start,End and Speaker
  #check if annotations are per spekaer or not speaker assigned
  if(length(dplyr::intersect(unique(events$Speaker), unique(annotations$Speaker)))==0){##keine Sprecher gleich
    exb <- dplyr::left_join(events, annotations, by=c("Start", "End"), suffix= c("",".y") ) %>% dplyr::select(-Speaker.y)
  }else if(dplyr::setequal(unique(events$Speaker), unique(annotations$Speaker))|length(dplyr::setdiff(unique(annotations$Speaker), unique(events$Speaker)))==0){ ##alle Sprecher gleich oder alle Sprecher der Annotationsspuren in Transkripionsspuren
    exb <- dplyr::left_join(events, annotations, by=c("Start", "End","Speaker"), suffix= c("",".y"))
  }else{##teils teils
    ##Speaker that have a annotation tier but not a transcription tier
    diff <- dplyr::setdiff(unique(annotations$Speaker), unique(events$Speaker))
    exb <- annotations %>%  dplyr::filter(Speaker %in% diff) %>% dplyr::select(-Speaker) %>% dplyr::left_join(events, ., by=c("Start", "End"), suffix= c("",".y") ) #assign them by time
    if ("Speaker.y" %in% names(exb)){ # some cosmetics
      exb <- exb%>% dplyr::select(-Speaker.y)
    }
    exb2 <- annotations %>%  dplyr::filter(!Speaker %in% diff)  %>% dplyr::left_join(events, ., by=c("Start", "End","Speaker"), suffix= c("",".y") ) # assinge them by time and speaker
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
      annCols <- multiAnn[k,] %>% dplyr::select(!where(is.na)) %>% dplyr::select(-c(Start, End, Speaker)) %>%  names()
      if(length(which(exb$Start==as.character( multiAnn[k,"Start"])))==0| length(which(exb$End==as.character(multiAnn[k,"End"])))==0){
        print("The following annotation event is assigned to a speaker but has no element in the corresponding transcription file:")
        print(multiAnn[k,])
        return(NULL)
      }
      a <- which(exb$Start==as.character( multiAnn[k,"Start"])) %>% max()
      b <- which(exb$End==as.character(multiAnn[k,"End"])) %>% max()
      exb[a:b,annCols] <- multiAnn[k,annCols]
    }
  }
return(exb)
}
