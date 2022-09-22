read_descriptions <- function(file,exb){
  DescriptionTiers <- xml2::xml_find_all(file,".//tier[@type='d']") #findet alle Annotationsspuren
  descriptions <- data.frame()
  for (n in 1:length(DescriptionTiers)) {
    des_help <- data.frame()
    ##TODO: nur event children Berücksichtigendas
    if(DescriptionTiers[n] %>% xml2::xml_children() %>% length()==0|DescriptionTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows() %>% names() %>% length()==0){
      next
    }else if(!"speaker" %in% names(xml2::xml_attrs(DescriptionTiers[n])[[1]])){ ##check for annTiers without speaker
      des_help <- DescriptionTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()%>% dplyr::rename(Start= start, End= end) %>%
        dplyr::mutate(Annotation=DescriptionTiers[n]  %>% xml2::xml_children() %>% xml2::xml_text()) %>%
        dplyr::mutate(Speaker= NA) %>%
        dplyr::mutate(TierID= xml2::xml_attrs(DescriptionTiers[n])[[1]][['id']]) %>%
        dplyr::mutate(Name=xml2::xml_attrs(DescriptionTiers[n])[[1]][['display-name']])
      descriptions <- dplyr::bind_rows(descriptions, des_help)
    }else{
      des_help <- DescriptionTiers[n] %>% xml2::xml_children() %>% xml2::xml_attrs() %>% dplyr::bind_rows()%>% dplyr::rename(Start= start, End= end) %>%
        dplyr::mutate(Annotation = DescriptionTiers[n]  %>% xml2::xml_children() %>% xml2::xml_text()) %>%
        dplyr::mutate(Speaker= xml2::xml_attrs(DescriptionTiers[n])[[1]][["speaker"]]) %>%
        dplyr::mutate(TierID= xml2::xml_attrs(DescriptionTiers[n])[[1]][['id']]) %>%
        dplyr::mutate(Name=xml2::xml_attrs(DescriptionTiers[n])[[1]][['display-name']])
      descriptions <- dplyr::bind_rows(descriptions, des_help)
    }
  }
  if(nrow(descriptions)==0){
   return(exb)
  }
  descriptions <- descriptions%>% tidyr::pivot_wider(names_from = Name, values_from = Annotation, names_repair = "universal") %>% dplyr::select(!TierID) %>% dplyr::filter(!is.na(Start))
  ##join annnotations that are alligned by Start,End and Speaker
  #check if annotations are per spekaer or not speaker assigned
  if(length(dplyr::intersect(unique(exb$Speaker), unique(descriptions$Speaker)))==0){##keine Sprecher gleich
    exb <- dplyr::left_join(exb, descriptions, by=c("Start", "End"), suffix= c("",".y") ) %>% dplyr::select(-Speaker.y)
  }else if(dplyr::setequal(unique(exb$Speaker), unique(descriptions$Speaker))|length(dplyr::setdiff(unique(descriptions$Speaker), unique(exb$Speaker)))==0){ ##alle Sprecher gleich oder alle Sprecher der Annotationsspuren in Transkripionsspuren
    exb <- dplyr::left_join(exb, descriptions, by=c("Start", "End","Speaker"), suffix= c("",".y"))
  }else{##teils teils
    ##Speaker that have a annotation tier but not a transcription tier
    diff <- dplyr::setdiff(unique(descriptions$Speaker), unique(exb$Speaker))
    exb <- descriptions %>%  dplyr::filter(Speaker %in% diff) %>% dplyr::select(-Speaker) %>% dplyr::left_join(exb, ., by=c("Start", "End"), suffix= c("",".y") ) #assign them by time
    if ("Speaker.y" %in% names(exb)){ # some cosmetics
      exb <- exb%>% dplyr::select(-Speaker.y)
    }
    exb2 <- descriptions %>%  dplyr::filter(!Speaker %in% diff)  %>% dplyr::left_join(exb, ., by=c("Start", "End","Speaker"), suffix= c("",".y") ) # assinge them by time and speaker
    exb <- dplyr::bind_rows(exb,exb2) %>% dplyr::arrange(EventID)
  }
  ##check for annotations that are left out
  if(nrow(dplyr::anti_join(descriptions,exb, by=c("Start", "End")))!=0){
    ##Möglichkeiten: ID vergeben und dann mergen oder nested tibble
    ##ID VERGEBEN:
    ##ToDO: ID vor Annotation setzen statt ID Spalte
    ##Idee: mutate across !Start,End;Speaker & !is.na; str_glue(ID,inhalt)
    ## ABER SUCHE DANN IRGENDWIE kompliziert...besser ID-Spalte pro Annotation?
    multiDes <-  dplyr::anti_join(descriptions,exb, by=c("Start", "End")) #%>% mutate(AnnID= seq(1:nrow(.)))
    #  exb <-exb %>%  mutate(AnnID=NA)
    desCols <- multiDes %>% dplyr::select(tidyselect:::where(~!all(is.na(.)))) %>%  names() %>% dplyr::intersect(names(exb)) %>% .[which(!stringr::str_detect(.,"Speaker|Start|End"))]
    multiDes_help <- data.frame()
    for (n in 1:length(desCols)) {
      varname <- stringr::str_glue(desCols[n],"_ID")
      multiDes_help2 <- data.frame()
      multiDes_help2 <- multiDes  %>%  dplyr::filter(.,!is.na(.data[[desCols[n]]])) %>% dplyr::mutate("{varname}":=seq(1:nrow(.)), .after= {desCols[n]})
      multiDes_help <- dplyr::bind_rows(multiDes_help, multiDes_help2)
    }
    multiDes <- multiDes_help
    remove(multiDes_help,multiDes_help2)
    for (k in 1:nrow(multiDes)) {
      desCols <- multiDes[k,] %>% dplyr::select(!tidyselect:::where(is.na)) %>% dplyr::select(-c(Start, End, Speaker)) %>%  names()
      if(length(which(exb$Start==as.character( multiDes[k,"Start"])))==0| length(which(exb$End==as.character(multiDes[k,"End"])))==0){
        print("The following description event is assigned to a speaker but has no element in the corresponding transcription file:")
        print(multiDes[k,])
        return(NULL)
      }
      a <- which(exb$Start==as.character( multiDes[k,"Start"])) %>% max()
      b <- which(exb$End==as.character(multiDes[k,"End"])) %>% max()

      exb[a:b,desCols] <- multiDes[k,desCols]
    }
  }
  return(exb)
}
