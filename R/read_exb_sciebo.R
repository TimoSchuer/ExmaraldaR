read_evens_xml <- read_events <- function(file,name){
  transcriptions <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='t']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn
  event <- character(0)
  start <- character(0)
  end <- character(0)
  text <- character(0)
  speaker <- character(0)
  tierId <- character(0)
  name <- character(0)
  filename <- name
  for(tier in transcriptions){
    event <- xml2::xml_children(tier)
    text_help <- xml2::xml_text(event, trim= TRUE) #Transkriptionstext auslesen
    #text_help <- stringr::str_trim(text_help)
    tierAttrs <- xml2::xml_attrs(tier) #TierId, Sprecher und Sprechername auslesen
    # Start und Endzeitpunkte auslesen ----------------------------------------
    start_help <- xml2::xml_attrs(event) %>% sapply('[','start') %>% unname()
    end_help <- xml2::xml_attrs(event) %>% sapply('[','end') %>% unname()
    start <- c(start,start_help)
    end <- c(end,end_help)
    text <- c(text, text_help)
    speaker_help <- character(length(text_help))
    speaker_help <- paste(speaker_help, tierAttrs[['speaker']])
    speaker <- c(speaker, speaker_help)
    tierId_help <-  character(length(text_help))
    tierId_help <- paste(tierId_help, tierAttrs[['id']])
    tierId <- c(tierId,tierId_help)
    name_help <-  character(length(text_help))
    name_help <-  paste(name_help,tierAttrs[['display-name']])
    name <- c(name,name_help)
    filename_help <-  character(length(start))
    filename_help <- paste(filename_help, filename)
  }
  speaker <- stringr::str_trim(speaker)
  if(length(length(end)!=length(tierId))){#if theres an dummy element at the end of a splitted transcription adjust lenght
    name <- name[1:length(end)]
    tierId <- tierId[1:length(end)]
    speaker <- speaker[1:length(end)]
  }
  events <- data.frame(File = filename_help, Speaker = speaker, TierID = tierId, Name= name,Text= text,Start =unlist(unname(start)),End=unlist(unname(end)), stringsAsFactors = FALSE)
  return(events)
}
read_exb_xml <- function(file,name, readAnn=TRUE,annotation= c("linear", "multilayer"),addMetaData= FALSE,sortMetaData=TRUE){
  timeline <- read_timeline(file)
  events <- read_events(file, name)
  events[,5] <- stringr::str_trim(events[,5])
  events_sorted <- sort_events(events, timeline)
  events_sorted <- dplyr::left_join(events_sorted,timeline, by=c("Start" = "id")) %>% dplyr::rename(Start_time = time) #Add absolute timepoints for start
  events_sorted <- dplyr::left_join(events_sorted,timeline, by=c("End" = "id")) %>% dplyr::rename(End_time = time) #Add absolute timepoints for start
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
  return(exb)
}else {
  stop("File has to be an EXMARaLDA basis-transcription (.exb)")
}


read_exb_sciebo <- function(path_list,username, password, readAnn=TRUE,annotation= c("linear", "multilayer"),addMetaData= FALSE,sortMetaData=TRUE){

  addMetaDataDir <- addMetaData
  readAnnDir <- readAnn
  AnnotationDir <- annotation
  exb <- data.frame()
  names <- str_extract_all(path_list, "/[^/]*\\.exb") %>% str_remove_all("/")
  # exb <- read_exb_xml(path= unlist(stringr::str_c(pathDir,"\\",files[1])),annotation= AnnotationDir, addMetaData=addMetaDataDir, readAnn= readAnnDir , sortMetaData=FALSE)
  # k <- 2
  for (p in 1:length(path_list)) {
    uri <- URLencode(paste(path_list[p]))
    file <- httr::GET(uri, httr::authenticate(username,password)) %>% httr::content("raw") %>% readBin("character") %>% xml2::xml_read(encoding= "UTF-8")
    help <- read_exb_xml(file, name = names[p],readAnn = readAnnDir,annotation = AnnotationDir, addMetaData = addMetaDataDir)
    exb <- append(exb, help)
  }
  if(addMetaDataDir==TRUE){
    startMetaData <- which(colnames(exb)=="sex")
    k <- ncol(exb)
    l <- startMetaData-1
    n <- k-startMetaData

    exb <- exb[,c(1:6,startMetaData:k,7:l)]
  }
  return(exb)
}
