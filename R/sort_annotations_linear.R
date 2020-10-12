#' Title
#'
#' @param exb data.frame containing sorted events and raw linear annotations
#'
#' @return
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE) # for a linear annotation
#'      file <- xml2::read_xml(path, encoding="UTF-8")
#'      timeline <- read_timeline(file)
#'      events <- read_events(file, path)
#'  events_sorted <- sort_events(events, timeline)
#'  events_sorted <- add_IpNumber(events_sorted)
#'  annotations <- read_annotations_linear(file= xml2::read_xml(path, encoding="UTF-8"))
#'  exb <- dplyr::left_join(events_sorted, annotations,by = c("Speaker", "Start", "End"))
#'  MultiEventAnn <- dplyr::anti_join( annotations,events_sorted, by=c("Speaker", "Start", "End")) # check for annotations for more than 1 event
#'  if(nrow(MultiEventAnn)!=0){
#'  for (n in 1:nrow(MultiEventAnn)) {
#'  a <- which(events_sorted[,'Start']==MultiEventAnn[n, 'Start'])
#'  b <- which(events_sorted[,'End']==MultiEventAnn[n, 'End'])
#'  exb[a:b,colnames(MultiEventAnn)[ncol(MultiEventAnn)]] <- MultiEventAnn[n, ncol(MultiEventAnn)]
#'  }
#'  }
#'  sort_anntotations_linear(exb)
sort_anntotations_linear <- function(exb){

  # Get Tag Set an set up table ---------------------------------------------

  #tagSet <- xml2::read_xml(PathTagSet)
  #variableNames <- unlist(xml2::xml_attrs(xml2::xml_children(tagSet)))
  #annotation_sorted <- data.frame(matrix(ncol = length(variableNames),nrow = nrow(exb)), stringsAsFactors = FALSE)
  #colnames(annotation_sorted) <- variableNames
  # Check for more than one annotation to a word ----------------------------
  if(all(is.na(exb$Annotation)==TRUE)){
    return(exb)
  }else{
    #check if there are more than one linear annotation ending with ";" in one event, if so split them up
    for (k in 1:nrow(exb)) {
      if(is.na(exb[k,"Annotation"])== FALSE & stringr::str_detect(exb[k,"Annotation"], ";[\\d\\s]")==TRUE){
        SplitAnn <- character(0)
        SplitAnn <- unlist(stringr::str_split(exb[k,"Annotation"],";"))
        SplitAnn <- stringr::str_trim(SplitAnn[SplitAnn != ""])
        SplitAnn[1] <- stringr::str_remove(SplitAnn[1],";")
        exb[k,"Annotation"] <- SplitAnn[1]
        l <- 2
        for (l in 2:length(SplitAnn)) {
          exb[seq(k+1,nrow(exb)+1),] <- exb[seq(k,nrow(exb)),]
          exb[k+1,] <- exb[k,]
          exb[k,"Annotation"] <- SplitAnn[l]
        }
      }
    }
    # variableNames <- unlist(xml2::xml_attrs(xml2::xml_children(tagSet)))
    # variableNames <- stringr::str_remove_all(variableNames, " ")
    # get highest number of Variables to set up data frame
    suppressWarnings(VarNum <- stringr::str_extract_all(exb$Annotation, "_[0-9]+\\:{1}") %>%  stringr::str_extract_all("\\d"))
    VarNum <- lapply(VarNum, as.numeric)
    VarNum <- max(unlist(VarNum),na.rm=TRUE)
    annotation_sorted <- data.frame(matrix(ncol = VarNum,nrow = nrow(exb)), stringsAsFactors = FALSE)
    #colnames(annotation_sorted) <- variableNames
    VecAnn <- exb$Annotation
    for (k in 1:length(VecAnn)) {
      if(is.na(VecAnn[k])==TRUE|VecAnn[k]==""){
        next()
      }else{
        TagSplit <- unlist(stringr::str_split(VecAnn[k], "_"))
        # VTag <- unlist(stringr::str_split(TagSplit[1],":"))
        # VTag <- stringr::str_c(VTag[2],":",VTag[3])
        # annotation_sorted[k,1] <- VTag
        #if(length(TagSplit)>1){
          #i <- 2
          for (i in 1:length(TagSplit)) {
            TagAtomic <- unlist(stringr::str_split(TagSplit[i],":"))
            TagAtomic <- stringr::str_remove_all(TagAtomic, "\\W")
            #annotation_sorted[k,as.numeric(TagAtomic[1])] <- as.character(TagAtomic[3])
            tryCatch(annotation_sorted[k,as.numeric(TagAtomic[1])] <- as.character(TagAtomic[3]), error= function(e) print_error(exb,k))
            tryCatch(colnames(annotation_sorted)[as.numeric(TagAtomic[1])] <- as.character(TagAtomic[2]), error= function(e) print_error(exb,k))
          }
        #}
      }
    }
    #colnames(annotation_sorted)[1] <- "Variable"
    annotation_sorted <- dplyr::bind_cols(exb, annotation_sorted)
    annotation_sorted <- dplyr::select(annotation_sorted,-(Annotation))
    return(annotation_sorted)
  }
}
