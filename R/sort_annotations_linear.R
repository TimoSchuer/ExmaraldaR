sort_anntotations_linear <- function(exb){

  # Get Tag Set an set up table ---------------------------------------------

  #tagSet <- xml2::read_xml(PathTagSet)
  #variableNames <- unlist(xml2::xml_attrs(xml2::xml_children(tagSet)))
  #annotation_sorted <- data.frame(matrix(ncol = length(variableNames),nrow = nrow(exb)), stringsAsFactors = FALSE)
  #colnames(annotation_sorted) <- variableNames
  # Check for more than one annotation to a word ----------------------------
  for (k in 1:nrow(exb)) {
    if(is.na(exb[k,10])== FALSE & stringr::str_detect(exb[k,10], ";[\\d\\s]")==TRUE){
      SplitAnn <- character(0)
      SplitAnn <- unlist(stringr::str_split(exb[k,10],";"))
      SplitAnn <- stringr::str_trim(SplitAnn[SplitAnn != ""])
      SplitAnn[1] <- stringr::str_remove(SplitAnn[1],";")
      exb[k,10] <- SplitAnn[1]
      l <- 2
      for (l in 2:length(SplitAnn)) {
        exb[seq(k+1,nrow(exb)+1),] <- exb[seq(k,nrow(exb)),]
        exb[k+1,] <- exb[k,]
        exb[k,10] <- SplitAnn[l]
      }
    }
  }
  # variableNames <- unlist(xml2::xml_attrs(xml2::xml_children(tagSet)))
  # variableNames <- stringr::str_remove_all(variableNames, " ")
  # get highest number of Variables to set up data frame
  VarNum <- stringr::str_extract_all(exb$Annotation, "[0-9]+")
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
      VTag <- unlist(stringr::str_split(TagSplit[1],":"))
      VTag <- stringr::str_c(VTag[2],":",VTag[3])
      annotation_sorted[k,1] <- VTag
      if(length(TagSplit)>1){
        i <- 2
        for (i in 2:length(TagSplit)) {
          TagAtomic <- unlist(stringr::str_split(TagSplit[i],":"))
          TagAtomic <- stringr::str_remove_all(TagAtomic, "\\W")
          #annotation_sorted[k,as.numeric(TagAtomic[1])] <- as.character(TagAtomic[3])
          tryCatch(annotation_sorted[k,as.numeric(TagAtomic[1])] <- as.character(TagAtomic[3]), error= function(e) print_error(exb,k))
          tryCatch(colnames(annotation_sorted)[as.numeric(TagAtomic[1])] <- as.character(TagAtomic[2]), error= function(e) print_error(exb,k))
        }
      }
    }
  }
  colnames(annotation_sorted)[1] <- "Variable"
  annotation_sorted <- dplyr::bind_cols(exb, annotation_sorted)
  annotation_sorted <- dplyr::select(annotation_sorted,-(Annotation))
  return(annotation_sorted)
}
