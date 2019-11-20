read_metadata <- function(path){
  file <- xml2::read_xml(path) #read file;
  SpeakerTable <- xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 1), 2)) #read Speakertable

# Set up dataframe --------------------------------------------------------
  Attributes <- data.frame(matrix(ncol = 6, nrow = length(SpeakerTable)))
  colnames(Attributes) <- c("Speaker","sex","languagesUsed", "L1", "L2","comment")

# Read first line ---------------------------------------------------------


  Speaker <- xml2::xml_children(SpeakerTable[1])
  Attributes[1,1] <- unname(xml2::xml_attrs(SpeakerTable[1]))
  Attributes[1,2] <- xml2::xml_attr(Speaker[2], "value")
  # Attributes[1,3] <- paste(xml2::xml_attr(xml2::xml_children(Speaker[3]), "lang"), collapse = ",")
  # Attributes[1,4] <- xml2::xml_attr(xml2::xml_children(Speaker[4]), "lang")
  # Attributes[1,5] <- xml2::xml_attr(xml2::xml_children(Speaker[5]), "lang")
  if(length(xml2::xml_attr(xml2::xml_children(Speaker[3]), "lang"))!=0){
    Attributes[1,3] <- paste(xml2::xml_attr(xml2::xml_children(Speaker[3]), "lang"), collapse = ",")
  }
  if(length(xml2::xml_attr(xml2::xml_children(Speaker[4]), "lang"))!=0){
    Attributes[1,4] <- xml2::xml_attr(xml2::xml_children(Speaker[4]), "lang")
  }
  if(length(xml2::xml_attr(xml2::xml_children(Speaker[5]), "lang"))!=0){
    Attributes[1,5] <- xml2::xml_attr(xml2::xml_children(Speaker[5]), "lang")
  }
  Attributes[1,6] <- xml2::xml_text(Speaker[7])
  SpkAttr <- unname(unlist(xml2::xml_attrs(xml2::xml_children(Speaker[6])))) # read own categories in spekaertabel
  ValSpkAttr <- xml2::xml_text(xml2::xml_children(Speaker[6]))

# Sets up own categories as collums and fills in values -------------------


  if(length(SpkAttr)!=0){
    for (p in 1:length(SpkAttr)) {
      Attributes[1,SpkAttr[p]] <- ValSpkAttr[p]
    }
  }
  if(length(SpeakerTable)>1){
  k <- 2
  for (k in 2:length(SpeakerTable)){
    Speaker <- xml2::xml_children(SpeakerTable[k])
    Attributes[k,1] <- unname(xml2::xml_attrs(SpeakerTable[k]))
    Attributes[k,2] <- xml2::xml_attr(Speaker[2], "value")
    if(length(xml2::xml_attr(xml2::xml_children(Speaker[3]), "lang"))!=0){
      Attributes[k,3] <- paste(xml2::xml_attr(xml2::xml_children(Speaker[3]), "lang"), collapse = ",")
    }
    if(length(xml2::xml_attr(xml2::xml_children(Speaker[4]), "lang"))!=0){
      Attributes[k,4] <- xml2::xml_attr(xml2::xml_children(Speaker[4]), "lang")
      }
    if(length(xml2::xml_attr(xml2::xml_children(Speaker[5]), "lang"))!=0){
      Attributes[k,5] <- xml2::xml_attr(xml2::xml_children(Speaker[5]), "lang")
    }
    Attributes[k,6] <- xml2::xml_text(Speaker[7])
    SpkAttr2 <- unname(unlist(xml2::xml_attrs(xml2::xml_children(Speaker[6]))))
    ValSpkAttr2 <- xml2::xml_text(xml2::xml_children(Speaker[6]))
    if(length(SpkAttr2)!=0){
      for (t in 1:length(SpkAttr2)) {
          Attributes[k, SpkAttr2[t]] <- ValSpkAttr2[t]
    }
  }
  }
  }
  Attributes[Attributes=="---unknown---"] <- NA
  return(Attributes)
}
