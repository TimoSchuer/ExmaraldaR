#' merge_exb_files
#' Merges several exb files into one xml elemen
#' @param files
#'
#' @return returns a xml element
#' @export
#'
#' @examples
merge_exb_files <- function(files=list()){
  file <- xml2::read_xml(files[1], encoding="UTF-8")
  #tiersAttr <- xml_attrs( xml_children(xml_child(file, 2))[-1])
  for (f in files[-1]) {
    appFile <- xml2::read_xml(f, encoding="UTF-8")
  #append timeline elements
    appTimeline <- xml2::xml_children(xml2::xml_child(appFile, 2))[1]
    xml2::xml_add_child(xml2::xml_children(xml2::xml_child(file, 2))[1], xml2::xml_children(appTimeline))
  #append events to other tiers
    appBody <- xml2::xml_children(xml2::xml_child(appFile, 2))[-1]

    for (t in appBody) {
      xPath <- stringr::str_glue("//tier[@display-name='",stringr::str_remove(xml2::xml_attrs(t)[["display-name"]],"'"),"']")
      if(length(xml2::xml_find_all(file, xPath))==0){
        xml2::xml_add_child(xml2::xml_child(file, 2),xml2::read_xml(as.character(t)))
      }else if(length(xml2::xml_children(t))==0){
        next
      }else{
        xml2::xml_add_child(xml2::xml_find_all(file, xPath), xml2::xml_children(t))
      }
    }
  }
  test <- as.character(file) %>% stringr::str_split(.,"\\n")
  test <- test[[1]]
  file <- test[!(which(stringr::str_starts(test,"\\s*<event|\\s*<tli") ) %in% which(duplicated(test)))]
  file <- paste0(file, collapse= "\n")
  file <- read_xml(file)
  return(file)
}
