
#' read_events()
#'
#' Reads events from an .exb transcription file
#'
#' @param file  xml Object from an .exb transcription file
#' @param path path of the .exb transcription file
#'
#' @return Returns data.frame containing all Information; events are not sortetd
#' @export
#'
#' @examples
#'  path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE)
#'  file <- xml2::read_xml(path, encoding="UTF-8")
#' read_events(file, path)
read_events <- function(file, path){
  transcriptions <- xml2::xml_find_all(file, "/basic-transcription/basic-body[1]/tier[@type='t']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn
  event <- character(0)
  start <- character(0)
  end <- character(0)
  text <- character(0)
  speaker <- character(0)
  tierId <- character(0)
  name <- character(0)
  filename <- stringr::str_remove(basename(path), "\\.exb")
  for(tier in transcriptions){
    event <- xml2::xml_children(tier)
    text_help <- xml2::xml_text(event, trim= FALSE) #Transkriptionstext auslesen
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
