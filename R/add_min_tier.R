#' Converts a basic GAT2 Transcript to an Mininmal transcript
#'
#' @param file path of an exb file
#'
#' @return NULL saves file to original path with suffix _M;in
#' @export
#' @importFrom magrittr %>%
#'
#' @importFrom utils read.delim
#'
add_MinimalTier <- function(file){

  # Datei einlesen ----------------------------------------------------------
  content <- xml2::read_xml(file)
  transcriptions <- xml2::xml_find_all(content, "/basic-transcription/basic-body[1]/tier[@type='t' and @display-name != 'P [v]']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn

  # Spurbearbeitung ---------------------------------------------------------
  for(tier in transcriptions){
    events <- xml2::xml_children(tier)

    # Start und Endzeitpunkte auslesen ----------------------------------------
    start <- ""
    end <- ""
    for (k in 1:length(events)[1]) {
      start[k] <- xml2::xml_attr(events[k], "start")
      end[k] <- xml2::xml_attr(events[k],"end")
      k <- k+1
    }

    # Textformatierung --------------------------------------------------------
    text <- xml2::xml_text(events) #extrahiert Text
    text <- text %>% sapply(stringr::str_to_lower) %>%
      sapply (stringr::str_remove_all, "=") %>%
      sapply(stringr::str_replace_all, "[,.;:?!=]$","|") %>%
      sapply(stringr::str_replace,"[,;?-]", "|") %>%
      sapply(stringr::str_remove_all, ":") %>%
      #sapply(stringr::str_remove_all, "\\U209") %>%
      sapply(stringr::str_replace_all,"\\[|\\]"," ") %>%
      sapply(stringr::str_replace_all,"\\(\\.\\)", " ") %>%
      sapply(stringr::str_remove_all, "<<[^>]+>") %>%
      sapply(stringr::str_remove_all, ">") %>%
      sapply(stringr::str_remove_all, "\\(\\([^)]+\\)\\)") %>%
      sapply(stringr::str_trim) %>%
      sapply(stringr::str_squish) %>%
      sapply(stringr::str_replace_all,"\\(-\\)", " ")#entfernt/ersetzt Zeichen aus dem Basistranskript, bereinigt von unnötigen Lehrzeichen

    # Spureiegenschaften auslesen --------------------------------------------
    TierAttr <- xml2::xml_attrs(tier)#Spureigenschaften auslesen
    NumTier <- xml2::xml_find_all(content, "/basic-transcription/basic-body[1]/tier") %>%
      xml2::xml_length() %>%
      length() #Anzahl der Spuren auslesen
    displName <- stringr::str_extract(TierAttr[["display-name"]], "^[A-Za-z]+[^ ]") # Anzeigename auslesen

    # Transkriptionsspur hinzufügen -------------------------------------------
    nodeTier <- NumTier+1
    xml2::xml_add_sibling(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), "tier", id = stringr::str_glue("TIE",NumTier), speaker = TierAttr["speaker"], category ="min",type ="t")
    nodeTier <- nodeTier +1
    xml2::xml_set_attr(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), "display-name", stringr::str_glue(displName," [min]"))

    # bearbeitete Events einfügen ---------------------------------------------
    for (n in 1:length(events)[1]){
      xml2::xml_add_child(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), "event")
      xml2::xml_set_attr(xml2::xml_child(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), n), "start", start[n])
      xml2::xml_set_attr(xml2::xml_child(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), n), "end", end[n])
      xml2::xml_set_text(xml2::xml_child(xml2::xml_child(xml2::xml_child(content, 2), nodeTier), n), text[n])
      n <- n+1
    }

  }
  # Neue Datei schreiben ----------------------------------------------------

  fileNew <- stringr::str_extract(file, ".+[:punct:]") %>%
    stringr::str_remove( "\\.") %>%
    stringr::str_glue("_Min.exb") %>%   # Wenn umgewandelte Dateien in anderen Ordner: vor Dateinamen zusätzlich Pfad eingeben (ACHTUNG: Vorwärtsslash beachten)
    stringr::str_remove(., "\\.") %>%
    stringr::str_glue(., "_Min.exb")  # Wenn umgewandelte Dateien in anderen Ordner: vor Dateinamen zusätzlich Pfad eingeben (ACHTUNG: Vorwärtsslash beachten)
  xml2::write_xml(content, fileNew)
  #remove(content, n, events, tier, TierAttr,transcriptions,displName,end, file,fileNew,k,nodeTier,NumTier,start,text)


}
