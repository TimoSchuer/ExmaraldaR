AddMinTier <- function(file){

  # Datei einlesen ----------------------------------------------------------
  content <- read_xml(file)
  transcriptions <- xml_find_all(content, "/basic-transcription/basic-body[1]/tier[@type='t' and @display-name != 'P [v]']") # findet alle Transkriptionszeilen; da ich immer eine Pausenzeile (benannt mit P) einfüge, wird diese ausgeschlossn

  # Spurbearbeitung ---------------------------------------------------------
  for(tier in transcriptions){
    events <- xml_children(tier)

    # Start und Endzeitpunkte auslesen ----------------------------------------
    start <- ""
    end <- ""
    for (k in 1:length(events)[1]) {
      start[k] <- xml_attr(events[k], "start")
      end[k] <- xml_attr(events[k],"end")
      k <- k+1
    }

    # Textformatierung --------------------------------------------------------
    xml_text(events) -> text #extrahiert Text
    sapply(text, str_to_lower) %>%
      sapply (.,str_remove_all, "=") %>%
      sapply(., str_replace_all, "[,.;:?!=]$","|") %>%
      sapply(., str_replace,"[,;?-]", "|") %>%
      sapply(., str_remove_all, ":") %>%
      sapply(., str_remove_all, "°") %>%
      sapply(., str_replace_all,"\\[|\\]"," ") %>%
      sapply(., str_replace_all,"\\(\\.\\)", " ") %>%
      sapply(., str_remove_all, "<<[^>]+>") %>%
      sapply(., str_remove_all, ">") %>%
      sapply(., str_remove_all, "\\(\\([^)]+\\)\\)") %>%
      sapply(., str_trim) %>%
      sapply(., str_squish) %>%
      sapply(., str_replace_all,"\\(-\\)", " ") -> text #entfernt/ersetzt Zeichen aus dem Basistranskript, bereinigt von unnötigen Lehrzeichen

    # Spureiegenschaften auslesen --------------------------------------------
    TierAttr <- xml_attrs(tier)#Spureigenschaften auslesen
    xml_find_all(content, "/basic-transcription/basic-body[1]/tier") %>%
      xml_length() %>%
      length()-> NumTier #Anzahl der Spuren auslesen
    displName <- str_extract(TierAttr[["display-name"]], "^[A-Za-z]+[^ ]") # Anzeigename auslesen

    # Transkriptionsspur hinzufügen -------------------------------------------
    nodeTier <- NumTier+1
    xml_add_sibling(xml_child(xml_child(content, 2), nodeTier), "tier", id = str_glue("TIE",NumTier), speaker = TierAttr["speaker"], category ="min",type ="t")
    nodeTier <- nodeTier +1
    xml_set_attr(xml_child(xml_child(content, 2), nodeTier), "display-name", str_glue(displName," [min]"))

    # bearbeitete Events einfügen ---------------------------------------------
    for (n in 1:length(events)[1]){
      xml_add_child(xml_child(xml_child(content, 2), nodeTier), "event")
      xml_set_attr(xml_child(xml_child(xml_child(content, 2), nodeTier), n), "start", start[n])
      xml_set_attr(xml_child(xml_child(xml_child(content, 2), nodeTier), n), "end", end[n])
      xml_set_text(xml_child(xml_child(xml_child(content, 2), nodeTier), n), text[n])
      n <- n+1
    }

  }
  # Neue Datei schreiben ----------------------------------------------------

  str_extract(file, ".+[:punct:]") %>%
    str_remove(., "\\.") %>%
    str_glue(., "_Min.exb") -> file_new # Wenn umgewandelte Dateien in anderen Ordner: vor Dateinamen zusätzlich Pfad eingeben (ACHTUNG: Vorwärtsslash beachten)
  write_xml(content, file_new)
  remove(content, n, events, tier, TierAttr,transcriptions,displName,end, file,file_new,k,nodeTier,NumTier,start,text)


}
