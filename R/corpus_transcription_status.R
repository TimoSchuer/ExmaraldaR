##TODO works only for wav; implement more file types

#' corpus_transcription_status
#'
#' @param path_files Directory of corpus, audios and transcriptions must be stored here
#' @param subDirs TRUE if Corpus is stored in subdirectories
#' @param summarize If TRUE summary (list) is given back, otherwise list of all files and transcriptions
#'
#' @return data.frame or list
#' @export
#'
#' @examples
#'
corpus_transcription_status <- function(path_files, subDirs=FALSE, summarize= TRUE){
  exb_files <- list.files(path_files, pattern = "\\.exb",full.names = TRUE, recursive = subDirs)
  audio_files <- list.files(path = path_files, pattern= "\\.wav|\\.mp3|\\.ogg", recursive = subDirs, full.names = TRUE)
  referenced_audio <- character()
  transcribed_time <- integer()
  for (i in 1:length(exb_files)) {
    file <- xml2::read_xml(exb_files[i])
    timeline <- xml2::xml_attrs(xml2::xml_children(xml2::xml_child(xml2::xml_child(file, 2), 1)))
    withCallingHandlers( timeline <-  do.call(rbind, timeline), warning =  function(w) if(grepl("number of columns of result is not a multiple of vector length", w, fixed= TRUE)) invokeRestart( "muffleWarning" ))
    timeline <- timeline |>  as.data.frame()
    used_tli <- xml_find_all(file, "//event") |> xml_attrs()
    withCallingHandlers( used_tli <-  do.call(rbind, used_tli), warning =  function(w) if(grepl("number of columns of result is not a multiple of vector length", w, fixed= TRUE)) invokeRestart( "muffleWarning" ))
    used_tli <- used_tli |> as.character() |> unique()
    timeline <- timeline[which(timeline$id %in% used_tli),]
    referenced_audio[i] <-  xml2::read_xml(exb_files[i]) |>  xml2::xml_find_first("//referenced-file") |>  xml2::xml_attr("url")  |> stringr::str_split("\\/") |> lapply(tail, n=1)
    transcribed_time[i] <- max(as.integer(timeline$time)) -  min(as.integer(timeline$time))
  }
  referenced_audio <- unlist(referenced_audio)
  for (l in 1:length(referenced_audio)) {
    if(length(which(stringr::str_ends(audio_files, stringr::fixed(referenced_audio[l]))))!=0)    {
      referenced_audio[l] <- audio_files[stringr::str_ends(audio_files, stringr::fixed(referenced_audio[l]))]
    }else{
      err <- paste(referenced_audio[l],"does not exist. Please check if the audio file is correctly linked to the transcription")
      referenced_audio[l] <- NA
      print(err)
    }

  }
  #referenced_audio <- audio_files[stringr::str_ends(audio_files, paste0(referenced_audio, collapse = "|"))]
  length_audio <- integer()
#  audio_files <- audio_files[stringr::str_ends(audio_files, "wav")]
    for (k in 1:length(audio_files)) {
        if(stringr::str_ends(audio_files[k],"wav")==FALSE){
          err <- paste(audio_files[k], "is not a wav file and will be ignored")
          print(err)
          length_audio[k] <- NA
          k <- k+1
        }
       sound <-  tuneR::readWave(audio_files[k], header = TRUE)
      length_audio[k] <-round(sound[["samples"]]/sound[["sample.rate"]], 2)
    }
  CorpInfo <- data.frame(transcript=exb_files, audio_files= referenced_audio, transcribed_time=transcribed_time)
  audios <- data.frame(audio_files= audio_files, length_audio= length_audio)
  CorpInfo <- dplyr::left_join(audios, CorpInfo, by= "audio_files")
  CorpInfo[is.na(CorpInfo$transcribed_time), "transcribed_time"] <- 0
  CorpInfo <- dplyr::mutate(CorpInfo, percentage_transcribed= transcribed_time/length_audio)
  if(summarize== TRUE){
    summary <- list()
    summary$NumberOfAudios <- nrow(CorpInfo)
    summary$NumberOfTranscripts <- dplyr::filter(CorpInfo, !(is.na(transcript))) |> nrow()
    summary$lengthAudio <- sum(CorpInfo$length_audio, na.rm = TRUE)
    summary$transcribedAudio <- sum(CorpInfo$transcribed_time, na.rm = TRUE)
    summary$percentageTranscribed <- summary$transcribedAudio/summary$lengthAudio
    return(summary)
  }else{
    return(CorpInfo)
  }
}



