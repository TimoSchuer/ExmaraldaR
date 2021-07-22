#' view_event
#' View event in Praat
#' @param exb exb object as created by exb_read_file or exb_read_dir; can also be a subset
#' @param rowEvent row of the event you want to play
#' @param pathFileDir path of the audio file dir (, to provide dir is useful if there are several audio files in one corpus;has to be .wav at the moment)
#' @param pathPraat path of Praat.exe; if not provided it will be assumend praat is in the working directory
#'
#' @return NULL
#' @export
#'
#' @examples
view_event <- function(exb, rowEvent= 1, pathFileDir= character(0),pathPraat= getwd(), pathSendPraat= getwd()){
  if(identical(pathFileDir, character(0))){
    pathFile <- paste(getwd(),"/",stringr::str_trim(exb$File[rowEvent]), ".wav", sep= "" )
  }else{
    pathFile <- paste(pathFileDir,"/",stringr::str_trim(exb$File[rowEvent]), ".wav", sep= "" )
  }
  line1 <- paste0("Open long sound file: \"",str_replace_all(pathFile,"\\\\", "/"),"\"")
  line2 <- paste0("editor: \"LongSound ",stringr::str_trim(exb$File[rowEvent]), "\"")
  line3 <-  paste0("Zoom: ",exb$Start_time[rowEvent], ", ",exb$End_time[rowEvent], sep="")
  praat <- paste(line1,"View",line2,line3, sep="\n")
  write(praat, file= paste(pathPraat,"/","viewEvent.praat", sep = ""))
  cmd <- paste0("cd ", pathSendPraat, " && sendpraat praat \"runScript: \\\"",pathPraat,"/","viewEvent.praat\\\"\""  ) %>% stringr::str_replace_all("/","\\\\\\")
  shell(cmd)
  unlink(paste(pathPraat, "/viewEvent.praat", sep = ""))
}
