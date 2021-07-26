#' play_event
#'
#' @param exb exb object as created by exb_read_file or exb_read_dir; can also be a subset
#' @param rowEvent row of the event you want to play
#' @param pathFileDir path of the audio file dir (, to provide dir is useful if there are several audio files in one corpus;has to be .wav at the moment)
#' @param pathPraat path of Praat.exe; if not provided it will be assumend praat is in the working directory
#'
#' @return NULL
#' @export
#'
#' @examples
play_event <- function(exb, rowEvent= 1, pathFileDir= character(0),pathPraat= getwd()){
  if(identical(pathFile, character(0))){
    pathFile <- paste(getwd(),"/",stringr::str_trim(exb$File[rowEvent]), ".wav", sep= "" )
  }else{
    pathFile <- paste(pathFileDir,"/",stringr::str_trim(exb$File[rowEvent]), ".wav", sep= "" )
  }
  line1 <- paste0("Open long sound file: ","\"",pathFile,"\"")
  if(as.numeric(exb$Start_time[rowEvent]) >1){
    exb$Start_time[rowEvent] <- as.numeric(exb$Start_time[rowEvent])-1
  }
  exb$End_time[rowEvent] <- as.numeric(exb$End_time[rowEvent])+1
  line2 <- paste0("Play part: ",exb$Start_time[rowEvent], ", ",exb$End_time[rowEvent], sep="")
  praat <- paste(line1,line2, sep="\n")
  write(praat, file= paste(pathPraat,"/","playEvent.praat", sep = ""))
  cmd <- paste0(pathPraat,"/Praat.exe"," --run ", pathPraat, "/playEvent.praat") %>% stringr::str_replace_all("/","\\\\\\")
  system(cmd)
  unlink(paste(pathPraat, "/playEvent.praat", sep = ""))
}
