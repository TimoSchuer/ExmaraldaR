#' play_event
#'
#' @param exb exb object as created by exb_read_file or exb_read_dir; can also be a subset
#' @param rowEvent row of the event you want to play
#' @param pathFile path of the audio file (has to be .wav at the moment)
#' @param pathPraat path of Praat.exe; if not provided it will be assumend praat is in the working directory
#'
#' @return NULL
#' @export
#'
#' @examples
play_event <- function(exb, rowEvent= 1, pathFile= character(0),pathPraat= paste(getwd(),"/Praat.exe", sep= "")){
  exb <- ContiCorp[rowEvent,]
  if(identical(pathFile, character(0))){
    pathFile <- paste(getwd(),"/",stringr::str_trim(exb$File), ".wav", sep= "" )
  }
  line1 <- paste0("Open long sound file: ","\"",pathFile,"\"")
  if(exb$Start>1){
    exb$Start_time <- as.numeric(exb$Start_time)-1
  }
  exb$End_time <- as.numeric(exb$End_time)+1
  line2 <- paste0("Play part: ",exb$Start_time, ", ",exb$End_time, sep="")
  praat <- paste(line1,line2, sep="\n")
  write(praat, file= paste(pathPraat,"/","playEvent.praat", sep = ""))
  cmd <- paste0(pathPraat,"/Praat.exe"," --run ", pathPraat, "/playEvent.praat") %>% stringr::str_replace_all("/","\\\\\\")
  system(cmd)
  unlink(paste(pathPraat, "/playEvent.praat", sep = ""))
}
