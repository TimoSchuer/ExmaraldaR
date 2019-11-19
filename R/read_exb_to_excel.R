read_exb_to_csv <- function(path,PathTagSet,PathCsv=getwd()){
  exb <- read_exb_file(path,PathTagSet)
  filename <- stringr::str_c(PathCsv,"/",stringr::str_trim(exb$File[1]),"_Ann.csv")
  utils::write.csv(exb,file = filename)
}
