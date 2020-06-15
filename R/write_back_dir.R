#' write_back_dir
#' Writes back edited annotations to original files
#' needs csv file form data.frame created by red_exb_dir()
#'
#' @param exb  data.frame created by red_exb_dir() or csv file created by such an object
#' @param sep Seperator in the csv file, if object can be left out
#' @param PathExbDir Path of the original files. Attention: the files must have the same names as in the "File" collumn
#' @param PathNewFiles Default is the original Directory, can be changend
#' @param suffix suffix to be added to the new files, default is "_new"
#'
#' @return
#' @export
#'
write_back_dir <- function(exb,sep=",", PathExbDir, PathNewFiles = PathExbDir, suffix="_new"){

# Check if exb is object or csv file, save in annotations -----------------
  if(is.data.frame(exb)){
    annotations <- exb
  }else{
    annotations <- read.delim(exb, header = TRUE,sep=sep, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }
  files <- unique(annotations$File)
  for (k in 1:length(files)) {
    ann <- dplyr::filter(annotations, File==files[k])
    PathFile <- stringr::str_glue(PathExbDir,"\\",stringr::str_trim(files[k]),".exb")
    PathNewFile <- PathNewFiles
    write_back_to_exb(ann, PathExb = PathFile,PathNewFile = PathNewFile )
  }
}
