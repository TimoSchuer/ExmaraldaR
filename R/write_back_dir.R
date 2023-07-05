#' write_back_dir
#' Writes back edited annotations to original files
#' needs csv file form data.frame created by red_exb_dir()
#'
#' @param exb  data.frame created by read_exb_dir() or csv file created by such an object
#' @param sep Seperator in the csv file, if object can be left out
#' @param PathExbDir Path of the original files. Attention: the files must have the same names as in the "File" collumn
#' @param PathNewFiles Default is the original Directory, can be changend
#' @param suffix suffix to be added to the new files, default is "_new"
#'
#' @return NULL
#' @export
#'
write_back_dir <- function(exb,sep=",", PathExbDir, PathNewFiles = PathExbDir, suffix="_new", verbose=TRUE, transcription_text="Text"){

# Check if exb is object or csv file, save in annotations -----------------
  if(is.data.frame(exb)){
    annotations <- exb
  }else{
    annotations <- utills::read.delim(exb, header = TRUE,sep=sep, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
  }
  files <- annotations %>% pull(File) %>% unique() %>% as.character()
  perc <- 0
  for (k in 1:length(files)) {
    ann <- dplyr::filter(annotations, File==files[k])
    PathFile <- stringr::str_glue(PathExbDir,"\\",stringr::str_trim(files[k]),".exb") # nolint: object_name_linter, line_length_linter.
    PathNewFile <- PathNewFiles
    write_back_to_exb(ann, PathExb = PathFile,PathNewFile = PathNewFile, suffix = suffix, annotation_colums = annotation_colums, transcription_text = transcription_text )
    if(verbose==TRUE){
      perc <- perc + round(nrow(ann)/nrow(annotations)*100,2)
      print(paste0(Sys.time()," ", k,"/", length(files)," ", files[k],"...done...",perc,"%", sep=""))
    }
  }
}
