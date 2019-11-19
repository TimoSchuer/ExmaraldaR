#' Check Filetype
#'
#' @param path Needs the path of the file you want to read
#'
#' @return Returns logical value
#'
#' @export
#'
#' @examples
#' check_exb(path)
check_exb <- function(path) {
  if(stringr::str_ends(path, "\\.exb")== TRUE){
    return(TRUE)
  }  else{
    return(FALSE)
  }
}
