#' Check Filetype
#'
#' @param path Needs the path of the file you want to read
#'
#' @return Returns logical value
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE)
#' check_exb(path)
check_exb <- function(path) {
  if(stringr::str_ends(path, "\\.exb")== TRUE){
    return(TRUE)
  }  else{
    return(FALSE)
  }
}
