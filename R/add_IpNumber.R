#' add_IpNumber()
#'
#' @param events_sorted Requieres data.frame of sorted events as returned by sort_events()
#'
#' @return Returns a data.frame with sorted events with a conitnuus IP-Number
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE)
#' file <- xml2::read_xml(path, encoding="UTF-8")
#' add_IpNumber(events_sorted = sort_events(events = read_events(file, path), timeline= read_timeline(file)))
#'
add_IpNumber <- function(events_sorted){
  IpNumber <- integer(0)
  IpNumber[1] <- 1
  k <- 2
  for (k in 2:nrow(events_sorted)) {
    if(stringr::str_ends(events_sorted$Text[k-1], "=")==FALSE & stringr::str_starts(events_sorted$Text[k], "=")){
      IpNumber[k] <- IpNumber[k-1]
      k <- k+1
    }else if((stringr::str_ends(events_sorted$Text[k-1], "[.;?,-]\\s?")|
        stringr::str_ends(events_sorted$Text[k-1], "\\Q|\\E\\s?")|
        stringr::str_ends(events_sorted$Text[k-1], "\\u01C0\\s?") |
        stringr::str_ends(events_sorted$Text[k-1], "\\Q/\\E\\s?")|
        stringr::str_ends(events_sorted$Text[k-1], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=>)")|
        stringr::str_ends(events_sorted$Text[k-1], "\\d\\)\\s?")|
        stringr::str_ends(events_sorted$Text[k-1], "\\)\\)\\s?")|
        # stringr::str_ends(events_sorted$Text[k-1], "\\)")|
        stringr::str_ends(events_sorted$Text[k-1], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=\\])")|
        #stringr::str_ends(events_sorted$Text[k-1], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)(?==)")
        stringr::str_ends(events_sorted$Text[k-1],"="))==TRUE){
      IpNumber[k] <- IpNumber[k-1] +1
      k <- k+1
    } else if(events_sorted$Speaker[k-1] != events_sorted$Speaker[k]){
      IpNumber[k] <- IpNumber[k-1] +1
      IpNumber[k+1] <- IpNumber[k-2]
      k <- k+2
    } else {
      IpNumber[k] <- IpNumber[k-1]
      k <- k+1
    }
  }
  if(length(IpNumber)!=nrow(events_sorted)){ # adjust length if there is a dummy event at the end of splitted transcription
    IpNumber <- IpNumber[1:nrow(events_sorted)]
  }
  IpNumber <- formatC(IpNumber, width = 5, format = "d", flag = "0")
  events_sorted <-  cbind(IpNumber, events_sorted)
  return(events_sorted)
}
