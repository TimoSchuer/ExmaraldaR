#' add_IpNumber()
#'
#' @param events_sorted Requieres data.frame of sorted events as returned by sort_events()
#'
#' @return Returns a data.frame with sorted events with a conitnuus IP-Number
#' @export
#'
#' @examples
#' add_IpNumber(events_sorted)
#'
add_IpNumber <- function(events_sorted){
  IpNumber <- integer(0)
  IpNumber[1] <- 1
  k <- 2
  for (k in 2:nrow(events_sorted)) {
    if((stringr::str_ends(events_sorted$Text[k-1], "[.;?,-]")|stringr::str_ends(events_sorted$Text[k-1], "|") |stringr::str_ends(events_sorted$Text[k-1], "[.;?,-]{1}(?=>)")|stringr::str_ends(events_sorted$Text[k-1], "\\d\\)")| stringr::str_ends(events_sorted$Text[k-1], "\\)\\)")|stringr::str_ends(events_sorted$Text[k-1], "[.;?,-]{1}(?=\\])")) & stringr::str_starts(events_sorted$Text[k], "=") == FALSE){
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
  events_sorted <-  cbind(IpNumber, events_sorted)
  return(events_sorted)
}
