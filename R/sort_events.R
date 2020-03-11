#' sort_events()
#'
#' @param events List of events as returned by read_events()
#' @param timeline List of timestamps returned by read_timeline()
#'
#' @return Returns list of sorted events
#' @export
#'
#' @examples
#' sort_events(events, timeline)
sort_events <- function(events, timeline){
  events_sorted <- left_join(data.frame(start=timeline),events, by="start")
  for (u in 2:nrow(events_sorted)) {
    if (stringr::str_detect(events_sorted$Text[u-1], "[.;?,-]")==FALSE & events_sorted$Speaker[u-1] != events_sorted$Speaker[u]){
      events_sorted <- events_sorted[c(1:u-1, u+1, u, u+2:nrow(events_sorted)),]
    }
  }
  events_sorted <- stats::na.omit(events_sorted)
  EventNumber <- 1:nrow(events_sorted)
  events_sorted <- cbind(EventNumber,events_sorted)
  return(events_sorted)
}


