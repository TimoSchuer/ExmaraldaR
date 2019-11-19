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
  events_sorted <- data.frame(Speaker = character(0), TierId =character(0), Name= character(0), Text = character(0), Start = character(0), End= character(0),stringsAsFactors = FALSE)
  for (i in 1:length(timeline)) {
    nextLine <- dplyr::filter(events, Start == timeline[i])
    events_sorted <- rbind(events_sorted, nextLine)
    i <- i+1
  }
  for (u in 2:nrow(events_sorted)) {
    if (stringr::str_detect(events_sorted$Text[u-1], "[.;?,-]")==FALSE & events_sorted$Speaker[u-1] != events_sorted$Speaker[u]){
      events_sorted <- events_sorted[c(1:u-1, u+1, u, u+2:nrow(events_sorted)),]
    }
    u <- u+1
  }
  events_sorted <- stats::na.omit(events_sorted)
  EventNumber <- 1:nrow(events_sorted)
  events_sorted <- cbind(EventNumber,events_sorted)
  return(events_sorted)
}
