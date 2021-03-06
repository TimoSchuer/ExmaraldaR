#' sort_events()
#'
#' @param events List of events as returned by read_events()
#' @param timeline List of timestamps returned by read_timeline()
#'
#' @return Returns list of sorted events
#' @export
#'
#' @examples
#' path <- system.file("extdata", "Example_linear.exb", package = "ExmaraldaR", mustWork = TRUE)
#'  file <- xml2::read_xml(path, encoding="UTF-8")
#' sort_events(events = read_events(file, path), timeline= read_timeline(file))
sort_events <- function(events, timeline){
# Sort events by common timeline ------------------------------------------
  vec_sort <- c()
  for (i in 1:nrow(timeline)) {
    vec_sort <- c(vec_sort, which(events$Start== timeline[i,1]))
  }
  events_sorted <- events[vec_sort,] %>% `rownames<-`(seq(1:nrow(.)))

# Sort events so that IPs are held together -------------------------------
  vecIp <- c()
  for (k in 1:nrow(events_sorted)) {
    if(k %in% vecIp){
      next()
    }else if((stringr::str_ends(events_sorted[k,'Text'], "[.;?,-]\\s?")|
              stringr::str_ends(events_sorted[k,'Text'], "\\Q|\\E\\s?")|
              stringr::str_ends(events_sorted[k,'Text'], "\\u01C0\\s?") |
              stringr::str_ends(events_sorted[k,'Text'], "\\Q/\\E\\s?")|
              stringr::str_ends(events_sorted[k,'Text'], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=>)")|
              stringr::str_ends(events_sorted[k,'Text'], "\\d\\)\\s?")|
              stringr::str_ends(events_sorted[k,'Text'], "\\)\\)\\s?")|
              # stringr::str_ends(events_sorted[k,'Text'], "\\)")|
              stringr::str_ends(events_sorted[k,'Text'], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=\\])")|
              stringr::str_ends(events_sorted[k,'Text'],"="))== TRUE){
          vecIp <- c(vecIp,k)

    }else{
      n <- k
      vecIp <- c(vecIp,n)
      tryCatch(
        while((stringr::str_ends(events_sorted[n,'Text'], "[.;?,-]\\s?")|
               stringr::str_ends(events_sorted[n,'Text'], "\\Q|\\E\\s?")|
               stringr::str_ends(events_sorted[n,'Text'], "\\u01C0\\s?") |
               stringr::str_ends(events_sorted[n,'Text'], "\\Q/\\E\\s?")|
               stringr::str_ends(events_sorted[n,'Text'], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=>)")|
               stringr::str_ends(events_sorted[n,'Text'], "\\d\\)\\s?")|
               stringr::str_ends(events_sorted[n,'Text'], "\\)\\)\\s?")|
               # stringr::str_ends(events_sorted[n,'Text'], "\\)")|
               stringr::str_ends(events_sorted[n,'Text'], "([.;?,-]{1}| \\Q|\\E|\\Q/\\E|\\u01C0)\\s?(?=\\])")|
               stringr::str_ends(events_sorted[n,'Text'],"="))== FALSE){
          subset <- dplyr::slice(events_sorted, n+1:nrow(events_sorted))
          n <- match(events_sorted[n,'Speaker'],subset$Speaker) +n
          vecIp <- c(vecIp,n)
        }, error=function(e){print(paste(events$File, ":Last event doesn't have an IP-ending sign."))})
    }
  }
  events_sorted <- events_sorted[vecIp,] %>% `row.names<-`(seq(1:nrow(.)))
  return(events_sorted)
}

#IDEE: nicht Zeilen sortieren, sondern nur Vektor mit Zeilennummern erstellen und damit dann sortieren
# sort_events <- function(events, timeline){
#   events_sorted <- data.frame(Speaker = character(0), TierId =character(0), Name= character(0), Text = character(0), Start = character(0), End= character(0),stringsAsFactors = FALSE)
#   for (i in 1:length(timeline)) {
#     nextLine <- dplyr::filter(events, Start == timeline[i])
#     events_sorted <- rbind(events_sorted, nextLine)
#   }
#   for (u in 2:nrow(events_sorted)) {
#     if (stringr::str_detect(events_sorted$Text[u-1], "[.;?,-]")==FALSE & events_sorted$Speaker[u-1] != events_sorted$Speaker[u]){
#       events_sorted <- events_sorted[c(1:u-1, u+1, u, u+2:nrow(events_sorted)),]
#     }
#   }
#   events_sorted <- stats::na.omit(events_sorted)
#   EventNumber <- 1:nrow(events_sorted)
#   events_sorted <- cbind(EventNumber,events_sorted)
#   return(events_sorted)
# }
