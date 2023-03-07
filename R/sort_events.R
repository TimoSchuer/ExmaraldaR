sort_events <- function(events, IPEndSign= c("|",".",";",",",",","?","=","-"),addIPNumber=TRUE){
  if(addIPNumber==FALSE){
    events <- events %>% dplyr::arrange(Start_time) %>% dplyr::mutate(EventID= paste(File,seq(1:nrow(.))))
    return(events)
  }
  #build regex

  sort <- events %>% dplyr::group_by(TierID) %>% dplyr::arrange(Start_time, .by_group=TRUE)

  ends <-stringr::str_which(sort$Text, paste0("[",paste(IPEndSign, collapse = ""),"]\\s*\\Z", collapse = ""), negate = FALSE) %>% sapply(function(x) x+1)
  if(length(which(ends>nrow(events)))!=0){
    ends <- ends[-which(ends>nrow(events))]
  }
  sort[ends,"IPStart"] <- TRUE
  sort[is.na(sort$IPStart),"IPStart"] <- FALSE
  events <- sort %>% dplyr::mutate(TierIPNo = 1 + cumsum(IPStart==TRUE)) %>% dplyr::ungroup() %>% dplyr::mutate(TierIPNo= paste(TierID,TierIPNo, sep="_"))
  IPRank <- events %>% dplyr::ungroup() %>% dplyr::group_by(TierIPNo) %>% dplyr::summarise(min=min(Start_time)) %>% dplyr::ungroup() %>% dplyr::arrange(min) %>% dplyr::pull(TierIPNo)
  events <- events %>% dplyr::ungroup() %>%
    dplyr::arrange(factor(TierIPNo, levels = IPRank)) %>%
    dplyr::mutate(IPNumber = rle(TierIPNo)$lengths %>% {rep(seq(length(.)), .)} %>% formatC(width = 5, format = "d", flag = "0")) %>%
    dplyr::mutate(EventID= paste(File,seq(1:nrow(.)))) %>%
    dplyr::select(IPNumber,EventID, File, Speaker, TierID, Start,End, Start_time,End_time,Name, Text)
  return(events)

}
