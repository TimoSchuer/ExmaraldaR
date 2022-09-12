sort_events <- function(events, IPEndSign= c("|",".",";",",",",","?","=","-"),addIPNumber=TRUE){
  if(addIPNumber==FALSE){
    events <- events %>% arrange(Start_time) %>% mutate(EventID= paste(File,seq(1:nrow(.))))
    return(events)
  }
  #build regex
  sort <- events %>%  group_by(TierID) %>% arrange(Start_time)
  ends <- str_which(sort$Text, paste0("[",paste(IPEndSign, collapse = ""),"]\\s*\\Z", collapse = ""), negate = FALSE) %>% sapply(function(x) x+1)
  ends <- ends[-which(ends>nrow(events))]
  sort[ends,"IPStart"] <- TRUE
  sort[is.na(sort$IPStart),"IPStart"] <- FALSE
  events <- sort %>% mutate(TierIPNo = 1 + cumsum(IPStart==TRUE)) %>% ungroup() %>% mutate(TierIPNo= paste(TierID,TierIPNo, sep="_"))
  IPRank <- events %>% ungroup() %>% group_by(TierIPNo) %>% summarise(min=min(Start_time)) %>% ungroup %>% arrange(min) %>% pull(TierIPNo)
  events <- events %>% ungroup() %>%
    arrange(factor(TierIPNo, levels = IPRank)) %>%
    mutate(IPNumber = rle(TierIPNo)$lengths %>% {rep(seq(length(.)), .)} %>% formatC(width = 5, format = "d", flag = "0")) %>%
    mutate(EventID= paste(File,seq(1:nrow(.)))) %>%
    select(IPNumber,EventID, File, Speaker, TierID, Start,End, Start_time,End_time,Name, Text)
  return(events)

}
