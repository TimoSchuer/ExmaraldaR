#' sort_by_unit
#'
#' @param exb needs a dataframe as given by read_exb_file or read_exb_dir
#' @param Unit_column column where the unit is defined. Needs to be consisted. Default is 1 as the output of read_exb_file and read_exb_dir saves the IP number there
#' @param drop logical value, wheter rows without an obeservation should be dropped or not
#' @param percentage logical valuem wether values per Variable per Unite should be relative or absolute
#'
#' @return returns data.frame
#' @export
#'
#' @examples sort_by_unit(exb, Unit_column=1, drop= TRUE)
#'
sort_by_unit <- function(exb, Unit_column= 1,noObservationAsNA=TRUE, drop= FALSE, percentage= TRUE){
  dummies <- as.data.frame(dummies::dummy("Variable",exb, sep=":")) #create binary values of variable
  dummies <- dplyr::select(dummies, -(dplyr::ends_with("NA")))# delete NA Variable
  ColNoVar <- which(colnames(exb)=="Variable")#get number of Variable column
  exb <- dplyr::select(exb,1:ColNoVar) # select everything besides the variables
  exb <- dplyr::select(exb,-(Start),-(End),-(Variable))

# Loop merges Ips together when there were mutiple rows -------------------
  for (i in nrow(exb):2) {
    if(exb[i,Unit_column]==exb[i-1,Unit_column] & exb[i,"EventNumber"]!= exb[i-1,"EventNumber"]){
      exb[i-1,"Text"] <- stringr::str_c(exb[i-1,"Text"],exb[i,"Text"], sep= " ") # merge text
      exb <- exb[-i,]
      dummies[i-1,] <- dummies[i-1,]+dummies[i,] # add up variable values
      dummies <- dummies[-i, ]
    }else if(exb[i,Unit_column]==exb[i-1,Unit_column] & exb[i,"EventNumber"]==exb[i-1,"EventNumber"]){
      exb <- exb[-i,]
      dummies[i-1,] <- dummies[i-1,]+dummies[i,] # add up variable values
      dummies <- dummies[-i, ]
    }
  }
  if(percentage==TRUE){
# get percentage if theres more than  one observation per variable --------
    colnames <- colnames(dummies)
    colnames <- stringr::str_remove(colnames, "Variable:")
    colnames <- stringr::str_remove(colnames,":.+")
    colnames <- unique(colnames)
    dummies2 <- data.frame(0) #empty data.frame
    if(noObservationAsNA == TRUE){
      for (k in 1:length(colnames)) {
        perVar <- dplyr::select(dummies, dplyr::contains(stringr::str_glue(colnames[k],":"))) # get all dummy Variables that belong to one Variable
        Sums <- unname(rowSums(perVar)) # get RowSums
        for (n in 1:nrow(perVar)){
          if(Sums[n] ==0){
            for (p in 1:ncol(perVar)) {
              perVar[n,p] <- NA
            }
          }else{
            for (l in 1:ncol(perVar)){
              perVar[n,l] <- as.numeric(perVar[n,l])
              perVar[n,l] <- perVar[n,l]/Sums[n] # get percentage
            }
          }
        }
      dummies2 <- cbind(dummies2,perVar) #save
      }
    }else{
      for (k in 1:length(colnames)) {
        perVar <- dplyr::select(dummies, dplyr::contains(stringr::str_glue(colnames[k],":"))) # get all dummy Variables that belong to one Variable
        Sums <- unname(rowSums(perVar)) # get RowSums
        for (n in 1:nrow(perVar)){
          if(Sums[n] !=0){
            for (l in 1:ncol(perVar)){
              perVar[n,l] <- as.numeric(perVar[n,l])
              perVar[n,l] <- perVar[n,l]/Sums[n] # get percentage
            }
          }
        }
      dummies2 <- cbind(dummies2,perVar) #save
      }
    }
  dummies <- dplyr::select(dummies2,-(1)) # overwrite absolute values, leave out empty column created by data.frame(0)
  }else{
    colnames <- colnames(dummies)
    colnames <- stringr::str_remove(colnames, "Variable:")
    colnames <- stringr::str_remove(colnames,":.+")
    colnames <- unique(colnames)
    dummies2 <- data.frame(0) #empty data.frame
    if(noObservationAsNA == TRUE){
      for (k in 1:length(colnames)) {
        perVar <- dplyr::select(dummies, dplyr::contains(stringr::str_glue(colnames[k],":"))) # get all dummy Variables that belong to one Variable
        Sums <- unname(rowSums(perVar)) # get RowSums
        for (n in 1:nrow(perVar)){
          if(Sums[n] ==0){
            for (p in 1:ncol(perVar)) {
              perVar[n,p] <- NA
            }
          }
        }
        dummies2 <- cbind(dummies2,perVar) #save
      }
    }
    dummies <- dplyr::select(dummies2,-(1)) # overwrite absolute values, leave out empty column created by data.frame(0)
  }
  sorted <- cbind(exb, dummies) # merge transcription information and obeservations
  # drops alls rows without observations ------------------------------------
    if(drop==TRUE){
    sortedNum <- dplyr::select(sorted, dplyr::starts_with("Variable"))
    emptyRows <- which(rowSums(sortedNum, na.rm = TRUE)==0)
    sorted <- sorted[-(emptyRows),]
  }
  return(sorted)
}



