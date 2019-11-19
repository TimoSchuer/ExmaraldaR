print_error <- function(exb, k){
  mes <- "The annotation is not correctly formated. Please correct the following line"
  cat(mes,"\n")
  print.data.frame(exb[k,])
}
