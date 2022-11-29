fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}