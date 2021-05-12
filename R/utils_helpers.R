
# labels for points on a map

createLabel <- function(data) {
  
  labelText <- paste0("<b style='color:#00a2ff'>Driver: </b>",data$driverName,"<br/>",
                      "<b style='color:#00a2ff'>Client: </b>",data$customerName,"<br/>",
                      "<b style='color:#00a2ff'>Transit: </b>",data$transitNr,"<br/>",
                      "<b style='color:#00a2ff'>Order: </b>",data$orderNr,"<br/>",
                      "<b style='color:#00a2ff'>Status time: </b>",data$statusDateTime,"<br/>")
  
}




