
# CREATE FUNCTIONS TO RETURN DATA FOR USE IN THE BOX TABLE

# transitsOrders <- function(data, columnNr){
#   
#   col <- colnames(data)[columnNr]
#   
#   df <- data %>% 
#     dplyr::select(col)
#   
#   return(df)
# }


colTable <- function(data, ...){

  df_sf <- data %>%
    sf::st_as_sf(coords = c("lonApp","latApp"), crs = 4326)
  
  init <- FALSE
  
  for (i in list(...)) {
    col <- unique(df_sf[i])
    # col <- unique(data[i])
    
    if (!init) {
      df <- col
      init <- TRUE
    } else {
      df <- cbind(df,col)
    }
  }
  return(df)
}







