
# CREATE POINTS FOR DATA SUBSETS

coordCustomers <- function(data, type) {

    df <- data %>%
      dplyr::filter(transitType == type) %>% 
      sf::st_as_sf(coords = c("lonCust","latCust"), crs = 4326)

  return(df)
    
}



coordApplication <- function(data, type) {
  
  df <- data %>%
    dplyr::filter(transitType %in% type) %>% 
    sf::st_as_sf(coords = c("lonApp","latApp"), crs = 4326)
  
  return(df)
  
}


# CREATE LINESTRINGS

createLinestring <- function(lonCust,latCust,lonApp,latApp) {
  sf::st_linestring(matrix(c(lonCust,lonApp,latCust,latApp),2,2))
}

createLines <- function(data, type) {
  
  df <- data.frame(transitType = data$transitType[data$transitType == type])
  
  geomLines <- data %>%
    dplyr::filter(transitType == type) %>% 
    dplyr::select(lonCust,latCust,lonApp,latApp) %>%
    purrr::pmap(createLinestring) %>%
    sf::st_as_sfc(crs = 4326)

  sf = sf::st_sf(df,geometry = geomLines)
  
  return(sf)
  
}



 
