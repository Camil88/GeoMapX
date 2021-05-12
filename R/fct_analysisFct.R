
# FUNCTIONS FOR SPATIAL/GENERAL ANALYSES

# Count Orders
# countOrders <- function(data, colName) {
#   
#   result <- data %>%
#     dplyr::select(colName) %>% 
#     dplyr::summarise(count = dplyr::n()) %>% 
#     sf::st_drop_geometry()
#   
#   return(result)
# }


# stats used in a table (controlbar)
tableStats <- function(data, colName1, colName2, colName3, colName4, colName5) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]]) %>%
    dplyr::summarise(countOrders = dplyr::n(),
                     "SetVSAll" = paste(countOrders, .data[[colName2]], sep = "/"),
                     "Greater30min" = sum(.data[[colName3]]),
                     "SerialSets" = sum(.data[[colName4]]),
                     "Distance1km" = sum(.data[[colName5]])) %>% 
    unique()
  
  return(result)
}




# stats used in charts (controlbar)
chartStats <- function(data, colName1, colName2, colName3) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]], .data[[colName3]]) %>%
    dplyr::summarise(countIntervals = dplyr::n()) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::summarise("Orders" = sum(countIntervals),
                     "sumAllOrders" = sum(.data[[colName3]]))

  return(result)
}

# stats used in charts (dashboard)


# stats used in charts (controlbar)
chartStatsBest <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::group_by(.data[[colName1]]) %>%
    dplyr::summarise("Orders" = sum(count),
                     "sumAllOrders" = sum(.data[[colName2]])) %>%
    dplyr::mutate(ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::arrange(desc(sumAllOrders)) %>% 
    dplyr::slice(1)
  
  return(result)
}

# chartStatsBest(data_app, "driverName", "statusGreater1km" )
# sapply(data_app, typeof)

# stats used in charts (controlbar)
chartStatsWorst <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::summarise("Orders" = sum(count),
                     "sumAllOrders" = sum(.data[[colName2]])) %>% 
    dplyr::mutate(ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::arrange(ratio) %>% 
    dplyr::slice(1)

  return(result)
}

#chartStatsBest(dataSQL,"driverName", "countAllOrders"  )

# stats used in charts (controlbar)
statsKmWorst <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data[[colName1]], .data[[colName2]]) %>% 
    dplyr::arrange(desc(.data[[colName2]])) %>% 
    dplyr::slice(1)
  
  return(result)
}

#statsKmWorst(dataSQL,"DivisionCity", "distanceKm")

# stats used in charts (controlbar)
statsKmBest <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data[[colName1]], .data[[colName2]]) %>% 
    dplyr::arrange(.data[[colName2]]) %>% 
    dplyr::slice(1)
  
  return(result)
}

#statsKmBest(dataSQL,"DivisionCity", "distanceKm")
#chartStatsWorst(dataSQL, "DivisionCity", "countAllOrders" )
#tableStats(dataSQL, "DivisionCity","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km")

