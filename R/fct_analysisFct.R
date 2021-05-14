
# FUNCTIONS FOR USE IN DASHBOARD/CONTROLBAR ANALYSES


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



# stats used in dashboard mod
chartStatsBestDriver <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::summarise(sumAllOrders = sum(countAllOrders),
                     Orders = sum(count)) %>% 
    dplyr::mutate(ratioPure = Orders/sumAllOrders,
                  ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::filter(ratioPure <= 1) %>% 
    dplyr::arrange(desc(ratioPure)) %>% 
    dplyr::slice(1)
  
  return(result)
}


# stats used in dashboard mod
chartStatsWorstDriver <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::summarise(sumAllOrders = sum(countAllOrders),
                     Orders = sum(count)) %>% 
    dplyr::mutate(ratioPure = Orders/sumAllOrders,
                  ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::filter(ratioPure <= 1) %>% 
    dplyr::arrange(ratioPure) %>% 
    dplyr::slice(1)

  return(result)
}


# stats used in dashboard mod
chartStatsBestDivision <- function(data, colName1, colName2, colName3) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]], .data[[colName3]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::summarise(sumAllOrders0 = sum(countAllOrders),
                     Orders0 = sum(count)) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::summarise(sumAllOrders = sum(sumAllOrders0),
                     Orders = sum(Orders0)) %>% 
    dplyr::mutate(ratioPure = Orders/sumAllOrders,
                  ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::filter(ratioPure <= 1) %>% 
    dplyr::arrange(desc(ratioPure)) %>% 
    dplyr::slice(1)
  
  return(result)
}


# stats used in dashboard mod
chartStatsWorstDivision <- function(data, colName1, colName2, colName3) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data[[colName1]], .data[[colName2]], .data[[colName3]]) %>%
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::summarise(sumAllOrders0 = sum(countAllOrders),
                     Orders0 = sum(count)) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::summarise(sumAllOrders = sum(sumAllOrders0),
                     Orders = sum(Orders0)) %>% 
    dplyr::mutate(ratioPure = Orders/sumAllOrders,
                  ratio = scales::percent(Orders/sumAllOrders, accuracy = 0.1)) %>% 
    dplyr::filter(ratioPure <= 1) %>% 
    dplyr::arrange(ratioPure) %>% 
    dplyr::slice(1)
  
  return(result)
}


# stats used in dashboard mod
statsKmBest <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data[[colName1]], .data[[colName2]]) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::slice_max(order_by = .data[[colName2]], n = 1) %>% 
    unique() %>% 
    dplyr::arrange(.data[[colName2]]) %>% 
    head(1)
  
  return(result)
}


# stats used in dashboard mod
statsKmWorst <- function(data, colName1, colName2) {
  
  result <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data[[colName1]], .data[[colName2]]) %>% 
    dplyr::group_by(.data[[colName1]]) %>% 
    dplyr::slice_max(order_by = .data[[colName2]], n = 1) %>% 
    unique() %>% 
    dplyr::arrange(desc(.data[[colName2]])) %>% 
    head(1)
  
  return(result)
}






