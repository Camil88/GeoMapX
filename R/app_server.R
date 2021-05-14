#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 


app_server <- function( input, output, session ) {

  
  # download and transform data
  github_csv <- read.table("https://raw.githubusercontent.com/Camil88/geoMapX/main/data/dataApp.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")

  df_dateFormat <- github_csv %>%
    dplyr::mutate(statusDateConverted = as.Date(statusDate, format = "%d.%m.%Y")) %>% 
    transform(orderNr = as.character(orderNr),
              distanceKm = stringr::str_replace(df_dateFormat$distanceKm, ",", "."))

  df_dateFormat$distanceKm = as.numeric(df_dateFormat$distanceKm)
  
  dateFrom <- anytime::anydate("2020-11-18")
  dateTo <- anytime::anydate("2020-11-18")

  func_FilterDate <- function(df, from, to){
    df[df$statusDateConverted >= from & df$statusDateConverted <= to,]
  }

  data_app <- func_FilterDate(df_dateFormat, dateFrom, dateTo)
  colnames(data_app)[1] <- "transitNr"
  


  mod_header_server("header_ui")
  sidebarMod <- mod_sidebar_server("sidebar_ui")
  mod_body_server("body_ui", data_app, btnAnalysis = sidebarMod$btnAnalysis, sidebarDashboard = sidebarMod$sidebarDashboard)


  
}
