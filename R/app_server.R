#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 


# SERVER LOGIC -----------------------------------------------------------

app_server <- function( input, output, session ) {


  # dataOd <- "'2020-11-18'"
  # dataDo <- "'2020-11-18'"
  # 
  # dataSQL <- pool %>%
  #   DBI::dbGetQuery(paste0("EXEC [dbo].[eDriverTEST] ", dataOd, ",", dataDo))

  github_csv <- read.csv("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")

  df_dateFormat <- github_csv %>%
    dplyr::mutate(github_csv, statusDateConverted = as.Date(statusDate, format = "%d.%m.%Y")) %>% 
    transform(orderNr = as.character(orderNr)) #tu robimy as character bo bez tego zle wyswietla dane w boxTable (jako interger sa z automatu przyklejone do prawej strony, jakw  excelu)

  dateFrom <- anytime::anydate("2020-11-18")
  dateTo <- anytime::anydate("2020-11-18")

  func_FilterDate <- function(df, from, to){
    df[df$statusDateConverted >= from & df$statusDateConverted <= to,]
  }

  data_app <- func_FilterDate(df_dateFormat, dateFrom, dateTo)


  colnames(data_app)[1] <- "transitNr"
#sapply(data_app, typeof)

  mod_header_server("header_ui")
  sidebarMod <- mod_sidebar_server("sidebar_ui")
  mod_body_server("body_ui", data_app, btnAnalysis = sidebarMod$btnAnalysis, sidebarDashboard = sidebarMod$sidebarDashboard)
  #mod_body_server("body_ui", dataSQL, btnAnalysis = sidebarMod$btnAnalysis, sidebarDashboard = sidebarMod$sidebarDashboard)

  
}
