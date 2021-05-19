#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_ui <- function(id){
  ns <- NS(id)

  
  body = bs4Dash::dashboardBody(
    
    shinyjs::useShinyjs(),

    mod_spatialOperations_ui(ns("proxyMap")),
    mod_dashboard_ui(ns("dashboard"))

  )
}
    
#' body Server Function
#'
#' @noRd 
mod_body_server <- function(id, data, btnAnalysis, sidebarDashboard){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
       mod_dashboard_server("dashboard", data)
       mod_spatialOperations_server("proxyMap", data)
       
       observeEvent(btnAnalysis(), {
        shinyjs::toggle(id = "proxyMap-analysis-analysisPanel",
                        anim = TRUE,
                        animType = "fade",
                        time = 0.4)
       })
      
      
      observe({        
          if(sidebarDashboard() == "dashboard") {
            shinyjs::hide(id = "proxyMap-map", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-coordPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-optionsBtns-optionsPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-analysis-analysisPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-analysis-analysisPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::runjs(
              htmltools::HTML("$('#shiny-tab-dashboard').show();
                               $('#panelTransits, #panelDrivers, #panelReports').hide();
                               $('#sidebar_ui-btnAnalysis').prop('disabled', true);"))
            
          } else {
            shinyjs::show(id = "proxyMap-map", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::show(id = "proxyMap-coordPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::show(id = "proxyMap-optionsBtns-optionsPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::runjs(
              htmltools::HTML("$('#shiny-tab-dashboard').hide();
                               $('#panelTransits, #panelDrivers, #panelReports').show();
                               $('#sidebar_ui-btnAnalysis').prop('disabled', false);")) 
          }
      })
})
}
    












