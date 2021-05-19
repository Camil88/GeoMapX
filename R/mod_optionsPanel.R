#' optionsPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optionsPanel_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    absolutePanel(id = ns("optionsPanel"),
                  tagList(shinyWidgets::actionBttn(inputId = ns("btnOpt1"), label = NULL, style = "material-circle", size = "sm", icon = icon("search")),
                          shinyWidgets::actionBttn(inputId = ns("btnOpt3"), label = NULL, style = "material-circle", size = "sm", icon = icon("layer-group"))))

  )
}
    

#' @noRd 
mod_optionsPanel_server <- function(id) { 
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)  

      
      shinyjs::delay(300,
                     shinyjs::runjs(
                       htmltools::HTML('$(".search-input").attr("placeholder","Search");')))


      shinyjs::delay(300,
                     shinyjs::runjs(
                       htmltools::HTML('$(".search-button").remove();')))


      shinyjs::delay(300,
                     shinyjs::runjs(
                       htmltools::HTML('$(".leaflet-control-layers-toggle").remove();')))


      shinyjs::delay(300,
                     shinyjs::runjs(
                       htmltools::HTML('$(".leaflet-control-layers-expanded").hide();')))
      
      shinyjs::delay(300,
                     shinyjs::runjs(
                       htmltools::HTML('$(".custom-switch").hide();')))

  
    }
)}


 
