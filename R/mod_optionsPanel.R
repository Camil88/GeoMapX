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
  
      
      visibilitySearch <- reactiveValues(showHide = FALSE)
      visibilityLayers <- reactiveValues(showHide = FALSE)

      
      observeEvent(input$btnOpt1, {

        shinyjs::runjs(
          htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(2); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))         
      })
      
      
      
      observeEvent(input$btnOpt3, {
        
        shinyjs::runjs(
          htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(2); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))         
      })      
      

      
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


 
