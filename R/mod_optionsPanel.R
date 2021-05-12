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
                          shinyWidgets::actionBttn(inputId = ns("btnOpt2"), label = NULL, style = "material-circle", size = "sm", icon = icon("draw-polygon")),
                          shinyWidgets::actionBttn(inputId = ns("btnOpt3"), label = NULL, style = "material-circle", size = "sm", icon = icon("layer-group")),
                          shinyWidgets::actionBttn(inputId = ns("btnOpt4"), label = NULL, style = "material-circle", size = "sm", icon = icon("tint"))))    
    
 
  )
}
    

#' @noRd 
mod_optionsPanel_server <- function(id, passedMap, passedMapInput, parentSession){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)  
  
      
      visibilityMapPolygons <- reactiveValues(showHide = FALSE)
      
      
  
      observeEvent(input$btnOpt2, {
        
        if (visibilityMapPolygons$showHide) {
          
          visibilityMapPolygons$showHide = !visibilityMapPolygons$showHide
          
          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(2); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))   
          
        } else {
          
          visibilityMapPolygons$showHide = !visibilityMapPolygons$showHide
          
          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(2); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))  
          
          
          shinyjs::runjs(
            htmltools::HTML('document.querySelector(".leaflet-draw-draw-polygon").click();'))
          
        }
      })
  
  
    }
)}


 
