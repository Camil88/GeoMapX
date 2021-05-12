#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_header_ui <- function(id){
  ns <- NS(id)
  
  
  header = bs4Dash::dashboardHeader(
    
    title = actionButton(ns("btnBurger"), NULL, icon = icon("bars")),
    left_menu = tagList(tags$img(src = "www/logo.png", id = ("logo"))),
    
    rightUi =  tagList(
      bs4Dash::dropdownMenu(
        type = "notifications",
        badgeStatus = "danger",
        icon = icon("bell"),
        bs4Dash::notificationItem(icon = icon("bell"),
                                  status = "info",
                                  "Dojechaem na miejsce, nie ma nikogo"
        ),
        
        bs4Dash::notificationItem(icon = icon("warning"), 
                                  status = "danger",
                                  "Korek, stoje")
        
        ),
      
        bs4Dash::dropdownMenu(
          type = "messages",
          badgeStatus = "warning",
          icon = icon("envelope"),
          bs4Dash::messageItem("Legal",
                               "Mail do dzialu prawnego",
                               time = "15 mins"),
          bs4Dash::messageItem("Drobnica",
                               "Wyniki w ekierowcy",
                               time = "5 mins")
          
          ),
        
        bs4Dash::dropdownMenu(
          type = "tasks",
          badgeStatus = "success",
          
          bs4Dash::taskItem(value = 20, color = "navy",
                            "Refactor code"),
          
          bs4Dash::taskItem(value = 60, color = "fuchsia",
                            "Create documentation")
          
          )
      )    
    )


}
    
#' header Server Function
#'
#' @noRd 
mod_header_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$btnBurger,{
        shinyjs::toggleClass(class = "sidebar-collapse", selector = "body")
      })
      
    })
}

    
## To be copied in the UI
# mod_header_ui("header_ui_1")
    
## To be copied in the server
# callModule(mod_header_server, "header_ui_1")
 
