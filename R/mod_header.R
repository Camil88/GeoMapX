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
        headerText = tags$span(style="color:#007bff", "Entering Customer GPS area by driver"),
        type = "notifications",
        badgeStatus = "danger",
        icon = icon("bell"),
        bs4Dash::notificationItem(icon = icon("draw-polygon"),
                                  status = "info",
                                  "Julius Craig entered Customer_234"
        ),
        
        bs4Dash::notificationItem(icon = icon("draw-polygon"),
                                   status = "info",
                                  "Adrian Baker entered Customer_22"
        ),
        
        bs4Dash::notificationItem(icon = icon("draw-polygon"),
                                  status = "info",
                                  "John Cain entered Customer_119"
        ),
        
        bs4Dash::notificationItem(icon = icon("draw-polygon"),
                                  status = "info",
                                  "Nick Carr entered Customer_38"
        )),         

        bs4Dash::dropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          icon = icon("comments"),
          bs4Dash::messageItem("Message from driver Adrian Baker",
                               "Customer closed",
                                time = "5 mins ago"),
          bs4Dash::messageItem("Message from driver Louis Plitz",
                               "Huge traffic, I'll be late",
                                time = "8 mins ago"),
          bs4Dash::messageItem("Message from driver George Hill",
                               "Lack of 4 pallets..",
                               time = "15 mins ago"),
          bs4Dash::messageItem("Message from driver Tony Parks",
                               "Need tel. no. to John!",
                               time = "45 mins ago"),
          bs4Dash::messageItem("Message from driver Adam Jones",
                               "Which gate to drive through?",
                               time = "1 hr 22 mins ago"),
          bs4Dash::messageItem("Message from driver Allen Barton",
                               "Customer closed, not responding",
                               time = "1 hr 38 mins ago")
      
          ),

        bs4Dash::userOutput(ns("user"))
    
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
      
      output$user <- bs4Dash::renderUser({
        bs4Dash::dashboardUser(
          image = "www/capitals.png",
          title = "Camill Head",
          subtitle = "welcome to the app!"
        )
      })

    })
}


