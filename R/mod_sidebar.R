#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)

  sidebar = bs4Dash::dashboardSidebar(
    
    fixed = TRUE,
    collapsed = TRUE,
    expandOnHover = FALSE,
    
    shinyWidgets::actionBttn(inputId = ns("refresh"), label = NULL, style = "material-circle", size = "md", icon = icon("sync-alt")), 

    bs4Dash::sidebarMenu(id = ns("sidebar"),
                         bs4Dash::menuItem("Transits & Orders", tabName = "transitsOrders", icon = icon("shipping-fast"), selected = TRUE),
                         bs4Dash::menuItem("Drivers", tabName = "drivers", icon = icon("user-friends")),                         
                         bs4Dash::menuItem("Reports", tabName = "reports", icon = icon("paper-plane")),
                         bs4Dash::menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
                         bs4Dash::menuItem("Calendar", tabName = "calendar", icon = icon("calendar-alt"),
                                           shinyWidgets::airDatepickerInput(
                                             inputId = "datepicker",
                                             range = TRUE,
                                             label = 'choose date range:',
                                             separator = "  |  ",
                                             value = c(Sys.Date(), Sys.Date()),
                                             position = "bottom left",
                                             language = "pl",
                                             autoClose = TRUE,
                                             dateFormat = 'yyyy-mm-dd',
                                             startView = Sys.Date(),
                                             addon = "right"
                                           ))),
    
    customArea = fluidRow(
      bs4Dash::actionButton(
        inputId = ns("btnAnalysis"),
        label = NULL,
        icon = icon("codepen"),
        width = NULL,
        status = "primary",
        style = "margin: auto",
      )
    )
  )
}
    

#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      observeEvent(input$refresh, {
        showNotification(
          ui = "Data has been refreshed!",
          type = "message",
          id = "toast",
          duration = 3,
          closeButton = FALSE
        )
      })   
      
      return(
        list(
          btnAnalysis = reactive({input$btnAnalysis}),
          sidebarDashboard = reactive({input$sidebar})
        )
      )
    })
}

