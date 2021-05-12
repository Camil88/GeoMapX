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
                         #bs4Dash::menuItem("Refresh map", tabName = "refresh", icon = icon("sync-alt")),  #'circle'
                         bs4Dash::menuItem("Transits & Orders", tabName = "transitsOrders", icon = icon("shipping-fast")),
                         bs4Dash::menuItem("Drivers", tabName = "drivers", icon = icon("user-friends")),                         
                         bs4Dash::menuItem("GPS areas", tabName = "areasGPS", icon = icon("object-ungroup"), selected = TRUE),
                         #bs4Dash::menuItem("Points on the map", tabName = "points", icon = icon("map-marker-alt")),
                         bs4Dash::menuItem("Reports", tabName = "reports", icon = icon("paper-plane"),
                                           bs4Dash::menuSubItem(
                                             text = "Excel",
                                             tabName = "excelRaport",
                                             icon = icon("file-excel")
                                           )),
                         bs4Dash::menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
                         bs4Dash::menuItem("Calendar", tabName = "calendar", icon = icon("calendar-alt"),
                                           shinyWidgets::airDatepickerInput(
                                             inputId = "datepicker",
                                             range = TRUE,
                                             #label = 'wybierz zakres dat od-do:',
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
    
    #shinyWidgets::actionBttn(inputId = ns("btrrr"), label = NULL, style = "material-circle", size = "sm", icon = icon("vector-square")) 
    
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

      # observeEvent(input$sidebar, {
      #   bs4Dash::updateBox("panelLeft", action = "toggle")
      # })

     
      # ponizej metoda na zwracanie klikniecia do modulu. To: reactive({input$btnAnaliza}) też dziala tylko ze przypisujac to do zmiennej i w module odwolujac sie do tej zmiennej wywala blad 'closure'. Trzeba zwroci wynik w liscie jak nizej.
      return(
        list(
          btnAnalysis = reactive({input$btnAnalysis}),
          sidebarDashboard = reactive({input$sidebar})
      
          # observe({      # tutaj w return mozna dodac observe jezeli chcemy zwrocic wartosc dla klikniecia w menuItem
          #   print(input$sidebar)
          # })  
        )
      )
      

    })
}

