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

    
    # bs4Dash::tabItems(
    #   # bs4Dash::tabItem(tabName = "live",
    #   #                  bs4Dash::box(
    #   #                    title = "LIVE",
    #   #                    id = "leftPanel",
    #   #                    #width = 2,
    #   #                    height = 450,
    #   #                    collapsible = TRUE#,
    #   #                  )
    #   # ),
    #   
    #   bs4Dash::tabItem(tabName = "transitsOrders",
    #                    bs4Dash::box(
    #                      title = "Transits & Orders",
    #                      id = "leftPanel",
    #                      #width = 2,
    #                      height = 450,
    #                      collapsible = TRUE,
    #                      mod_boxTable_ui(ns("boxTableTransits"))
    #                    )
    #   ),
    # 
    #   bs4Dash::tabItem(tabName = "drivers",
    #                    bs4Dash::box(
    #                      title = "Drivers",
    #                      id = "leftPanel",
    #                      #width = 2,
    #                      height = 450,
    #                      collapsible = TRUE,
    #                      mod_boxTable_ui(ns("boxTableDrivers"))
    #                    )
    #   ),      
    #         
    #   bs4Dash::tabItem(tabName = "areasGPS",
    #                    bs4Dash::box(
    #                      title = "GPS areas",
    #                      id = "leftPanel",
    #                      #width = 2,
    #                      height = 450,
    #                      collapsible = TRUE
    #                    )
    #   ),
    #   
    #   bs4Dash::tabItem(tabName = "points",
    #                    bs4Dash::box(
    #                      title = "Points on the map",
    #                      id = "leftPanel",
    #                      #width = 2,
    #                      height = 450,
    #                      collapsible = TRUE
    #                    )
    #   ),
    #   
    #   bs4Dash::tabItem(tabName = "dashboard",
    # 
    #                     bs4Dash::box(
    #                       title = "Chart1",
    #                       id = "chart1",
    #                       #width = 2,
    #                       #height = 450,
    #                       collapsible = TRUE
    #                    ),
    #                    bs4Dash::box(
    #                      title = "Chart1",
    #                      id = "chart1",
    #                      #width = 2,
    #                      #height = 450,
    #                      collapsible = TRUE
    #                    )
    #     )
    # ),
    
    
    
    # shinyjs::hidden(absolutePanel(id = ns("analysisPanel"),
    #                               tagList(shinyWidgets::actionBttn(inputId = "btnAn1", label = NULL, style = "material-circle", size = "sm", icon = icon("border-style")),
    #                                       shinyWidgets::actionBttn(inputId = "btnAn2", label = NULL, style = "material-circle", size = "sm", icon = icon("map")),
    #                                       shinyWidgets::actionBttn(inputId = "btnAn3", label = NULL, style = "material-circle", size = "sm", icon = icon("fire")),
    #                                       shinyWidgets::actionBttn(inputId = "btnAn4", label = NULL, style = "material-circle", size = "sm", icon = icon("dot-circle"))))), # pokazac tu  okregi np. 1,2,3 km od punktu odbiorcy, zeby mozna bylo recznie ustawiac radius i wtedy zrobic podsumowanie ile statusuw zostalo nadanych w danym zakresie radiusa               
    # 
    # absolutePanel(id = ns("optionsPanel"),
    #               tagList(shinyWidgets::actionBttn(inputId = "btnOpt1", label = NULL, style = "material-circle", size = "sm", icon = icon("search")),
    #                       shinyWidgets::actionBttn(inputId = "btnOpt2", label = NULL, style = "material-circle", size = "sm", icon = icon("draw-polygon")),
    #                       shinyWidgets::actionBttn(inputId = "btnOpt3", label = NULL, style = "material-circle", size = "sm", icon = icon("building")),
    #                       shinyWidgets::actionBttn(inputId = "btnOpt4", label = NULL, style = "material-circle", size = "sm", icon = icon("layer-group")))),

   
    # fluidRow(
    #   column(4,
    #     absolutePanel(id = ns("coordPanel"),
    #                   tagList(
    #                           shinyWidgets::actionBttn(inputId = ns("btnCoord1"), label = 'Nadawcy', style = "unite", icon = icon("map-marker-alt")),
    #                           shinyWidgets::actionBttn(inputId = ns("btnCoord2"), label = 'Odbiorcy', style = "unite", icon = icon("map-marker-alt")),
    #                           shinyWidgets::actionBttn(inputId = ns("btnCoord3"), label = 'App', style = "unite", icon = icon("mobile-alt")))))),

    #mod_analysisPanel_ui(ns("analysisMap")),
    
    
    #mod_boxTable_ui(ns("boxTable")),
    mod_spatialOperations_ui(ns("proxyMap")),
    mod_dashboard_ui(ns("dashboard"))
    
    #mapboxer::mapboxerOutput(ns("map"))
    #leaflet::leafletOutput(ns("map"))
    

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
      

       # btn1 = reactive({input$btnCoord1})
       # btn2 = reactive({input$btnCoord2})
       # btn3 = reactive({input$btnCoord3})

       #mod_coordPanel_server("proxyMap", data, btn1, btn2, btn3)

      
      
       mod_dashboard_server("dashboard", data)
       mod_spatialOperations_server("proxyMap", data)
       #mod_boxTable_server("boxTable", data, "proxyMap-map", session) # tu w taki sposob mozemy odwolac sie do ns('map') bedacego w module ns('proxyMap')
       
       
       
      # mod_analysisPanel_server("analysisMap", data)
       #mod_boxTable_server("boxTableTransits", data, c(1,2), mapa)
       #mod_boxTable_server("boxTableDrivers", data, 6, mapa)
      
       
       
       
       
          
      observeEvent(btnAnalysis(), {
        shinyjs::toggle(id = "proxyMap-analysis-analysisPanel",
                        anim = TRUE,
                        animType = "fade",
                        time = 0.4)
        
    })
      
      observe({   # tutaj dodac jeszcze obsluge znikania paneli jak bedzie wlaczony dashboard

          #shinyjs::hide(id = "shiny-tab-dashboard")
        
   
        
          if(sidebarDashboard() == "dashboard") {
            shinyjs::hide(id = "proxyMap-map", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-coordPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-optionsBtns-optionsPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-analysis-analysisPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::hide(id = "proxyMap-analysis-analysisPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::runjs(
              htmltools::HTML("$('#shiny-tab-dashboard').show();
                               $('#panelTransits, #panelDrivers, #panelGPS').hide();"))
            
          } else {
            shinyjs::show(id = "proxyMap-map", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::show(id = "proxyMap-coordPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::show(id = "proxyMap-optionsBtns-optionsPanel", anim = TRUE, animType = "fade", time = 0.3)
            shinyjs::runjs(
              htmltools::HTML("$('#shiny-tab-dashboard').hide();
                               $('#panelTransits, #panelDrivers, #panelGPS').show();"  )) 
          }

      })
      
      
      # observe({
      # 
      #   if(sidebarDashboard() == "transitsOrders") {
      #     # shinyjs::runjs(
      #     #   htmltools::HTML('$("#body_ui-proxyMap-btnDrawArea1").prependTo(".card-tools");'))
      #     
      #     #shinyjs::hide(id = "proxyMap-btnDrawArea2")
      #     #shinyjs::show(id = "proxyMap-btnDrawArea1")
      #     #shinyjs::toggle(id = "proxyMap-btnDrawArea1")
      # 
      #   } else if (sidebarDashboard() == "drivers") {
      #     # shinyjs::runjs(
      #     #   htmltools::HTML('$("#body_ui-proxyMap-btnDrawArea2").prependTo(".card-tools");'))          
      #     
      #     
      #     #shinyjs::hide(id = "proxyMap-btnDrawArea1")
      #     #shinyjs::show(id = "proxyMap-btnDrawArea2")
      #     #shinyjs::toggle(id = "proxyMap-btnDrawArea2")
      #   }
      #   # else {
      #   #   shinyjs::hide(id = "proxyMap-btnDrawArea1")
      #   #   shinyjs::hide(id = "proxyMap-btnDrawArea2")
      #   # }
      # 
      # })
      
      
      
      
})
}
    












