#' coordPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spatialOperations_ui <- function(id){
    ns <- NS(id)
    
    tagList(
    
    fluidRow(
      column(4,
             absolutePanel(id = ns("coordPanel"),
                           tagList(
                             shinyWidgets::actionBttn(inputId = ns("btnCoord1"), label = 'Pick-up', style = "simple", icon = icon("map-marker-alt")),
                             shinyWidgets::actionBttn(inputId = ns("btnCoord2"), label = 'Delivery', style = "simple", icon = icon("map-marker-alt")),
                             shinyWidgets::actionBttn(inputId = ns("btnCoord3"), label = 'App', style = "simple", icon = icon("mobile-alt")))))),
    
    mod_optionsPanel_ui(ns("optionsBtns")),
    mod_mapAnalysis_ui(ns("analysis")),
    mod_boxTable_ui(ns("boxTable")),
    leaflet::leafletOutput(ns("map"))
)
  }  
    
 
     
#' @noRd 
mod_spatialOperations_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      '%notin%' <- Negate('%in%')
      
      mod_optionsPanel_server("optionsBtns")
      
      drawnShape <- mod_mapAnalysis_server("analysis", data, "map", reactive({input$map_draw_new_feature$geometry$coordinates[[1]]}), 
                                           reactive({input$map_shape_click}), session)
      
      mod_boxTable_server("boxTable", data, "map", reactive({input$map_draw_new_feature$geometry$coordinates[[1]]}), 
                          reactive({input$map_shape_click}), session, drawnShape$btnDrawRectangle, drawnShape$btnDrawChoro, drawnShape$btnDrawHeat)
      


      # Visibility - coords panel (buttons logic)
      visibilityReceipt <- reactiveValues(showHide = FALSE)
      visibilityDelivery <- reactiveValues(showHide = TRUE)
      visibilityAppReceipt <- reactiveValues(showHide = FALSE)
      visibilityAppDelivery <- reactiveValues(showHide = TRUE)
      visibilityAppALL <- reactiveValues(showHide = FALSE)
      
      coordCustReceipt <- coordCustomers(data, "pickup")
      coordCustDelivery <- coordCustomers(data,"delivery")
      coordAppReceipt <- coordApplication(data, "pickup")
      coordAppDelivery <- coordApplication(data, "delivery")
      coordAppALL <- coordApplication(data, c("pickup","delivery"))
      
      linesReceipt <- createLines(data, "pickup")
      linesDelivery <- createLines(data, "delivery")
      
      labelStyle <- list("color" = "white",
                         "background-color" = "#343a40",
                         "font-size" = "11px",
                         "border-style" = "none")     
      
      
    
      # MAIN MAP
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(coordAppALL,
          options = leaflet::leafletOptions(zoomControl = FALSE)) %>% 
          leaflet::setView(19.145136, 51.919438, zoom = 7) %>%
          leaflet.mapboxgl::addMapboxGL(
            style = "mapbox://styles/mapbox/light-v10",
            setView = FALSE,
            group = "Light"
          ) %>%
          
          leaflet.mapboxgl::addMapboxGL(
            style = "mapbox://styles/mapbox/streets-v11",
            setView = FALSE,
            group = "Streets"
          ) %>%
          
          leaflet.mapboxgl::addMapboxGL(
            style = "mapbox://styles/hwl/ckmuuqnqm5cgl17nn4soev09l",
            setView = FALSE,
            group = "Dark"
          ) %>%

          leaflet::addCircleMarkers(
            data = coordCustDelivery,
            group = "custDelivery",
            fillOpacity = 0.6,
            radius = 6,
            stroke = FALSE,
            fillColor = "#952444",
            label = lapply(createLabel(coordCustDelivery), htmltools::HTML),
            labelOptions = leaflet::labelOptions(
              style = labelStyle)
          ) %>% 
          
          leaflet::addCircleMarkers(
            data = coordAppDelivery,
            group = "appDelivery",
            fillOpacity = 0.6,
            radius = 6,
            stroke = FALSE,
            fillColor = "#042f66",
            label = lapply(createLabel(coordAppDelivery), htmltools::HTML),
            labelOptions = leaflet::labelOptions(
              style = labelStyle)
          ) %>% 
          
          leaflet::addPolylines(
            data = linesDelivery,
            group = "linesDelivery",
            fillOpacity = 0.6,
            stroke = TRUE,
            weight = 1,
            color = "#6b747d"
          ) %>% 
          
          leaflet.extras::addDrawToolbar(
            circleOptions = FALSE,
            markerOptions = FALSE,
            singleFeature = TRUE, 
            editOptions = leaflet.extras::editToolbarOptions()
          ) %>% 

          leaflet.extras::addSearchOSM(
            options = leaflet.extras::searchOptions(
              minLength = 2,
              hideMarkerOnCollapse = TRUE)
          ) %>%
          
          leaflet::addLayersControl(
            baseGroups = c("Light", "Streets", "Dark"), 
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )

      })   

      
      # buttons displaying points on a map (bottom panel)
      
      observeEvent(input$btnCoord1, {
        
        if(visibilityAppReceipt$showHide) {
          
          visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide
          visibilityReceipt$showHide = !visibilityReceipt$showHide
          
          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("custReceipt") %>% 
            leaflet::clearGroup("linesReceipt") %>% 
            leaflet::hideGroup("custDelivery") %>%
            leaflet::hideGroup("appDelivery")
          
        } else {
          
          visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide
          visibilityReceipt$showHide = TRUE
          visibilityAppDelivery$showHide = FALSE
          visibilityDelivery$showHide = FALSE
          visibilityAppALL$showHide = FALSE
          
          leaflet::leafletProxy("map") %>%
            leaflet::hideGroup("custDelivery") %>%
            leaflet::hideGroup("appDelivery") %>%
            leaflet::hideGroup("linesDelivery") %>%
            leaflet::clearGroup("appALL") %>%
            leaflet::clearGroup("appReceipt") %>%
            leaflet::clearGroup("linesReceipt") %>%
            leaflet::addCircleMarkers(
              data = coordCustReceipt,
              group = "custReceipt",
              fillOpacity = 0.6,
              radius = 6,
              stroke = FALSE,
              fillColor = "#11a679",
              label = lapply(createLabel(coordCustReceipt), htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                style = labelStyle)
            ) %>%
            
            leaflet::addCircleMarkers(
              data = coordAppReceipt,
              group = "appReceipt",
              fillOpacity = 0.6,
              radius = 6,
              stroke = FALSE,
              fillColor = "#042f66",
              label = lapply(createLabel(coordAppReceipt), htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                style = labelStyle)
            ) %>% 
            
            leaflet::addPolylines(
              data = linesReceipt,
              group = "linesReceipt",
              fillOpacity = 0.6,
              stroke = TRUE,
              weight = 1,
              color = "#6b747d"
            ) 

        }
      })
      
      
      observeEvent(input$btnCoord2, {
        
        if(visibilityAppDelivery$showHide) {
          
          visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide
          visibilityDelivery$showHide = !visibilityDelivery$showHide
          
          leaflet::leafletProxy("map") %>%
            leaflet::hideGroup("custDelivery") %>%              
            leaflet::hideGroup("linesDelivery")             
          
        } else {
          
          visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide
          visibilityDelivery$showHide = TRUE
          visibilityAppReceipt$showHide = FALSE
          visibilityReceipt$showHide = FALSE
          visibilityAppALL$showHide = FALSE
          
          leaflet::leafletProxy("map") %>%
            leaflet::showGroup("custDelivery") %>%
            leaflet::showGroup("appDelivery") %>%
            leaflet::showGroup("linesDelivery") %>%
            leaflet::clearGroup("appReceipt") %>%
            leaflet::clearGroup("custReceipt") %>% 
            leaflet::clearGroup("linesReceipt") %>% 
            leaflet::clearGroup("appALL")
          
        }
        
      })       
      
      
      observeEvent(input$btnCoord3, {
        
        if(visibilityReceipt$showHide) {
          
          if (visibilityAppReceipt$showHide) {
            
            visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide

            leaflet::leafletProxy("map") %>%
              leaflet::clearGroup("appReceipt") %>% 
              leaflet::clearGroup("linesReceipt")
            
          } else {
            
            visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide

            leaflet::leafletProxy("map") %>%
              leaflet::clearGroup("appReceipt") %>%
              leaflet::clearGroup("linesReceipt") %>%
              leaflet::addCircleMarkers(
                data = coordAppReceipt,
                group = "appReceipt",
                fillOpacity = 0.6,
                radius = 6,
                stroke = FALSE,
                fillColor = "#042f66",
                label = lapply(createLabel(coordAppReceipt), htmltools::HTML),
                labelOptions = leaflet::labelOptions(
                  style = labelStyle)
              ) %>% 
              
              leaflet::addPolylines(
                data = linesReceipt,
                group = "linesReceipt",
                fillOpacity = 0.6,
                stroke = TRUE,
                weight = 1,
                color = "#6b747d"
              )                
            
          }
        } else if (visibilityDelivery$showHide) {
          
          if (visibilityAppDelivery$showHide) {
            
            visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide

            leaflet::leafletProxy("map") %>%
              leaflet::hideGroup("appDelivery") %>%   
              leaflet::hideGroup("linesDelivery")   
            
          } else {
            
            visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide

            leaflet::leafletProxy("map") %>%
              leaflet::showGroup("appDelivery") %>% 
              leaflet::showGroup("linesDelivery")
            
          }
          
        } else if (visibilityDelivery$showHide == FALSE & visibilityReceipt$showHide == FALSE) {
          
          if (visibilityAppALL$showHide) {
            
            visibilityAppALL$showHide = !visibilityAppALL$showHide
            
            leaflet::leafletProxy("map") %>%
              leaflet::clearGroup("appALL")           
            
          } else {
            
            visibilityAppALL$showHide = !visibilityAppALL$showHide
            
            leaflet::leafletProxy("map") %>%
              leaflet::hideGroup("appDelivery") %>% 
              leaflet::clearGroup("appReceipt") %>%
              leaflet::clearGroup("appALL") %>%
              leaflet::addCircleMarkers(
                data = coordAppALL,
                group = "appALL",
                fillOpacity = 0.6,
                radius = 6,
                stroke = FALSE,
                fillColor = "#042f66",
                label = lapply(createLabel(coordAppALL), htmltools::HTML),
                labelOptions = leaflet::labelOptions(
                  style = labelStyle)
              )  
          }
        }      
      }) 
      
    })
}