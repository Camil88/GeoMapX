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

      
      
      
      drawnShape <- mod_mapAnalysis_server("analysis", data, "map", reactive({input$map_draw_new_feature$geometry$coordinates[[1]]}), 
                                           reactive({input$map_shape_click}), session) # przekazujemy do mapAnalysis reactive expr z modulu mod_boxTable (na dole sa ustawione)
      
      mod_boxTable_server("boxTable", data, "map", reactive({input$map_draw_new_feature$geometry$coordinates[[1]]}), 
                          reactive({input$map_shape_click}), session, drawnShape$btnDrawRectangle, drawnShape$btnDrawChoro)
      
      mod_optionsPanel_server("optionsBtns","map", reactive({input$map_draw_new_feature$geometry$coordinates[[1]]}), session)

      
      # voivodeship <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-medium.geojson")
      # poviats <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/powiaty/powiaty-medium.geojson")     
   
      '%notin%' <- Negate('%in%')

      

      
      # Visibility - coords panel
      visibilityReceipt <- reactiveValues(showHide = FALSE)
      visibilityDelivery <- reactiveValues(showHide = TRUE)
      visibilityAppReceipt <- reactiveValues(showHide = FALSE)
      visibilityAppDelivery <- reactiveValues(showHide = TRUE)
      visibilityAppALL <- reactiveValues(showHide = FALSE)

      # # Visibility - analysis panel      
      # visibilityMapVoivod <- reactiveValues(showHide = FALSE)
      # visibilitySearchBar <- reactiveValues(showHide = FALSE)
      
      coordCustReceipt <- coordCustomers(data, "pickup")
      coordCustDelivery <- coordCustomers(data,"delivery")
      coordAppReceipt <- coordApplication(data, "pickup")
      coordAppDelivery <- coordApplication(data, "delivery")
      coordAppALL <- coordApplication(data, c("pickup","delivery"))

      # nizej do odkomentowania jak bede zmianieal na PL
      # coordCustReceipt <- coordCustomers(data, "zaladunek")
      # coordCustDelivery <- coordCustomers(data,"rozladunek")
      # coordAppReceipt <- coordApplication(data, "zaladunek")
      # coordAppDelivery <- coordApplication(data, "rozladunek")
      # coordAppALL <- coordApplication(data, c("zaladunek","rozladunek"))
      
      linesReceipt <- createLines(data, "pickup")
      linesDelivery <- createLines(data, "delivery")
      
      # nizej do odkomentowania jak bede zmianieal na PL
      #linesReceipt <- createLines(data, "zaladunek")
      #linesDelivery <- createLines(data, "rozladunek") 
      

      
      labelStyle <- list("color" = "white",
                         "background-color" = "#343a40",
                         "font-size" = "11px",
                         "border-style" = "none")     
      
      
     
      # main map
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(coordAppALL,
          options = leaflet::leafletOptions(zoomControl = FALSE)) %>% 
          leaflet::setView(19.145136, 51.919438, zoom = 7) %>%
          
          leaflet.mapboxgl::addMapboxGL(
            style = "mapbox://styles/mapbox/light-v10",
            #options = mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$light_v10, center =c(19.145136, 51.919438) ),
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

          # leaflet.mapboxgl::addMapboxGL(
          #   style = "mapbox://styles/mapbox/satellite-v11",
          #   setView = FALSE,
          #   group = "Satellite"
          # ) %>%
          
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
            #targetGroup = "draw",
            circleOptions = FALSE,
            markerOptions = FALSE,
            singleFeature = TRUE, 
            editOptions = leaflet.extras::editToolbarOptions()
          ) %>% 

          leaflet.extras::addSearchOSM(
            options = leaflet.extras::searchOptions(
              minLength = 2)
          ) %>%
          
          leaflet::addLayersControl(
            baseGroups = c("Light", "Streets", "Dark"), 
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )

      })   

      
      
      # buttons 
      
      observeEvent(input$btnCoord1, {
        
        if(visibilityAppReceipt$showHide) {
          
          visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide
          visibilityReceipt$showHide = !visibilityReceipt$showHide
          #print(paste("ReceiptHide1" ,visibilityReceipt$showHide,visibilityAppReceipt$showHide))
          
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
          #print(paste("ReceiptShow1",visibilityReceipt$showHide,visibilityAppReceipt$showHide))
          
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
            leaflet::hideGroup("linesDelivery") #%>%              
          #leaflet::clearGroup("linesReceipt")             
          
        } else {
          
          visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide
          visibilityDelivery$showHide = TRUE
          visibilityAppReceipt$showHide = FALSE
          visibilityReceipt$showHide = FALSE
          visibilityAppALL$showHide = FALSE
          #print(visibilityAppReceipt$showHide)
          
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
            #print(paste("ReceiptHide3 ",visibilityReceipt$showHide, visibilityAppReceipt$showHide))
            leaflet::leafletProxy("map") %>%
              leaflet::clearGroup("appReceipt") %>% 
              leaflet::clearGroup("linesReceipt")
            
          } else {
            
            visibilityAppReceipt$showHide = !visibilityAppReceipt$showHide
            #print(paste("ReceiptShow3 ",visibilityReceipt$showHide, visibilityAppReceipt$showHide))
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
            #print(paste("DeliveryHide3 ",visibilityDelivery$showHide,visibilityAppDelivery$showHide))
            leaflet::leafletProxy("map") %>%
              leaflet::hideGroup("appDelivery") %>%   
              leaflet::hideGroup("linesDelivery")   
            
          } else {
            
            visibilityAppDelivery$showHide = !visibilityAppDelivery$showHide
            #print(paste("DeliveryShow3 ",visibilityDelivery$showHide, visibilityAppDelivery$showHide))
            leaflet::leafletProxy("map") %>%
              leaflet::showGroup("appDelivery") %>% 
              leaflet::showGroup("linesDelivery")
            
          }
        } else if (visibilityDelivery$showHide == FALSE & visibilityReceipt$showHide == FALSE) {
          
          if (visibilityAppALL$showHide) {
            
            visibilityAppALL$showHide = !visibilityAppALL$showHide
            
            #print(paste("ComboHide3" ,visibilityDelivery$showHide ,visibilityReceipt$showHide))
            #print(visibilityReceipt$showHide)
            
            leaflet::leafletProxy("map") %>%
              leaflet::clearGroup("appALL")           
            
            
          } else {
            
            visibilityAppALL$showHide = !visibilityAppALL$showHide
            
            # print(visibilityAppDelivery$showHide)
            # print(visibilityAppReceipt$showHide)
            #print(paste("ComboShow3" ,visibilityDelivery$showHide ,visibilityReceipt$showHide))
            
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
      

      
    
      # observeEvent(input$btnAnl2, {
      #   #voivodeship <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-medium.geojson")
      #   
      #   if(visibilityMapVoivod$showHide) {
      #     
      #     visibilityMapVoivod$showHide = !visibilityMapVoivod$showHide
      #     
      #     leaflet::leafletProxy("map") %>%
      #       leaflet::clearGroup("voivodeship")
      #   } else {
      #   
      #     visibilityMapVoivod$showHide = !visibilityMapVoivod$showHide  
      #     
      #     leaflet::leafletProxy("map") %>%
      #       leaflet::addPolygons(
      #         data = voivodeship,
      #         group = "voivodeship",
      #         #fillColor = "green",
      #         weight = 2,
      #         opacity = 0.3,
      #         color = "#6b747d",
      #         fillOpacity = 0.2,
      #         highlight = leaflet::highlightOptions(
      #           weight = 2,
      #           color = "#9b22bf",
      #           opacity = 0.8,
      #           fillOpacity = 0.5,
      #           bringToFront = TRUE)
      #       )}
      # 
      # })
        
        
      
      # LEFT PANEL BOXES

      # polygon_coords <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]]})
      # selectedRowTransits <- reactive(reactable::getReactableState("tableTransits", "selected"))
      # selectedRowDrivers <- reactive(reactable::getReactableState("tableDrivers", "selected"))
      # prevSelectedTransits <- reactiveVal()
      # prevSelectedDrivers <- reactiveVal()
      # #commonPointsTransits <- reactiveVal()
      # #commonPointsDrivers <- reactiveVal()
      # #commonPoints<- reactiveVal()
      # 
      # visibilityDraw <- reactiveValues(showHide = FALSE)
      # 
      # 
      # 
      # dataTableTransits <- colTable(data, c(1,2))
      # dataTableDrivers <- colTable(data, c(6,2))
      # 
      # 
      # 
      # # df filtered by points selected on a map (this reactive df is passed to table as data)
      # reactiveDf <- reactive({
      # 
      #   if (is.null(polygon_coords())) {
      #     newDf <- dataTableTransits
      #   } else {
      #     newDf <- dataTableTransits[commonPoints(),]
      # 
      #   }
      # })
      # 
      # 
      # commonPoints <- reactive({
      # 
      #   rbind_coords <- do.call(rbind,lapply(polygon_coords(),function(x){c(x[[1]][1],x[[2]][1])}))
      #   polygon_object <- sf::st_polygon(list(rbind_coords))
      #   returnCommonPoints <- dataTableTransits[polygon_object,]
      #   returnCommonPoints
      # 
      # })
      # 
      # 
      # commonPointsWholeCountry <- reactive({
      #   
      #   polygon_object <- sf::st_polygon(list(rbind(c(48.86471476180277, 13.77685546875), 
      #                                               c(48.86471476180277, 24.27978515625), 
      #                                               c(54.939765758658936, 24.27978515625), 
      #                                               c(54.939765758658936, 13.77685546875),
      #                                               c(48.86471476180277, 13.77685546875))))
      #   returnCommonPoints <- dataTableTransits[polygon_object,]
      #   returnCommonPoints
      #   
      # })      
      # 
      # toListen <- reactive({
      #   #list(dataTableTransits[selectedRowTransits(),],dataTableDrivers[selectedRowDrivers(),])
      #   list(reactiveDf()[selectedRowTransits(),],reactiveDf()[selectedRowDrivers(),])
      # })
      # 
      # 
      # # tables for boxes
      # output$tableTransits <- reactable::renderReactable({
      #   reactable::reactable(
      #                        reactiveDf(),
      #                        searchable = TRUE,
      #                        pagination = FALSE,
      #                        height = 400,
      #                        highlight = TRUE,
      #                        theme = reactable::reactableTheme(
      #                          backgroundColor = "#343a40",
      #                          highlightColor = "#414142",
      #                          searchInputStyle = list(width = "70%",backgroundColor = "#343a40", border = "1px solid #797a80"),
      #                          rowSelectedStyle = list(backgroundColor = "#283a4d", boxShadow = "inset 2px 0 0 0 #ffa62d")
      #                        ),
      #                        columns = list(
      #                          geometry = reactable::colDef(show = FALSE)
      #                        ),
      #                        selection = "multiple",
      #                        onClick = "select",
      #                        compact = TRUE,
      #                        groupBy = "ZLP"
      #   )
      # })
      # 
      # output$tableDrivers <- reactable::renderReactable({
      #   reactable::reactable(dataTableDrivers,
      #                        searchable = TRUE,
      #                        pagination = FALSE,
      #                        height = 400,
      #                        highlight = TRUE,
      #                        theme = reactable::reactableTheme(
      #                          backgroundColor = "#343a40",
      #                          highlightColor = "#414142",
      # 
      #                          searchInputStyle = list(width = "70%",backgroundColor = "#343a40", border = "1px solid #797a80"),
      #                          rowSelectedStyle = list(backgroundColor = "#283a4d", boxShadow = "inset 2px 0 0 0 #ffa62d")
      #                        ),
      #                        columns = list(
      #                          geometry = reactable::colDef(show = FALSE)
      #                        ),
      #                        selection = "multiple",
      #                        onClick = "select",
      #                        compact = TRUE,
      #                        groupBy = "Kierowca"
      #   )
      # })
      # 
      # 
      # 
      # 
      # # show/hide points on a map based on row clicked
      # observeEvent(toListen(), {
      # 
      #   popIcons <- leaflet.extras::pulseIcons(color = "#1b0bb3",heartbeat = 0.8)
      # 
      #   lenSelectedTransits <- length(selectedRowTransits())
      #   lenPrevSelectedTransits <- length(prevSelectedTransits())
      #   lenSelectedDrivers <- length(selectedRowDrivers())
      #   lenPrevSelectedDrivers <- length(prevSelectedDrivers())
      # 
      #   layerClickedTransits <- as.character(selectedRowTransits())
      #   layerClickedDrivers <- as.character(selectedRowDrivers())
      # 
      #   layerToHideTransits <- NULL
      #   layerToHideDrivers <- NULL
      # 
      #   if (!is.null(prevSelectedTransits()) | !is.null(prevSelectedDrivers()) ) {
      #     layerToHideTransits <- subset(prevSelectedTransits(), prevSelectedTransits() %notin% selectedRowTransits() )
      #     layerToHideDrivers <- subset(prevSelectedDrivers(), prevSelectedDrivers() %notin% selectedRowDrivers() )
      #   }
      # 
      # 
      #   if (lenSelectedTransits > lenPrevSelectedTransits ) {
      #     leaflet::leafletProxy("map") %>%
      #       leaflet.extras::addPulseMarkers(
      #         data = reactiveDf()[selectedRowTransits(),],
      #         group = "points",
      #         layerId = layerClickedTransits,
      #         icon = popIcons
      #       )
      #     print(reactiveDf()[selectedRowTransits(),])
      # 
      #   } else if (lenSelectedTransits < lenPrevSelectedTransits) {
      #     leaflet::leafletProxy("map") %>%
      #       leaflet::removeMarker(layerToHideTransits) #%>%
      #   }
      # 
      # 
      #   if (lenSelectedDrivers > lenPrevSelectedDrivers ) {
      #     leaflet::leafletProxy("map") %>%
      #       leaflet.extras::addPulseMarkers(
      #         data = dataTableDrivers[selectedRowDrivers(),],
      #         group = "points",
      #         layerId = layerClickedDrivers,
      #         icon = popIcons
      #       )
      # 
      #   } else if (lenSelectedDrivers < lenPrevSelectedDrivers) {
      #     leaflet::leafletProxy("map") %>%
      #       leaflet::removeMarker(layerToHideDrivers) #%>%
      #   }
      # 
      # 
      #   prevSelectedTransits(selectedRowTransits())
      #   prevSelectedDrivers(selectedRowDrivers())
      # 
      # 
      # })
      # 
      # 
      # 
      # # draw buttons - boxes
      # observeEvent(input$btnDrawArea1, {
      # 
      #   if (visibilityDraw$showHide) {
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-edit-remove").click();
      #                         var elements = document.getElementsByClassName("leaflet-draw-actions")[1].children;
      #                         var clearAll = elements.item(2);
      #                         clearAll.firstElementChild.click();'))
      # 
      #   } else {
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      # 
      #     reactable::updateReactable("tableTransits", data = commonPointsWholeCountry())
      # 
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-draw-rectangle").click();'))
      # 
      #   }
      # })
      # 
      # 
      # observeEvent(input$btnDrawArea2, {
      # 
      #   if (visibilityDraw$showHide) {
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-edit-remove").click();
      #                         var elements = document.getElementsByClassName("leaflet-draw-actions")[1].children;
      #                         var clearAll = elements.item(2);
      #                         clearAll.firstElementChild.click();'))
      # 
      #   } else {
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      # 
      # 
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-draw-rectangle").click();'))
      # 
      #   }
      # })
           
      
      
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
       
            
    })
}