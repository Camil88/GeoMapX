
#' boxTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boxTable_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    bs4Dash::tabItems(
      bs4Dash::tabItem(tabName = "transitsOrders",
                       bs4Dash::box(
                         title = "Transits & Orders",
                         id = "panelTransits",
                         #width = 4,
                         height = 450,
                         collapsible = TRUE,
                         #actionButton(inputId = ns("btnDrawAreaTransits"), label = NULL, icon = icon("vector-square")), # bylo border-style
                         reactable::reactableOutput(ns("tableTransits"))
                       )
      ),
      
      bs4Dash::tabItem(tabName = "drivers",
                       bs4Dash::box(
                         title = "Drivers",
                         id = "panelDrivers",
                         #width = 5,
                         height = 450,
                         collapsible = TRUE,
                         #actionButton(inputId = ns("btnDrawAreaDrivers"), label = NULL, icon = icon("vector-square")),
                         reactable::reactableOutput(ns("tableDrivers"))
                       )
      ),      
      
      bs4Dash::tabItem(tabName = "areasGPS",
                       bs4Dash::box(
                         title = "GPS areas",
                         id = "panelGPS",
                         #width = 4,
                         height = 450,
                         collapsible = TRUE
                       )
      )#,
      
      # bs4Dash::tabItem(tabName = "points",
      #                  bs4Dash::box(
      #                    title = "Points on the map",
      #                    #id = "leftPanel",
      #                    #width = 4,
      #                    height = 450,
      #                    collapsible = TRUE
      #                  )
      # )#,
      

    )
    
  )
}


#' @noRd 
mod_boxTable_server <- function(id, data, passedMap, passedMapInput, passedMapInputShape, parentSession, btnDrawRectangle, btnDrawChoro){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      '%notin%' <- Negate('%in%')     
      
      
      # LEFT PANEL BOXES
      
      selectedRowTransits <- reactive(reactable::getReactableState("tableTransits", "selected"))
      selectedRowDrivers <- reactive(reactable::getReactableState("tableDrivers", "selected"))
      prevSelectedTransits <- reactiveVal()
      prevSelectedDrivers <- reactiveVal()
      visibilityDraw <- reactiveValues(showHide = FALSE)
      
      
      ###### te 2 ponizsze dotycza buttonow przekazanych z mod_mapAnalaysis
      
      visibilityDrawRectangle <- reactiveValues(showHide = FALSE)
      visibilityDrawChoro <- reactiveValues(showHide = FALSE)
      
      ##################
      
    
      dataTableTransits <- colTable(data, c(1,2,4,6))
      #dataTableDrivers <- colTable(data, c(2,3,4,6))      
      dataTableDrivers <- colTable(data, c(2,3,4,6,13))      
      allData <- colTable(data, c(1:6))
      #dataStats <- colTable(data, c(1:3,8:10,12,14)) # wczesniej uzywalem tu dataStats zamiast allData i dzialalo jakby co
      
      # te 2 ponizsze dzialaja razem - najpierw reactiveVal, potem do niego przekazujemy passedMapInput, do ktorego z poziomu btnDrawArea1
      # przekazujemy NULL przy klikniciu
      polygon_coords <- reactiveVal()
      choro_coords <- reactiveVal()

      
      # zeby zadzialalo poprawnie akcja mapa-tabelka (kartogram i zaznaczanie na mapie) MUSIMY rozdzielic obserwowanie passedMap.. na dwa
      # oddzielne observe - inaczej nie zadziala do konca dobrze - to jest sposob generalnie na rozdzielanie roznych akcji gdy uzywamy 
      # drawTolbara i na nim korzystamy z jakiegos przycisku (np. draw) ktory jest wspoldzielony przez rozne przyciski!
      
      ########################################
      
      observe({
        polygon_coords(passedMapInput())
      })
      
      observe({
        choro_coords(passedMapInputShape())
      })
      
      
      # bierzemy te punkty z mod_mapAnalysis zeby dzieki temy moc poprawnie filtrowac tabele zaznaczjac punkty na mapie. Buttony te ustawiaja
      # zawsze input$.. (akcja z mapy) na NULL, dzieki temu przy przechodzeniu miedzy buttonami (analysisPanel) zawsze zeruja (NULL) sie koordynaty
      # kliiniete (choropleth)/ zaznaczone (draw) na mapie
      
      observeEvent(btnDrawRectangle(), {
        
        polygon_coords(NULL)
        choro_coords(NULL)
        
      })
      
      observeEvent(btnDrawChoro(), {
        
        polygon_coords(NULL)
        choro_coords(NULL)
        
      })         
      
      #######################################
      
      
      commonPointsChoro <- reactive({
        
        if (is.null(choro_coords())) return()
        
        df <- districts %>%
          dplyr::filter(id == choro_coords()$id)
        
        #returnCommonPoints <- dataTableTransits[df,]
        returnCommonPoints <- allData[df,]
        returnCommonPoints
        
      })    
      

      commonPoints <- reactive({
        
        rbind_coords <- do.call(rbind,lapply(polygon_coords(),function(x){c(x[[1]][1],x[[2]][1])}))
        polygon_object <- sf::st_polygon(list(rbind_coords))
        returnCommonPoints <- allData[polygon_object,] 
        returnCommonPoints
        
      })        
      

      
      # df filtered by points selected on a map (this reactive df is passed to table as data)

      reactiveDfTransits <- reactive({

        if (!is.null(polygon_coords()) ) { 
          newDf <- dataTableTransits[commonPoints(),]
        } else if (!is.null(choro_coords())) {
          newDf <- dataTableTransits[commonPointsChoro(),]
        } else {
          newDf <- dataTableTransits
        }

      })

          
      reactiveDfDrivers <- reactive({
        
        if (!is.null(polygon_coords()) ) { 
          newDf <- dataTableDrivers[commonPoints(),]
        } else if (!is.null(choro_coords())) {
          newDf <- dataTableDrivers[commonPointsChoro(),]
        } else {
          newDf <- dataTableDrivers
        }
      }) 
 

  
      toListen <- reactive({
        list(reactiveDfTransits()[selectedRowTransits(),], 
             reactiveDfDrivers()[selectedRowDrivers(),])
      })
      
      
      # tables for boxes
      output$tableTransits <- reactable::renderReactable({
        reactable::reactable(
          #data = dataMainRowsTransits,
          data = reactiveDfTransits(),
          #reactiveDf(),
          searchable = TRUE,
          pagination = FALSE,
          highlight = TRUE,
          compact = TRUE,
          height = 400,
          width = 310, #bylo 330
          selection = "multiple", 
          onClick = "select",
          groupBy = "transitNr",
          
          
          # details = function(index){
          #   #extraData <- subset(dataTableTransits, dataTableTransits[[1]] == dataTableMainRows[[1]][index])[detailsLevel+1]
          #   extraData <- subset(dataTableTransits, dataTableTransits[[1]] == dataMainRowsTransits[[1]][index])[2]
          #   htmltools::div(style = "padding: 16px; color: black;",
          #                  reactable::reactable(extraData, outlined = TRUE)
          #                 )
          # },
          
          
          
          columns = list(
            transitNr = reactable::colDef(
              style = list(fontSize = 14),
              width = 140
            ),
            #LP = reactable::colDef(
            orderNr = reactable::colDef(
              width = 160, #bylo 180
              cell = function(value, index) {
                
                stops <- reactiveDfTransits()$customerName[index]
                transitType <- reactiveDfDrivers()$transitTypeShort[index]
                
                if (transitType == "D") {
                  classCSS <- "delivery"
                } else {
                  classCSS <- "pickup"
                }
                
                info_tag <- div(style = list(float = "right"), span(class = classCSS, transitType)) #podmienic na to gdy dane z sql
                #info_tag <- div(style = list(float = "left"), span(class = classCSS, transitType))
                tagList(
                  div(style = list(fontWeight = 600), value, info_tag),
                  div(style = list(fontSize = 12), stops)
                )
              }
            ),
            customerName = reactable::colDef(show = FALSE),
            orderNr = reactable::colDef(show = FALSE),
            transitTypeShort = reactable::colDef(show = FALSE),
            geometry = reactable::colDef(show = FALSE)
          ),
          
          theme = reactable::reactableTheme(
            backgroundColor = "#343a40",
            highlightColor =  "#2e2d30",
            borderColor = "gray",
            searchInputStyle = list(width = 165,backgroundColor = "#343a40", border = "1px solid gray"),
            rowSelectedStyle = list(backgroundColor = "#222d38", boxShadow = "inset 2px 0 0 0 #151a1f")#,
            #rowHighlightStyle = list(width = "370px")
          )         
          
        )
      })
      
      
      output$tableDrivers <- reactable::renderReactable({
        reactable::reactable(
          data = reactiveDfDrivers(),
          searchable = TRUE,
          pagination = FALSE,
          highlight = TRUE,
          compact = TRUE,
          height = 400,
          width = 370,
          selection = "multiple", 
          onClick = "select",
          groupBy = "driverName",
          
          columns = list(
            #Kierowca = reactable::colDef(
            driverName = reactable::colDef(
              style = list(fontSize = 14),
              width = 200
            ),

            orderNr = reactable::colDef(
              width = 165,
              cell = function(value, index) {
                #stops <- reactiveDfDrivers()$nazwaOdbiorcyNadawcy[index]
                distance <- reactiveDfDrivers()$distanceKm[index]
                transitType <- reactiveDfDrivers()$transitTypeShort[index]
                
                if (transitType == "D") {
                  classCSS <- "delivery"
                } else {
                  classCSS <- "pickup"
                }
                
                info_tag <- div(style = list(float = "right"), span(class = classCSS, transitType)) #podmienic na to gdy dane z sql
                #info_tag <- div(style = list(float = "left"), span(class = classCSS, transitType))
                tagList(
                  div(style = list(fontWeight = 600), value, info_tag),
                  div(style = list(fontSize = 12), paste("distance:", distance, " km"))
                )
              }
            ),
            customerName = reactable::colDef(show = FALSE),
            transitTypeShort = reactable::colDef(show = FALSE),
            geometry = reactable::colDef(show = FALSE)
          ),
          
          theme = reactable::reactableTheme(
            backgroundColor = "#343a40",
            highlightColor =  "#2e2d30",
            borderColor = "gray",
            searchInputStyle = list(width = 165,backgroundColor = "#343a40", border = "1px solid gray"),
            rowSelectedStyle = list(backgroundColor = "#222d38", boxShadow = "inset 2px 0 0 0 #151a1f")
          )                             
        )
      })
      
      
      
      
      # show/hide points on a map based on row clicked
      observeEvent(toListen(), {
        
        popIcons <- leaflet.extras::pulseIcons(color = "#007bff",heartbeat = 0.8)
        
        lenSelectedTransits <- length(selectedRowTransits())
        lenPrevSelectedTransits <- length(prevSelectedTransits())
        lenSelectedDrivers <- length(selectedRowDrivers())
        lenPrevSelectedDrivers <- length(prevSelectedDrivers())
        
        layerClickedTransits <- as.character(selectedRowTransits())
        layerClickedDrivers <- as.character(selectedRowDrivers())
        
        layerToHideTransits <- NULL
        layerToHideDrivers <- NULL
        
        if (!is.null(prevSelectedTransits()) | !is.null(prevSelectedDrivers()) ) {
          layerToHideTransits <- subset(prevSelectedTransits(), prevSelectedTransits() %notin% selectedRowTransits() )
          layerToHideDrivers <- subset(prevSelectedDrivers(), prevSelectedDrivers() %notin% selectedRowDrivers() )
        }
        
        
        if (lenSelectedTransits > lenPrevSelectedTransits ) {
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet.extras::addPulseMarkers(
              data = reactiveDfTransits()[selectedRowTransits(),],
              group = "points",
              layerId = layerClickedTransits,
              icon = popIcons
            )

          
        } else if (lenSelectedTransits < lenPrevSelectedTransits) {
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::removeMarker(layerToHideTransits)
        }
        
        
        if (lenSelectedDrivers > lenPrevSelectedDrivers ) {
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet.extras::addPulseMarkers(
              data = reactiveDfDrivers()[selectedRowDrivers(),],
              group = "points",
              layerId = layerClickedDrivers,
              icon = popIcons
            )
          
        } else if (lenSelectedDrivers < lenPrevSelectedDrivers) {
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::removeMarker(layerToHideDrivers)
        }        
        
        prevSelectedTransits(selectedRowTransits())
        prevSelectedDrivers(selectedRowDrivers())
        
      }) 
      
      
      
      # draw buttons - boxes: zakomentowane bo bylo uzywane do buttona w boxie na gorze (rysowanie obszaru na mapie)
      
      # observeEvent(input$btnDrawAreaTransits, {
      #   
      #   if (visibilityDraw$showHide) {
      #     
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     #print(polygon_coords())
      #     polygon_coords(NULL) # here we have to clear up coordinates every time drawing is
      #     
      #     shinyjs::runjs(
      #       htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
      #                        var cancel = elementsCancel.item(0); 
      #                        cancel.firstElementChild.click();   
      #       
      #                        document.querySelector(".leaflet-draw-edit-remove").click();       
      #                        var elements = document.getElementsByClassName("leaflet-draw-actions")[1].children;
      #                        var clearAll = elements.item(2); 
      #                        clearAll.firstElementChild.click();' ))  
      #     
      #     #reactable::updateReactable("tableTransits", data = reactiveDf())
      #     
      #   } else {
      #     
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     
      # 
      #     #print(polygon_coords())
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-draw-rectangle").click();'))
      #     
      #   }
      # })
      # 
      # 
      # 
      # observeEvent(input$btnDrawAreaDrivers, {
      #   
      #   if (visibilityDraw$showHide) {
      #     
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     
      #     polygon_coords(NULL)
      #     
      #     shinyjs::runjs(
      #       htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
      #                        var cancel = elementsCancel.item(0);
      #                        cancel.firstElementChild.click();
      # 
      #                        document.querySelector(".leaflet-draw-edit-remove").click();
      #                        var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
      #                        var clearAll = elementsClearAll.item(2);
      #                        clearAll.firstElementChild.click();' ))
      #     
      #   } else {
      #     
      #     visibilityDraw$showHide = !visibilityDraw$showHide
      #     
      #     shinyjs::runjs(
      #       htmltools::HTML('document.querySelector(".leaflet-draw-draw-rectangle").click();'))
      #     
      #   }
      # })  
      
      
      
      
      # this retyrn must be at the end to return value and make other components working
      # jak przekazac reactiveExpr z tego modulu do np. mod_analysis? czy mozna przekazac reactiveExpr czy same inputy np. input$btn?



      
      
    }
  )}









