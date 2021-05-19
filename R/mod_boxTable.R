
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
                         height = 450,
                         collapsible = TRUE,
                         reactable::reactableOutput(ns("tableTransits"))
                       )
      ),      
      bs4Dash::tabItem(tabName = "drivers",
                       bs4Dash::box(
                         title = "Drivers",
                         id = "panelDrivers",
                         height = 450,
                         collapsible = TRUE,
                         reactable::reactableOutput(ns("tableDrivers"))
                       )
      ),            
      bs4Dash::tabItem(tabName = "reports",
                       bs4Dash::box(
                         title = "Reports",
                         id = "panelReports",
                         #width = 4,
                         height = 120,
                         collapsible = FALSE,
                         downloadButton(ns("downloadExcel"), icon = icon("file-export"), "Generate .xlsx report")
                       )
      )            
    )   
  )
}


#' @noRd 
mod_boxTable_server <- function(id, data, passedMap, passedMapInput, passedMapInputShape, parentSession, btnDrawRectangle, btnDrawChoro, btnDrawHeatMap){
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
      
      ################## reactives for buttons passed from mod_mapAnalaysis
      
      visibilityDrawRectangle <- reactiveValues(showHide = FALSE)
      visibilityDrawChoro <- reactiveValues(showHide = FALSE)
      
      ##################
      
      polygon_coords <- reactiveVal()
      choro_coords <- reactiveVal()
    
      dataTableTransits <- colTable(data, c(1,2,4,6))
      dataTableDrivers <- colTable(data, c(2,3,4,6,13))      
      allData <- colTable(data, c(1:6))
            
      observe({
        polygon_coords(passedMapInput())
      })
      
      observe({
        choro_coords(passedMapInputShape())
      })
      
      # buttons passed from mod_mapAnalysis
      observeEvent(btnDrawRectangle(), {        
        polygon_coords(NULL)
        choro_coords(NULL)        
      })
      
      observeEvent(btnDrawChoro(), {      
        polygon_coords(NULL)
        choro_coords(NULL)       
      })   
      
      
      observeEvent(btnDrawHeatMap(), {        
        polygon_coords(NULL)
        choro_coords(NULL)       
      })  
            
      # selected points/choropleth on a map retuned as reactive
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
           
      # df filtered by points selected on a map (this reactive df is passed to table as final data)
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
          data = reactiveDfTransits(),
          searchable = TRUE,
          pagination = FALSE,
          highlight = TRUE,
          compact = TRUE,
          height = 400,
          width =  280,
          selection = "multiple", 
          onClick = "select",
          groupBy = "transitNr",
          columns = list(
            transitNr = reactable::colDef(
              style = list(fontSize = 14),
              width = 140
            ),
            orderNr = reactable::colDef(
              width = 90,
              style = list(
                fontWeight = 300,
                fontSize = 14
              )
            ),
            transitTypeShort = reactable::colDef(width = 40,
              style = function(value) { 
                if (value == "P") {
                    color <- "#DC143C"
                } else {
                    color <- "#20B2AA"
                }
                list(color = color, fontWeight = "bold", fontSize = 14)
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
            
      output$tableDrivers <- reactable::renderReactable({
        reactable::reactable(
          data = reactiveDfDrivers(),
          searchable = TRUE,
          pagination = FALSE,
          highlight = TRUE,
          compact = TRUE,
          height = 400,
          width = 360,
          selection = "multiple", 
          onClick = "select",
          groupBy = "driverName",
          columns = list(
            driverName = reactable::colDef(
              style = list(fontSize = 14),
              width = 180
            ),
            orderNr = reactable::colDef(
              width = 90,
              style = list(
                fontWeight = 300,
                fontSize = 14
              )
            ),
            distanceKm = reactable::colDef(
              width = 80,
              cell = function(value) {  
                paste0(value, " km")
              },
              style = function(value) {
                if (value >= 1) {
                  color <- "#DC143C"
                } else {
                  color <- "#20B2AA"
                }
                list(color = color, fontWeight = 500, fontSize = 14)
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
        
        popIcons <- leaflet.extras::pulseIcons(color = "#007bff", heartbeat = 0.8)
        
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
              icon = popIcons) %>% 
            leaflet::flyTo(19.145136, 51.919438, zoom = 7)
          
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
              icon = popIcons) %>% 
            leaflet::flyTo(19.145136, 51.919438, zoom = 7)
          
        } else if (lenSelectedDrivers < lenPrevSelectedDrivers) {
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::removeMarker(layerToHideDrivers)
        }                
        prevSelectedTransits(selectedRowTransits())
        prevSelectedDrivers(selectedRowDrivers())        
      }) 
      
 
      
      # create excel report           
      output$downloadExcel <- downloadHandler(
        
        rows <- nrow(data),
        
        filename = function() {
          "All_data_report.xlsx"
        },
        content = function(file) {
          my_workbook <- openxlsx::createWorkbook()
          
          openxlsx::addWorksheet(
            wb = my_workbook,
            sheetName = "Transit_data"
          )
          
          openxlsx::setColWidths(
            my_workbook,
            1,
            cols = 1:16,
            widths = c(12, 12, 15, 15, 12, 12, 12, 15, 15, 15, 17, 15, 15, 15, 12, 10)
          )
          
          openxlsx::writeData(
            my_workbook,
            sheet = 1,
            c(
              "Transits & Drivers report - all data"
            ),
            startRow = 1,
            startCol = 1
          )

          openxlsx::addStyle(
            my_workbook,
            sheet = 1,
            style = openxlsx::createStyle(
              fontSize = 18,
              textDecoration = "bold",
              fontColour = "#2d349c"
            ),
            rows = 1,
            cols = 1:6
          )
          
          openxlsx::writeData(
            my_workbook,
            sheet = 1,
            data[1:rows,1:16],
            startRow = 3,
            startCol = 1
          )
          
          openxlsx::addStyle(
            my_workbook,
            sheet = 1,
            style = openxlsx::createStyle(
              fgFill = "#151a59",
              halign = "center",
              fontColour = "#ffffff"
            ),
            rows = 3,
            cols = 1:16,
            gridExpand = TRUE
          )

          openxlsx::showGridLines(my_workbook, 1, showGridLines = FALSE)
          openxlsx::saveWorkbook(my_workbook, file)
        }
      )
    }
  )}









