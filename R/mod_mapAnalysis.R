#' mapAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mapAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    controlbar = bs4Dash::dashboardControlbar(
      id = ns("controlbar"),
      skin = "dark",
      width = 450,
      pinned = FALSE,
      overlay = TRUE,
      collapsed = TRUE,
      echarts4r::echarts4rOutput(ns("plot"), height = "25%"),
      reactable::reactableOutput(ns("tableStats"))
    ),

    shinyjs::hidden(absolutePanel(id = ns("analysisPanel"),
                                  tagList(shinyWidgets::actionBttn(inputId = ns("btnAnl1"), label = NULL, style = "material-circle", size = "sm", icon = icon("vector-square")),
                                          shinyWidgets::actionBttn(inputId = ns("btnAnl2"), label = NULL, style = "material-circle", size = "sm", icon = icon("map")),
                                          shinyWidgets::actionBttn(inputId = ns("btnAnl3"), label = NULL, style = "material-circle", size = "sm", icon = icon("fire")))))

  )
}



#' @noRd 
mod_mapAnalysis_server <- function(id, data, passedMap, passedMapInput, passedMapInputShape, parentSession) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
        
      visibilityMapRectangles <- reactiveValues(showHide = FALSE)
      visibilityMapDistrict <- reactiveValues(showHide = FALSE)
      visibilityHeatMap <- reactiveValues(showHide = FALSE)
      buttonNumber <- reactiveValues(number = NULL)
      
      # draw polygon coords
      polygon_coords <- reactiveVal()
      choro_coords <- reactiveVal()
      
      dataStats <- colTable(data, c(1:3,8:10,12,14))

      observe ({
        choro_coords(passedMapInputShape())
        polygon_coords(passedMapInput())
        
      })

      observe(
        if (!is.null(polygon_coords()) | !is.null(choro_coords())) {
          shinyjs::runjs(
            htmltools::HTML('document.querySelector("#controlbar-toggle").click();'))
        }
      )
      

      commonPointsChoro <- reactive({

        if (is.null(choro_coords())) return()

        df <- districts %>%
          dplyr::filter(id == choro_coords()$id)

        returnCommonPoints <- dataStats[df,]
        returnCommonPoints
      })
       

      commonPoints <- reactive({        
        rbind_coords <- do.call(rbind,lapply(polygon_coords(),function(x){c(x[[1]][1],x[[2]][1])}))
        polygon_object <- sf::st_polygon(list(rbind_coords))
        returnCommonPoints <- dataStats[polygon_object,]
        returnCommonPoints
        
      })  
      
      
    
      # SELECTING AREA ON A MAP

      # main data for a table and chart
      dataTableStats <- reactive({

        df =  switch (buttonNumber$number,
            '1' = tableStats(dataStats[commonPoints(),], "driverName","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km"),
            '2' = tableStats(dataStats[commonPointsChoro(),], "driverName","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km")
          )
      })
      
      dataChartStats <- reactive({
        
        df =  switch (buttonNumber$number,
             '1' = chartStats(dataStats[commonPoints(),], "statusHrsMin","transitNr", "countAllOrders"), 
             '2' = chartStats(dataStats[commonPointsChoro(),], "driverName","transitNr", "countAllOrders")
        )
      })


      output$plot <- echarts4r::renderEcharts4r({
        
        chart_xAxis <- chartXAxisStats(buttonNumber$number)
        chart_type <- chartTypeStats(buttonNumber$number)
        chart_title <- chartTitleStats(buttonNumber$number)
        chart_color <- chartColorStats(buttonNumber$number)
        chart_label <- chartLabelShowStats(buttonNumber$number)
        
        dataChartStats() %>% 
          echarts4r::e_charts_(chart_xAxis) %>% 
          chart_type(Orders) %>% 
          echarts4r::e_title(chart_title,
                             textStyle = list(
                               color = chart_color,
                               fontWeight = 600,
                               fontSize = 14),
                             right = 108,
                             top = 5) %>%
          echarts4r::e_legend(show = FALSE) %>% 
          echarts4r::e_show_loading(hide_overlay = FALSE, mask_color = '#343a40') %>% 
          echarts4r::e_grid(bottom = 35) %>% 
          echarts4r::e_color(chart_color) %>% 
          echarts4r::e_x_axis(axisLabel = list(show = chart_label)) %>% 
          echarts4r::e_tooltip(
            trigger = "axis"
            )        
      })

      
      output$tableStats <- reactable::renderReactable ({       
        reactable::reactable(
          data = dataTableStats(),
          searchable = FALSE,
          pagination = FALSE,
          highlight = FALSE,
          compact = TRUE,
          borderless = TRUE,
          height = 240,
          columns = list(
            driverName = reactable::colDef(
              name = "Driver",
              style = list(fontSize = 13, color = '#c2c7d0'),
              width = 160,
              align = "center"
            ),
            SetVSAll = reactable::colDef(
              name = "Set vs. all",
              style = list(fontSize = 13, color = '#c2c7d0'),
              width = 60,
              align = "center"
            ),
            Distance1km = reactable::colDef(
              name = "> 1 km dist.",
              style = list(fontSize = 13, color = '#c2c7d0'),
              width = 60,
              align = "center"
            ),            
            Greater30min = reactable::colDef(
              name = "> 30 min.",
              style = list(fontSize = 13, color = '#c2c7d0'),
              width = 60,
              align = "center"
            ),
            SerialSets = reactable::colDef(
              name = "Serial sets",
              style = list(fontSize = 13, color = '#c2c7d0'),
              width = 60,
              align = "center"
            ),
            countAllOrders = reactable::colDef(show = FALSE),
            countOrders = reactable::colDef(show = FALSE)
          ),         
          theme = reactable::reactableTheme(
            backgroundColor = "#343a40",
            headerStyle = list(fontSize = 14, color = '#0089d9'),
            borderColor = "#0089d9",
            borderWidth = 1           
          )
        )
      })      
      
    
      
      clearDrawJS <- 'var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                      var cancel = elementsCancel.item(0);
                      cancel.firstElementChild.click();

                      document.querySelector(".leaflet-draw-edit-remove").click();
                      var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                      var clearAll = elementsClearAll.item(2);
                      clearAll.firstElementChild.click();'
      
      

      observeEvent(input$btnAnl1, {

        buttonNumber$number = 1

        visibilityMapDistrict$showHide = FALSE
        visibilityHeatMap$showHide = FALSE

        leaf_map <- leaflet::leafletProxy(mapId = passedMap, session = parentSession)
        
        leaf_map %>%  
          leaflet::clearGroup("districts") %>%
          leaflet::clearGroup("heat")           

        if (visibilityMapRectangles$showHide) {

          visibilityMapRectangles$showHide = !visibilityMapRectangles$showHide

          leaf_map %>% leaflet::flyTo(19.145136, 51.919438, zoom = 7)
          
          shinyjs::runjs(
            htmltools::HTML(clearDrawJS))

        } else {

          visibilityMapRectangles$showHide = !visibilityMapRectangles$showHide

          shinyjs::runjs(
            htmltools::HTML(clearDrawJS))

          shinyjs::runjs(
            htmltools::HTML('document.querySelector(".leaflet-draw-draw-rectangle").click();'))
        }
      })
      
        
      observeEvent(input$btnAnl2, {
        
        buttonNumber$number = 2

        visibilityMapRectangles$showHide = FALSE
        visibilityHeatMap$showHide = FALSE

        if(visibilityMapDistrict$showHide) {
          
          visibilityMapDistrict$showHide = !visibilityMapDistrict$showHide
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("districts") %>% 
            leaflet::flyTo(19.145136, 51.919438, zoom = 7)
                    
          shinyjs::runjs(
            htmltools::HTML(clearDrawJS))               
                    
        } else {
          
          visibilityMapDistrict$showHide = !visibilityMapDistrict$showHide  
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("districts") %>% 
            leaflet::clearGroup("heat") %>% 
            leaflet::addPolygons(
              data = districts,
              group = "districts",
              layerId = ~id,
              weight = 2,
              opacity = 0.3,
              color = "#6b747d",
              fillOpacity = 0.2,
              highlight = leaflet::highlightOptions(
                weight = 2,
                color = "#9b22bf",
                opacity = 0.8,
                fillOpacity = 0.5,
                bringToFront = TRUE)
            )
          
          shinyjs::runjs(
            htmltools::HTML(clearDrawJS))           
         
          }
      }) 
      
          
      observeEvent(input$btnAnl3, {
        
        buttonNumber$number = 3
        visibilityMapRectangles$showHide = FALSE
        visibilityMapDistrict$showHide = FALSE
        
        if (visibilityHeatMap$showHide) {
          
          visibilityHeatMap$showHide = !visibilityHeatMap$showHide
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("heat") %>% 
            leaflet::flyTo(19.145136, 51.919438, zoom = 7)
         
        } else {
          
          visibilityHeatMap$showHide = !visibilityHeatMap$showHide
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("heat") %>% 
            leaflet::clearGroup("districts") %>% 
            leaflet.extras::addHeatmap(
              data = dataStats,
              group = "heat",
              intensity = ~orderNr,
              blur = 35,
              radius = 27             
            )
          
          shinyjs::runjs(
            htmltools::HTML(clearDrawJS)                  
        }   
      })
      
              
      # buttons returned to mod_boxTable
      return( list(
        btnDrawRectangle = reactive({input$btnAnl1}),
        btnDrawChoro = reactive({input$btnAnl2}),
        btnDrawHeat = reactive({input$btnAnl3})
      ))
           
    }
    
  )
}
 
