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
      #htmlOutput(ns("textStats")),
      echarts4r::echarts4rOutput(ns("plot"), height = "25%"),
      reactable::reactableOutput(ns("tableStats"))#,
      #verbatimTextOutput(ns("text"))
    ),


    shinyjs::hidden(absolutePanel(id = ns("analysisPanel"),
                                  tagList(shinyWidgets::actionBttn(inputId = ns("btnAnl1"), label = NULL, style = "material-circle", size = "sm", icon = icon("vector-square")),
                                          shinyWidgets::actionBttn(inputId = ns("btnAnl2"), label = NULL, style = "material-circle", size = "sm", icon = icon("map")),
                                          shinyWidgets::actionBttn(inputId = ns("btnAnl3"), label = NULL, style = "material-circle", size = "sm", icon = icon("fire"))))),
                                          #shinyWidgets::actionBttn(inputId = ns("btnAnl4"), label = NULL, style = "material-circle", size = "sm", icon = icon("dot-circle"))))), # pokazac tu  okregi np. 1,2,3 km od punktu odbiorcy, zeby mozna bylo recznie ustawiac radius i wtedy zrobic podsumowanie ile statusuw zostalo nadanych w danym zakresie radiusa               


  )
}



#' @noRd 
mod_mapAnalysis_server <- function(id, data, passedMap, passedMapInput, passedMapInputShape, parentSession){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      
      
      #districts <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-medium.geojson")
      # poviats <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/powiaty/powiaty-medium.geojson")  
  
      visibilityMapRectangles <- reactiveValues(showHide = FALSE)
      visibilityMapDistrict <- reactiveValues(showHide = FALSE)
      visibilityHeatMap <- reactiveValues(showHide = FALSE)
      buttonNumber <- reactiveValues(number = NULL)
      
      # draw polygon coords
      polygon_coords <- reactiveVal()
      choro_coords <- reactiveVal()
      
    
      dataStats <- colTable(data, c(1:3,8:10,12,14))
      #dataTextStats <- reactive({countOrders(dataStats[commonPoints(),], "LP")})
      #dataTableStats <- reactive({tableStats(dataStats[commonPoints(),], "Kierowca","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km")})
      #dataChartStats <- reactive({chartStats(dataStats[commonPoints(),], "statusHrsMin","ZLP", "countAllOrders")})
      #labelChoroStats <- reactive({labelStats(dataStats[commonPointsChoro(),], "ZLP", "LP")})

      # observeEvent(input$controlbarToggle, {
      #   bs4Dash::updateControlbar(id = "controlbar")
      # })      
      # 
      
      observe ({
        
        polygon_coords(passedMapInput())
        choro_coords(passedMapInputShape())
        
      })

      
      observe(
        #if (is.null(polygon_coords())) {
        if (!is.null(polygon_coords()) | !is.null(choro_coords())) {
          shinyjs::runjs(
            htmltools::HTML('document.querySelector("#controlbar-toggle").click();'))          
        }
      )      
      
      
###################################################

      # observe({
      #   polygon_coords(isolate(passedMapInput())) # isolate powoduje, ze zaznaczajac obszar z poziomu boxTable controlbar nie wysuwa sie!
      #   choro_coords(passedMapInputShape())  
      # })
      
######################################################
      

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
      
      
      
      
      # SELECT AREA

      # main data for a table and chart
      dataTableStats <- reactive({

        df =  switch (buttonNumber$number,
            '1' = tableStats(dataStats[commonPoints(),], "driverName","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km"),
            '2' = tableStats(dataStats[commonPointsChoro(),], "driverName","countAllOrders","statusGreater30min","statusSerialSet", "statusGreater1km")
          )

      })
      

      dataChartStats <- reactive({
        
        df =  switch (buttonNumber$number,
             '1' = chartStats(dataStats[commonPoints(),], "statusHrsMin","transitNr", "countAllOrders"), # tu bylo ZLP i nizej tez
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
          #echarts4r::e_text_style(color = "red") %>% 
          #echarts4r::e_grid(height = "35%") %>% 
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
            #Kierowca = reactable::colDef(
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
      
    

  

      observeEvent(input$btnAnl1, {

        buttonNumber$number = 1
        #print(polygon_coords())
        #polygon_coords(NULL)
        #print(buttonClickedFromBoxTable())
        visibilityMapDistrict$showHide = FALSE
        visibilityHeatMap$showHide = FALSE

        leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
          leaflet::clearGroup("districts") %>%
          leaflet::clearGroup("heat")

        if (visibilityMapRectangles$showHide) {
          #print(paste0('IF ',polygon_coords()))
          #print(polygon_coords() == "Empty")
          #polygon_coords("Empty")
          #polygon_coords(NULL)
          visibilityMapRectangles$showHide = !visibilityMapRectangles$showHide
          #emptyPolygon$empty = FALSE

          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(0);
                             cancel.firstElementChild.click();

                             document.querySelector(".leaflet-draw-edit-remove").click();
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))

        } else {

          visibilityMapRectangles$showHide = !visibilityMapRectangles$showHide
          #polygon_coords(NULL)
          #print(paste0('ELSE ',polygon_coords()))
          #emptyPolygon$empty = !emptyPolygon$empty




          #polygon_coords(NULL)
          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(0);
                             cancel.firstElementChild.click();

                             document.querySelector(".leaflet-draw-edit-remove").click();
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))


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
            leaflet::clearGroup("districts")
          
          
          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(0); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))               
          
          
        } else {
          
          visibilityMapDistrict$showHide = !visibilityMapDistrict$showHide  
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("districts") %>% 
            leaflet::clearGroup("heat") %>% 
            leaflet::addPolygons(
              data = districts,
              group = "districts",
              layerId = ~id,
              #fillColor = "green",
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
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(0); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))           
         
          }
      }) 
      
      
      
      
      observeEvent(input$btnAnl3, {
        
        buttonNumber$number = 3
        visibilityMapRectangles$showHide = FALSE
        visibilityMapDistrict$showHide = FALSE
        
        if (visibilityHeatMap$showHide) {
          
          visibilityHeatMap$showHide = !visibilityHeatMap$showHide
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("heat")
         
        } else {
          
          visibilityHeatMap$showHide = !visibilityHeatMap$showHide
          
          leaflet::leafletProxy(mapId = passedMap, session = parentSession) %>%
            leaflet::clearGroup("heat") %>% 
            leaflet::clearGroup("districts") %>% 
            leaflet.extras::addHeatmap(
              data = dataStats,
              group = "heat",
              intensity = ~orderNr,
              #intensity = ~LP,
              blur = 35,
              radius = 27
              
            )
          
          shinyjs::runjs(
            htmltools::HTML('var elementsCancel = document.getElementsByClassName("leaflet-draw-actions")[0].children;
                             var cancel = elementsCancel.item(0); 
                             cancel.firstElementChild.click();  
            
                             document.querySelector(".leaflet-draw-edit-remove").click(); 
                             var elementsClearAll = document.getElementsByClassName("leaflet-draw-actions")[1].children;
                             var clearAll = elementsClearAll.item(2);
                             clearAll.firstElementChild.click();' ))   
                
        }   
      })
      
   
      
      
      # buttons return to mod_boxTable (as to properly filter box table, unfortunately it's not working from this module properly)
      return( list(
        btnDrawRectangle = reactive({input$btnAnl1}),
        btnDrawChoro = reactive({input$btnAnl2})
      ))
      
         
      
}
    
  )
}
 
