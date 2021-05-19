#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  bs4Dash::tabItems(
    bs4Dash::tabItem(
      tabName = "dashboard",
      fluidRow(
        column(
          width = 4,
          bs4Dash::box(
            id = "performanceDivisions",
            closable = FALSE,
            collapsible = FALSE,
            title = tags$span(style="font-weight:300","Divisions performance - statues set vs. all orders"),
            background = NULL,
            width = 12,
            status = "success",
            footer = fluidRow(
              column(
                width = 6,
                bs4Dash::descriptionBlock(
                  number = textOutput(ns("bestDivisionRatio")),
                  numberColor = "success",
                  numberIcon = icon("circle"),
                  header = textOutput(ns("bestDivisionNumbers")),
                  text = textOutput(ns("bestDivisionName")),
                  rightBorder = TRUE,
                  marginBottom = FALSE
                )
            ),           
              column(
                width = 6,
                bs4Dash::descriptionBlock(
                  number = textOutput(ns("worstDivisionRatio")),
                  numberColor = "danger",
                  numberIcon = icon("circle"),
                  header = textOutput(ns("worstDivisionNumbers")),
                  text = textOutput(ns("worstDivisionName")),
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              )
            )
          ),         
          bs4Dash::infoBox(
            tabName = "divisionKmShortest",
            title = tags$span(style="color:#337fd4","Division with the shortest distance from customer to app coordinates"),
            value = htmlOutput(ns("bestDivisionKm")),
            width = 12,
            color = "success",
            icon = icon("route")
          ),          
          bs4Dash::infoBox(
            tabName = "divisionKmLongest",
            title = tags$span(style="color:#337fd4","Division with the longest distance from customer to app coordinates"),
            value = htmlOutput(ns("worstDivisionKm")),
            width = 12,
            color = "danger",
            icon = icon("route")
          )
        ),        
        column(
          width = 4, 
          bs4Dash::box(
            title = tags$span(style="font-weight:300","Number of statuses set per division"),
            id = "chart2",
            status = "maroon",
            width = 12,
            closable = FALSE,
            collapsible = FALSE,
            echarts4r::echarts4rOutput(ns("plot2"), height = "100%")
          )
        ),        
        column(
          width = 4, 
          bs4Dash::box(
            title = tags$span(style="font-weight:300","Number of orders in 30 min intervals"),
            id = "chart3",
            width = 12,
            closable = FALSE,
            collapsible = FALSE,
            status = "primary", 
            echarts4r::echarts4rOutput(ns("plot3"), height = "100%")
          )
        )        
      ),     
      fluidRow(
        column(
          width = 4,
          bs4Dash::box(
            closable = FALSE,
            collapsible = FALSE,
            title = tags$span(style="font-weight:300","Drivers performance - statues set vs. all orders"),
            background = NULL,
            width = 12,
            status = "success",
            footer = fluidRow(
              column(
                width = 6,
                bs4Dash::descriptionBlock(
                  number = textOutput(ns("bestDriverRatio")),
                  numberColor = "success",
                  numberIcon = icon("circle"),
                  header = textOutput(ns("bestDriverNumbers")),
                  text = textOutput(ns("bestDriverName")),
                  rightBorder = TRUE,
                  marginBottom = FALSE
                )
              ),             
              column(
                width = 6,
                bs4Dash::descriptionBlock(
                  number = textOutput(ns("worstDriverRatio")),
                  numberColor = "danger",
                  numberIcon = icon("circle"),
                  header = textOutput(ns("worstDriverNumbers")),
                  text = textOutput(ns("worstDriverName")),
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              )
            )
          ),          
          bs4Dash::infoBox(
            tabName = "driverKmShortest",
            title = tags$span(style="color:#337fd4","Driver with the shortest distance from customer to app coordinates"),
            value = htmlOutput(ns("bestDriverKm")),
            width = 12,
            color = "success",
            icon = icon("route")
          ),          
          bs4Dash::infoBox(
            tabName = "driverKmLongest",
            title = tags$span(style="color:#337fd4","Driver with the longest distance from customer to app coordinates"),
            value = htmlOutput(ns("worstDriverKm")),
            width = 12,
            color = "danger",
            icon = icon("route")
          )
        ),       
        column(
          width = 8, 
          reactable::reactableOutput(ns("tableAllData"))         
        )
      )          
    )
    )
  )
}
    

#' @noRd 
mod_dashboard_server <- function(id, data)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      # datasets for use in various statistics
      dataStats <- colTable(data, c(1:15))
      
      dataPlot1 <- chartStats(dataStats, "DivisionCity","orderNr", "countAllOrders")      
      dataPlot2 <- chartStats(dataStats, "driverName","statusGreater30min", "statusSerialSet")
      dataPlot3 <- chartStats(dataStats, "statusHrsMin","transitNr", "countAllOrders")

      dataTableAllData <- dataStats %>% 
        dplyr::select("DivisionCity", "driverName", "transitNr","orderNr","transitTypeShort","customerName","Status", 
                      "statusDateTime", "distanceKm", "statusGreater30min", "statusGreater1km", "statusSerialSet")
      
      bestDriver <- chartStatsBestDriver(dataStats, "driverName", "countAllOrders" )
      bestDivision <- chartStatsBestDivision(dataStats, "DivisionCity", "driverName", "countAllOrders" )
      
      worstDriver <- chartStatsWorstDriver(dataStats, "driverName", "countAllOrders" )
      worstDivision <- chartStatsWorstDivision(dataStats, "DivisionCity", "driverName", "countAllOrders" )
      
      bestDriverKm <- statsKmBest(dataStats, "driverName", "distanceKm")
      bestDivisionKm <- statsKmBest(dataStats, "DivisionCity", "distanceKm" )
      
      worstDriverKm <- statsKmWorst(dataStats, "driverName", "distanceKm" )
      worstDivisionKm <- statsKmWorst(dataStats, "DivisionCity", "distanceKm" )      
      
      # outputs - panels with statistics
      output$bestDivisionRatio <- renderText(
        bestDivision$ratio
      )
      
      output$bestDivisionNumbers <- renderText(
        paste(bestDivision$Orders,"/",bestDivision$sumAllOrders)
      )
      
      output$bestDivisionName <- renderText(
        bestDivision$DivisionCity
      )
      
      output$worstDivisionRatio <- renderText(
        worstDivision$ratio
      )
      
      output$worstDivisionNumbers <- renderText(
        paste(worstDivision$Orders,"/",worstDivision$sumAllOrders)
      )
      
      output$worstDivisionName <- renderText(
        worstDivision$DivisionCity
      )      
           
      output$bestDriverRatio <- renderText(
        bestDriver$ratio
      )
      
      output$bestDriverNumbers <- renderText(
        paste(bestDriver$Orders,"/",bestDriver$sumAllOrders)
      )
      
      output$bestDriverName <- renderText(
        bestDriver$driverName
      )
            
      output$worstDriverRatio <- renderText(
        worstDriver$ratio
      )
      
      output$worstDriverNumbers <- renderText(
        paste(worstDriver$Orders,"/",worstDriver$sumAllOrders)
      )
      
      output$worstDriverName <- renderText(
        worstDriver$driverName
      )        
       
      output$bestDriverKm <- renderText(
        paste("<font color=\"#28a745\"><b>", bestDriverKm$distanceKm," km   ","</b></font>",bestDriverKm$driverName)
      )
      
      output$worstDriverKm <- renderText(
        paste("<font color=\"#dc3545\"><b>", worstDriverKm$distanceKm," km   ","</b></font>",worstDriverKm$driverName)
      )
  
      output$bestDivisionKm <- renderText(
        paste("<font color=\"#28a745\"><b>", bestDivisionKm$distanceKm," km   ","</b></font>",bestDivisionKm$DivisionCity)
      )
      
      output$worstDivisionKm <- renderText(
        paste("<font color=\"#dc3545\"><b>", worstDivisionKm$distanceKm, " km ", "</b></font>",worstDivisionKm$DivisionCity)
      )
         
      output$plot2 <- echarts4r::renderEcharts4r({        
        dataPlot1 %>% 
          echarts4r::e_charts(DivisionCity) %>% 
          echarts4r::e_pie(Orders, radius = c("45%", "80%")) %>% 
          echarts4r::e_legend(show = FALSE) %>% 
          echarts4r::e_show_loading(hide_overlay = FALSE, mask_color = '#343a40') %>% 
          echarts4r::e_color(background = "#343a40") %>% 
          echarts4r::e_theme("dark-mushroom") %>% 
          echarts4r::e_tooltip(
            trigger = "item",
            formatter = echarts4r::e_tooltip_pie_formatter(
              style = c("percent")
            )
          )        
      })      
      
      output$plot3 <- echarts4r::renderEcharts4r({       
        dataPlot3 %>% 
          echarts4r::e_charts(statusHrsMin) %>% 
          echarts4r::e_area(Orders) %>% 
          echarts4r::e_show_loading(hide_overlay = FALSE, mask_color = '#343a40') %>% 
          echarts4r::e_color("#2653ad") %>% 
          echarts4r::e_legend(show = FALSE) %>% 
          echarts4r::e_grid(height = "60%") %>% 
          echarts4r::e_tooltip(
            trigger = "axis"
          )       
      })      
  
      
      output$plot4 <- echarts4r::renderEcharts4r({       
        dataPlot4 %>% 
          echarts4r::e_charts(statusHrsMin) %>% 
          echarts4r::e_area(Orders) %>% 
          echarts4r::e_show_loading(hide_overlay = FALSE, mask_color = '#343a40') %>% 
          echarts4r::e_color("#2653ad") %>% 
          echarts4r::e_legend(show = FALSE) %>% 
          echarts4r::e_grid(height = "60%") %>% 
          echarts4r::e_tooltip(
            trigger = "axis"
          )        
      })      
           
      output$tableAllData <- reactable::renderReactable({
        reactable::reactable(
          data = dataTableAllData,
          searchable = TRUE,
          pagination = TRUE,
          highlight = TRUE,
          compact = TRUE,
          columns = list(
            DivisionCity = reactable::colDef(name = "Division", align = "center", width = 70),
            driverName = reactable::colDef(name = "Driver", align = "center", width = 170),
            transitNr = reactable::colDef(name = "Transit no.", align = "center", width = 100),
            orderNr = reactable::colDef(name = "Order no.", align = "center", width = 100),
            transitTypeShort = reactable::colDef(name = "Type", align = "center", width = 70,
                                                 style = function(value) {
                                                   if (value == "D") {
                                                     color <- "#20B2AA"
                                                   } else {
                                                     color <- "#DC143C"
                                                   }
                                                   list(color = color, fontWeight = "bold")
                                                   }),
            customerName = reactable::colDef(name = "Customer", align = "center",width = 170),
            Status = reactable::colDef(name = "Shpt. status", align = "center", width = 100),
            statusDateTime = reactable::colDef(name = "Event time", align = "center", defaultSortOrder = "desc",
                                               format = reactable::colFormat(datetime = TRUE)),
            distanceKm = reactable::colDef(name = "Dist. (km)", align = "center", width = 70),
            statusGreater30min = reactable::colDef(name = "> 30 min.",align = "center", width = 70),
            statusGreater1km = reactable::colDef(name = "> 1 km",align = "center", width = 70),
            statusSerialSet = reactable::colDef(name = "Serial set",align = "center", width = 70),
            geometry = reactable::colDef(show = FALSE)
          ),          
          theme = reactable::reactableTheme(
            color = "#f2f2f2",
            backgroundColor = "#292828",
            highlightColor = "#383838",
            borderColor = "#383838",
            borderWidth = "1px",
            searchInputStyle = list(width = "15%",backgroundColor = "#292828", border = "1px solid #383838"),
            headerStyle = list(
              letterSpacing = "1px",
              color = "#b3b3b3",
              fontWeight = 400,
              textTransform = "uppercase"
            ),
            paginationStyle = list(color = "#b3b3b3"),
            style = list(
              fontSize = "12px"
              )
          )         
        )
      })           
    }
)

 
