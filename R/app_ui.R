#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(

    golem_add_external_resources(),

    
    bs4Dash::dashboardPage(
      
      
      # preloader = list(
      #   waiter = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#343a40"),
      #   duration = 5
      # ),

        
       mod_header_ui("header_ui"),
        
       mod_sidebar_ui("sidebar_ui"),
    
       mod_body_ui("body_ui")
       
       
       #mod_dashboard_ui("dashboard_ui")
  

  )
  )

}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GeoMapX'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

