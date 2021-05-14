#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(

    golem_add_external_resources(),
    waiter::use_waiter(),
    waiter::use_steward(colors = c("#58088a","#ab60d6", "#0b2d52","#87b8ed")),
    waiter::waiter_preloader(html = tagList(waiter::spin_flower(), HTML(paste(tags$span(style="font-size:17px;letter-spacing: 1.5px","Loading"), tags$span(style="font-size:17px;font-weight:600", "geoMapX..."), sep = " ")))),
    
    bs4Dash::dashboardPage(
      
      fullscreen = TRUE,
      
       mod_header_ui("header_ui"),
       mod_sidebar_ui("sidebar_ui"),
       mod_body_ui("body_ui")

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

