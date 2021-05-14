# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "DBI")
usethis::use_package( "odbc")
usethis::use_package( "pool")
usethis::use_package( "dbplyr")
usethis::use_package( "sf")
usethis::use_package( "leaflet")
usethis::use_package( "dplyr")
usethis::use_package( "purrr")
usethis::use_package( "sfheaders")
usethis::use_package( "mapboxer")
usethis::use_package( "leaflet.mapboxgl")
usethis::use_package( "shinydashboard")
usethis::use_package( "shinydashboardPlus")
usethis::use_package( "dashboardthemes")
usethis::use_package( "rlang")
usethis::use_package( "shinyjs")
usethis::use_package( "bs4Dash")
usethis::use_package( "thematic")
usethis::use_package( "htmltools")
usethis::use_package( "htmlwidgets")
usethis::use_package( "shinyWidgets")
usethis::use_package( "mapboxapi")
usethis::use_package( "leaflet.extras")
usethis::use_package( "reactable")
usethis::use_package( "echarts4r")
usethis::use_package( "tidyr")
usethis::use_package( "scales")
usethis::use_package( "anytime")
usethis::use_package( "rsconnect")
usethis::use_package( "curl")
usethis::use_package( "openxlsx")
usethis::use_package( "waiter")
usethis::use_package( "stringr")
usethis::use_pipe()

attachment::att_amend_desc()


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "header" ) # Name of the module
golem::add_module( name = "sidebar" ) # Name of the module
golem::add_module( name = "body" ) # Name of the module
golem::add_module( name = "mapAnalysis" ) # Name of the module
golem::add_module( name = "boxTable" ) # Name of the module
golem::add_module( name = "optionsPanel" ) # Name of the module
golem::add_module( name = "dashboard" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "dbConnection" ) 
golem::add_fct( "mapTokens" ) 
golem::add_fct( "tableFct" ) 
golem::add_fct( "analysisFct" ) 
golem::add_fct( "chartFct" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("GeoMapX")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()


#shiny::addResourcePath(prefix = 'img', system.file('app/img', package = 'geomapx'))

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

