# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()


# moze uzyc well panel dla buttonow w bottom? well panel  powinien byc relatywny do sidebara

# ?bs4Dash::dashboardHeader()
?wellPanel()
?absolutePanel()
?bs4Dash::sidebarMenu()
?mapboxer::mapboxer()
?mapboxer::add_circle_layer()
?mapboxer::add_line_layer()
?mapboxer::mapboxer_proxy()
?mapboxer::update_mapboxer()
?mapboxer::set_layout_property()
?mapboxer::set_filter()
?mapboxer::set_data()
?mapboxer::add_control()
?mapboxer::add_draw_control()


?leaflet::clearShapes()
?leaflet::addProviderTiles()
?leaflet::addCircleMarkers()
?leaflet::addPolylines()
?leaflet.mapboxgl::addMapboxGL()
?leaflet::addCircles()  
?leaflet::leaflet()  
?leaflet::renderLeaflet()  
?leaflet::removeControl()
?leaflet::clearGroup()
?leaflet::addLayersControl()
?leaflet::layersControlOptions()
?leaflet::addProviderTiles()
?leaflet.mapboxgl::addMapboxGL()

?leaflet.extras::addDrawToolbar()
?leaflet.extras::drawPolygonOptions()
?leaflet.extras::drawShapeOptions()
?leaflet.extras::addSearchFeatures()
?leaflet.extras::searchFeaturesOptions()
?leaflet.extras::searchOptions()
?leaflet.extras::addSearchOSM()
?leaflet.extras::addHeatmap()


?shinydashboard::dashboardHeader()
remotes::install_github("SymbolixAU/mapdeck")
remotes::install_github("SymbolixAU/spatialwidget")
remotes::install_github("crazycapivara/mapboxer")
install.packages("mapboxapi")
install.packages("tidyr")
install.packages("scales")
?shinyWidgets::actionBttn()

?bs4Dash::box()
?shinyWidgets::airDatepickerInput()
?mapboxer::add_tooltips()

?dateRangeInput()
?bs4Dash::menuItem()
?bs4Dash::tabItem()
?bs4Dash::dashboardPage()
?bs4Dash::dashboardSidebar()
?bs4Dash::bs4TableItem()
?bs4Dash::box()
?bs4Dash::renderbs4InfoBox()
?bs4Dash::dashboardControlbar()
?bs4Dash::controlbarItem()
?bs4Dash::descriptionBlock()
?bs4Dash::infoBox()

?reactable::reactableTheme()
?reactable::reactable()
?reactable::colDef()

?data.frame()
sessionInfo()

?shinyjs::show()
install.packages("reactable")
install.packages("echarts4r")
devtools::install_github("rstudio/crosstalk")

?echarts4r::e_theme()
?echarts4r::e_tooltip()
?echarts4r::e_show_loading()
?echarts4r::e_format_y_axis()
?echarts4r::e_y_axis()
?echarts4r::e_color()
?echarts4r::e_text_style()
?echarts4r::e_title()
?echarts4r::e_visual_map()
?echarts4r::e_charts()
?echarts4r::e_grid()
?echarts4r::e_bar()
?echarts4r::e_x_axis()
?echarts4r::e_axis()
?echarts4r::e_pie()


?bs4Dash::box()


# result <- aggregate(depth ~ stations, data = quakes, c)
# result
# 
# result2 <- aggregate(depth ~ stations, data = quakes, paste, collapse = ",")
# result
# 
# result3 <- aggregate(depth ~ stations, data = quakes, toString)
# result3






