
# MAPBOX token ------------------------------------------------------------

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1IjoiaHdsIiwiYSI6ImNramJxY2YxcDV2YXoyeW40YXlvbmUyazQifQ.7HBEvMyrAnVpkKO7MNH7ww")
options(mapbox.accessToken= "pk.eyJ1IjoiaHdsIiwiYSI6ImNramJxY2YxcDV2YXoyeW40YXlvbmUyazQifQ.7HBEvMyrAnVpkKO7MNH7ww")

districts <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-medium.geojson")

# cccc <- read.csv("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp2.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
# cccc
# 
# df2 <- data.frame(read.csv("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp6.csv", header = FALSE, sep = ";"))
# 
# df <- read.table("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp6.csv"
#                   )
# 
# 
# #df <- readLines("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp6.csv")
# 
# read.csv("https://raw.githubusercontent.com/Camil88/geoMapX/main/dataApp6.csv", header = FALSE) -> mydata
