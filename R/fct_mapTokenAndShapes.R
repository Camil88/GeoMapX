
# MAPBOX token and other resources ------------------------------------------------------------

options(mapbox.accessToken = "pk.eyJ1IjoiaHdsIiwiYSI6ImNramJxY2YxcDV2YXoyeW40YXlvbmUyazQifQ.7HBEvMyrAnVpkKO7MNH7ww")

districts <- sf::st_read(dsn = "https://raw.githubusercontent.com/ppatrzyk/polska-geojson/master/wojewodztwa/wojewodztwa-medium.geojson")

rsconnect::setAccountInfo(name='kghwl', token='BC3B81C5BF29A03FBDAB6698312706C2', secret='wf6w4piUPN1hCE1D3cKNPHxnEOhLTcLFlqBXXxt2')
