library( dplyr    )
library( magrittr )
library( ggplot2 )
library( stringr )
library( leaflet )
library( rgdal )

centre_longitude  = postcode_holder %>% pull( longitude ) %>% mean
centre_latitude   = postcode_holder %>% pull( latitude  ) %>% mean

# https://rstudio.github.io/leaflet/markers.html

# participant_icon <- makeIcon(
#   iconUrl = "img/participant_grey.png",
#   iconWidth = 38, iconHeight = 95,
#   iconAnchorX = 22, iconAnchorY = 94#,
#   #shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
#   #shadowWidth = 50, shadowHeight = 64,
#   #shadowAnchorX = 4, shadowAnchorY = 62
# )


# Create a palette that maps factor levels to colors
#pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

# leaflet(df) %>% addTiles() %>%
#   addCircleMarkers(
#     radius = ~ifelse(type == "ship", 6, 10),
#     color = ~pal(type),
#     stroke = FALSE, fillOpacity = 0.5
#   )
# 
# leaflet(data = quakes[1:4,]) %>% addTiles() %>%
#   addMarkers(~long, ~lat, icon = greenLeafIcon)

postcode_areas = readOGR("/conf/Clinical_Trials_Unit/01 Projects/PROJECT_DSAP2020/6050105/postcode_polygons.gpkg", layer = "postcode_district")
#postcode_areas = readOGR(dsn="Distribution", layer="Areas")
layers <- ogrListLayers("../6050105/postcode_polygons.gpkg")


postcode_areas = spTransform(postcode_areas, CRS("+proj=longlat +datum=WGS84"))

postcode_areas = subset(postcode_areas, grepl("PA", postcodes$pc_district))



postcode_areas = readOGR(dsn="Distribution", layer="Areas")

marker_palette = colorFactor( c("navy","red"),
                              domain = c("Hospital","Participant"))


leaflet(postcode_holder) %>%
  setView( lng=centre_longitude,
           lat=centre_latitude,
           zoom = 10 ) %>%
  addTiles() %>% 
  #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  # addMarkers(~longitude,
  #            ~latitude,
  #            popup = ~as.character(id),
  #            label = ~as.character(postcode))
  addCircleMarkers(
    ~longitude,
    ~latitude,
    radius = ~ifelse(group == "Hospital",20, 5),
    color  = ~marker_palette(group),
    stroke = TRUE,
    fillOpacity = 0.5
  )
