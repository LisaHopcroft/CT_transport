library( tidyverse    )
library( PostcodesioR )
library( leaflet )
library( rgdal )

set.seed( "5482" )

number_of_participants = 20

list_of_outcodes = c( sprintf( "PA%d", c( 1:6, 10:11 ) ),
                      sprintf( "G%d" , c( 52, 53, 60, 77, 78, 81, 82) )
)

hospital_postcode = "PA29PN"
hospital_postcode_information = postcode_lookup( hospital_postcode )

# postcode_holder = tibble()
# 
# postcode_columns = c(
#   "outcode"          , "postcode"  ,
#   "longitude"        , "latitude"  ,
#   "zone_intermediate", "zone_lower",
#   "admin_authority"   
# )
# 
# postcode_holder = postcode_columns %>%
#   purrr::map_dfc(setNames, object = list(character())) %>% 
#   mutate( id = integer()) %>%
#   mutate( longitude = double() ) %>% 
#   mutate( latitude = double() ) %>% 
#   select( id, everything() )

postcode_holder = tibble(
  id = NA,
  outcode  = hospital_postcode_information$outcode,
  postcode = hospital_postcode_information$postcode,
  longitude = hospital_postcode_information$longitude,
  latitude  = hospital_postcode_information$latitude,
  zone_intermediate = hospital_postcode_information$msoa,
  zone_lower        = hospital_postcode_information$lsoa,
  admin_authority   = hospital_postcode_information$admin_district
)

for ( i in 1:number_of_participants ) {
  
  this.outcode = sample( list_of_outcodes, 1 )
  
  this.postcode_information = random_postcode( this.outcode )
  
  if ( i %% 25 == 0 ) {
    cat( sprintf( "%04d Random postcodes generated\n",
                  i ) )
  }
  
  postcode_holder = postcode_holder %>% 
    add_row( id = i,
             outcode  = this.outcode,
             postcode = this.postcode_information$postcode,
             longitude = this.postcode_information$longitude,
             latitude  = this.postcode_information$latitude,
             zone_intermediate = this.postcode_information$msoa,
             zone_lower        = this.postcode_information$lsoa,
             admin_authority   = this.postcode_information$admin_district )
  
  
}

postcode_holder = postcode_holder %>%
  arrange( id ) %>% 
  mutate( group = ifelse( is.na(id), "Hospital", "Participant") )

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


postcode_holder %>% 
  filter( group == "Participant" ) %>% 
  write_csv( "dat/participants.csv" )


postcode_holder %>% 
  filter( group == "Hospital" ) %>% 
  write_csv( "dat/hospital.csv" )



