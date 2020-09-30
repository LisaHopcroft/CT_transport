library( dplyr    )

#library( data.table )
library( rgdal ) 
#library( htmltools )
#library( htmlwidgets )

library( magrittr )
library( ggplot2 )
library( stringr )
library( leaflet )
library( purrr )

load( "dat/20_random-postcodes.Rdat")

postcode_holder = postcode_holder %>% 
  mutate( Postcode = postcode )

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

### POSTCODE areas


marker_palette = colorFactor( c("navy","red"),
                              domain = c("Hospital","Participant"))

leaflet(postcode_holder) %>%
  setView( lng=centre_longitude,
           lat=centre_latitude,
           zoom = 10 ) %>%
  #addTiles() %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
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


##### ADDING POSTCODE AREAS
##### 

# POSTCODE_UNITS.in <- readOGR( dsn = "NRS_shapefiles/spd-unit-boundaries-cut-19-1", layer = "PC_Cut_19_1" )
# POSTCODE_UNITS = spTransform( POSTCODE_UNITS.in, CRS( "+proj=longlat +datum=WGS84" ) )
# POSTCODE_UNITS.outcodes = POSTCODE_UNITS$Postcode %>% map_chr( ~str_replace(.x, " .*", "" ) )
# 
# POSTCODE_UNITS.mask_broad = POSTCODE_UNITS.outcodes %in% list_of_outcodes
# POSTCODE_UNITS.mask_focus = POSTCODE_UNITS$Postcode %in% postcode_holder$postcode
# 
# POSTCODE_UNITS.focus = subset( POSTCODE_UNITS, POSTCODE_UNITS.mask_focus )
# POSTCODE_UNITS.broad = subset( POSTCODE_UNITS, POSTCODE_UNITS.mask_broad )
# 
# proj4string( POSTCODE_UNITS.broad )
# 
# 
### Combine with trial data
# circle_fams <- merge( POSTCODE_UNITS.broad,
#                       postcode_holder,
#                       by = "Postcode" )
# 
# #num_pal <- colorNumeric( "Reds", circle_fams$SIMD16_Decile )
# 
# leaflet( POSTCODE_UNITS.broad ) %>% 
#   addProviderTiles( providers$Stamen.TonerLite ) %>%
#   setView( lng=centre_longitude,
#            lat=centre_latitude,
#            zoom = 10 ) %>%
#   addCircleMarkers(data=postcode_holder,
#                    ~longitude,
#                    ~latitude,
#                    popup = ~(Postcode),
#                    radius = ~ifelse(group == "Hospital",20, 5),
#                    color  = ~marker_palette(group),
#                    stroke = TRUE,
#                    fillOpacity = 0.5
#   ) %>% 
#   # addPolygons( weight = 1,
  #              fillOpacity = 0.33,
  #              popup = ~ ( Postcode ),
  #              smoothFactor = 0.5,
  #              color = "red",
  #              highlightOptions = highlightOptions( color = "black",
  #                                                   weight = 5,
  #                                                   bringToFront = TRUE ) ) #%>%
  # addPolygons( data = circle_fams,
  #              weight = 15,
  #              fillOpacity = 0.5,
  #              popup = ~ paste( "Postcode sector: ", Postcode,
  #                               "<br> Zone: ", zone_intermediate,
  #                               sep = "" ),
  #              popupOptions = popupOptions( keepInView = TRUE ),
  #              smoothFactor = 0.5,
  #              #color = ~ num_pal( SIMD16_Decile ),
  #              highlightOptions = highlightOptions( color = "red",
  #                                                   weight = 15,
  #                                                   bringToFront = TRUE ) ) #%>%
# addAwesomeMarkers( icon = icon.fa, 
#                    lng = -3.2472141, 
#                    lat = 55.9721032, 
#                    popup = "Circle charity" ) %>%
# addLegend( "topright", 
#            pal = num_pal, values = families_to_plot$SIMD16_Decile,
#            title = "Postcode<br> deprivation<br> (SIMD16 Deciles)",
#            labFormat = labelFormat( prefix = "" ),
#            opacity = 1 )
#            
#            



post.units <- readOGR( dsn = "NRS_shapefiles/spd-unit-boundaries-cut-19-1", layer = "PC_Cut_19_1" )
post.units = spTransform( post.units, CRS( "+proj=longlat +datum=WGS84" ) )
# post.units = subset( post.units, grepl( "^PA", post.units$Postcode ) )
post.units.outcodes = post.units$Postcode %>% map_chr( ~str_replace(.x, " .*", "" ) )
post.units = subset( post.units, post.units.outcodes %in% list_of_outcodes )
proj4string( post.units )

circle_fams <- subset( post.units, post.units$Postcode %in% postcode_holder$Postcode )
circle_fams <- merge( circle_fams, postcode_holder, by = "Postcode" )

icon.fa <- makeAwesomeIcon( icon = 'flag', markerColor = 'red', iconColor = 'black' )


postcode_map = leaflet( post.units ) %>% 
  addProviderTiles( providers$Stamen.TonerLite ) %>%
  setView( lng=centre_longitude,
           lat=centre_latitude,
           zoom = 10 ) %>%
  addPolygons( weight = 0.5, fillOpacity = 0.33, popup = ~ ( Postcode ), 
               smoothFactor = 0.5, color = "black",
               highlightOptions = highlightOptions( color = "black", weight = 5, bringToFront = TRUE ) ) %>%
  addPolygons( data = circle_fams,
               weight = 15, 
               fillOpacity = 0.5,
               popup = ~ paste( "Postcode sector: ", Postcode,
                                # "<br> Resident families: ", FamilyID,
                                sep = "" ),
               popupOptions = popupOptions( keepInView = TRUE ),
               smoothFactor = 0.5,
               # color = ~ num_pal( SIMD16_Decile ),
               highlightOptions = highlightOptions( color = "red",
                                                    weight = 15,
                                                    bringToFront = TRUE ) ) %>%
  addAwesomeMarkers( data=postcode_holder %>% filter( group=="Hospital"),
                     icon = icon.fa, 
                     ~longitude, 
                     ~latitude, 
                     popup = "RAH" ) #%>%
  # addLegend( "topright", 
  #            pal = num_pal, values = families_to_plot$SIMD16_Decile,
  #            title = "Postcode<br> deprivation<br> (SIMD16 Deciles)",
  #            labFormat = labelFormat( prefix = "" ),
  #            opacity = 1 )

save( postcode_map,
      post.units,
      circle_fams,
      file=sprintf( "dat/%d_postcode_map.Rdat",
                    number_of_participants ) )


