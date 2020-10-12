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

marker_palette = colorFactor( c("navy","red"),
                              domain = c("Hospital","Participant"))


### ============================================================= ###
### BASIC MAP (patients as points)                                ###
### ============================================================= ###

leaflet(postcode_holder) %>%
  setView( lng=centre_longitude,
           lat=centre_latitude,
           zoom = 10 ) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(
    ~longitude,
    ~latitude,
    radius = ~ifelse(group == "Hospital",20, 5),
    color  = ~marker_palette(group),
    stroke = TRUE,
    fillOpacity = 0.5
  )

### ============================================================= ###
### AREA MAP (patients mapped to postcode areas)                  ###
### ============================================================= ###


### Read in shapefiles, the output is an object of the class
### SpatialPolygonsDataFrame
postcode.objects = readOGR( dsn = "NRS_shapefiles/spd-unit-boundaries-cut-19-1", layer = "PC_Cut_19_1" )
### Transform data in shapefile to the necessary format
postcode.objects = spTransform( postcode.objects, CRS( "+proj=longlat +datum=WGS84" ) )

### Identify the postcode objects that we are interested in
### and retain only the data in the SpatialPolygonsDataFrame that
### is relevant for the areas of interest 
postcode.objects.outcodes = postcode.objects$Postcode %>% map_chr( ~str_replace(.x, " .*", "" ) )
postcode.objects = subset( postcode.objects,
                           postcode.objects.outcodes %in% list_of_outcodes )
### Set projection attributes
proj4string( postcode.objects )

### Identify which postcodes are listed in our list of patients
### (i.e, postcode_holder ) and combine our postcode_holder data
### with the information from the shape files
postcode.objects.PATIENTS <- subset( postcode.objects,
                                     postcode.objects$Postcode %in% postcode_holder$Postcode )
postcode.objects.PATIENTS <- merge( postcode.objects.PATIENTS, postcode_holder, by = "Postcode" )

icon.fa <- makeAwesomeIcon( icon = 'flag', markerColor = 'red', iconColor = 'black' )

postcode_map = leaflet( postcode.objects ) %>% 
  addProviderTiles( providers$Stamen.TonerLite ) %>%
  setView( lng=centre_longitude,
           lat=centre_latitude,
           zoom = 10 ) %>%
  ### Add the polygons for the postcode areas, data for this are
  ### provided in postcode.objects)
  addPolygons( weight = 0.5, fillOpacity = 0.33, popup = ~ ( Postcode ), 
               smoothFactor = 0.5, color = "black",
               highlightOptions = highlightOptions( color = "black", weight = 5, bringToFront = TRUE ) ) %>%
  ### Add the polygomes for those postcode objects to which 
  ### our patient population has been mapped
  addPolygons( data = postcode.objects.PATIENTS,
               weight = 15, 
               fillOpacity = 0.5,
               popup = ~ paste( "Postcode sector: ", Postcode, sep = "" ),
               popupOptions = popupOptions( keepInView = TRUE ),
               smoothFactor = 0.5,
               # color = ~ num_pal( SIMD16_Decile ),
               highlightOptions = highlightOptions( color = "red",
                                                    weight = 15,
                                                    bringToFront = TRUE ) ) %>%
  ### Add the hospital
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
      postcode.objects,
      postcode.objects.PATIENTS,
      marker_palette,
      centre_longitude,
      centre_latitude,
      file=sprintf( "dat/%d_postcode_map.Rdat",
                    number_of_participants ) )


