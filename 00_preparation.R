# library( dplyr )
library( rgdal ) 
library( magrittr )
library( stringr )
library( purrr )


#####################################################################
### RGDAL PRE PROCESSING                                          ###
#####################################################################

### Read in shapefiles, the output is an object of the class
### SpatialPolygonsDataFrame
postcode.objects.in = readOGR( dsn = "ref/NRS_shapefiles/spd-unit-boundaries-cut-19-1", layer = "PC_Cut_19_1" )
### Transform data in shapefile to the necessary format
postcode.objects.sp = spTransform( postcode.objects.in, CRS( "+proj=longlat +datum=WGS84" ) )

### Identify the postcode objects that we are interested in
### and retain only the data in the SpatialPolygonsDataFrame that
### is relevant for the areas of interest 
postcode.objects.sp.outcodes = postcode.objects.sp$Postcode %>% map_chr( ~str_replace(.x, " .*", "" ) )

#####################################################################
### MAP FUNCTIONS                                                 ###
#####################################################################


draw_patient_map = function( data,
                             data_patients,
                             data_postcodes,
                             focus_icon = makeAwesomeIcon( icon = 'flag',
                                                           markerColor = 'red',
                                                           iconColor = 'black' ),
                             focus_label = "FOCUS",
                             centre_long = -4.430403,
                             centre_lat = 55.859,
                             zoom_val = 10,
                             summarise = TRUE ) {
  
  if ( summarise ) {
    leaflet( data ) %>% 
      addProviderTiles( providers$Stamen.TonerLite ) %>%
      setView( lng=centre_long,
               lat=centre_lat,
               zoom = zoom_val ) %>%
      ### Add the polygons for the postcode areas, data for this are
      ### provided in postcode.objects)
      addPolygons( weight = 0.5,
                   fillOpacity = 0.33,
                   popup = ~ ( Postcode ), 
                   smoothFactor = 0.5,
                   color = "black",
                   highlightOptions = highlightOptions( color = "black",
                                                        weight = 2,
                                                        bringToFront = TRUE ) ) %>%
      ### Add the polygomes for those postcode objects to which 
      ### our patient population has been mapped
      addPolygons( data = data_patients,
                   weight = 2, 
                   fillOpacity = 0.5,
                   fill="purple",
                   popup = ~ paste( "Postcode sector: ", Postcode, sep = "" ),
                   popupOptions = popupOptions( keepInView = TRUE ),
                   smoothFactor = 0.5,
                   # color = ~ num_pal( SIMD16_Decile ),
                   highlightOptions = highlightOptions( color = "red",
                                                        weight = 5,
                                                        bringToFront = TRUE ) ) %>%
      ### Add the hospital
      addAwesomeMarkers( data=data_postcodes %>% filter( group=="Hospital"),
                         icon = focus_icon, 
                         ~longitude, 
                         ~latitude, 
                         popup = focus_label ) #%>%
    # addLegend( "topright", 
    #            pal = num_pal, values = families_to_plot$SIMD16_Decile,
    #            title = "Postcode<br> deprivation<br> (SIMD16 Deciles)",
    #            labFormat = labelFormat( prefix = "" ),
    #            opacity = 1 )
    #            
  } else {
    leaflet(data_postcodes) %>%
      setView( lng=centre_long,
               lat=centre_lat,
               zoom = zoom_val ) %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addProviderTiles( providers$Stamen.TonerLite ) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~ifelse(group == "Hospital",20, 5),
        color  = ~marker_palette(group),
        stroke = TRUE,
        fillOpacity = 0.5
      )
  }
}


add_annotations_to_basic_map = function( basic_map,
                                         data_patients,
                                         data_postcodes,
                                         focus_icon = makeAwesomeIcon( icon = 'flag',
                                                                       markerColor = 'red',
                                                                       iconColor = 'black' ),
                                         focus_label = "FOCUS",
                                         centre_long = -4.430403,
                                         centre_lat = 55.859,
                                         zoom_val = 10,
                                         summarise = TRUE ) {
  
  if ( summarise ) {
    BASIC_MAP %>% 
      setView( lng=centre_long,
               lat=centre_lat,
               zoom = zoom_val ) %>%
      ### Add the polygomes for those postcode objects to which 
      ### our patient population has been mapped
      addPolygons( data = data_patients,
                   weight = 2, 
                   fillOpacity = 0.5,
                   popup = ~ paste( "Postcode sector: ", Postcode, sep = "" ),
                   popupOptions = popupOptions( keepInView = TRUE ),
                   smoothFactor = 0.5,
                   # color = ~ num_pal( SIMD16_Decile ),
                   highlightOptions = highlightOptions( color = "red",
                                                        weight = 5,
                                                        bringToFront = TRUE ) ) %>%
      ### Add the hospital
      addAwesomeMarkers( data=data_postcodes %>% filter( group=="Hospital"),
                         icon = focus_icon, 
                         ~longitude, 
                         ~latitude, 
                         popup = focus_label ) #%>%
    # addLegend( "topright", 
    #            pal = num_pal, values = families_to_plot$SIMD16_Decile,
    #            title = "Postcode<br> deprivation<br> (SIMD16 Deciles)",
    #            labFormat = labelFormat( prefix = "" ),
    #            opacity = 1 )
    #            
  } else {
    leaflet(data_postcodes) %>%
      setView( lng=centre_long,
               lat=centre_lat,
               zoom = zoom_val ) %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addProviderTiles( providers$Stamen.TonerLite ) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~ifelse(group == "Hospital",20, 5),
        color  = ~marker_palette(group),
        stroke = TRUE,
        fillOpacity = 0.5
      )
  }
}

BASIC_MAP = leaflet( postcode.objects ) %>% 
  addProviderTiles( providers$Stamen.TonerLite ) %>%
  addPolygons( weight = 0.5,
               fillOpacity = 0.33,
               popup = ~ ( Postcode ), 
               smoothFactor = 0.5, color = "black",
               highlightOptions = highlightOptions( color = "black",
                                                    weight = 5,
                                                    bringToFront = TRUE ) )
               
               
save( postcode.objects.in,
      postcode.objects.sp,
      postcode.objects.sp.outcodes,
      draw_patient_map,
      add_annotations_to_basic_map,
      BASIC_MAP,
      file="dat/00_PREPARATION.Rdat" )