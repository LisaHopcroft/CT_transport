library( dplyr    )
library( rgdal ) 
library( magrittr )
library( ggplot2 )
library( stringr )
library( leaflet )
library( purrr )
library( sp )

load( "dat/00_PREPARATION.Rdat" )
load( "dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat" )

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

### ============================================================= ###
### AREA MAP (patients mapped to postcode areas)                  ###
### ============================================================= ###

postcode.objects = subset( postcode.objects.sp,
                           postcode.objects.sp.outcodes %in% list_of_outcodes )
### Set projection attributes
proj4string( postcode.objects )

### Identify which postcodes are listed in our list of patients
### (i.e, postcode_holder ) and combine our postcode_holder data
### with the information from the shape files
postcode.objects.PATIENTS <- subset( postcode.objects,
                                     postcode.objects$Postcode %in% postcode_holder$Postcode )
postcode.objects.PATIENTS <- merge( postcode.objects.PATIENTS, postcode_holder, by = "Postcode",
                                    duplicateGeoms = TRUE)

icon.fa <- makeAwesomeIcon( icon = 'flag', markerColor = 'red', iconColor = 'black' )

leaflet( postcode.objects ) %>% 
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

save( postcode.objects,
      postcode.objects.PATIENTS,
      marker_palette,
      centre_longitude,
      centre_latitude,
      file=sprintf( "dat/01c_MAPPING-INFORMATION_n=%d.Rdat",
                    number_of_participants ) )


### A function now exists to plot the map easily.
### This is defined in the 00_preparation .R script.

draw_patient_map( data           = postcode.objects,
                  data_patients  = postcode.objects.PATIENTS,
                  data_postcodes = postcode_holder,
                  focus_label    = "RAH",
                  centre_long    = centre_longitude,
                  centre_lat     = centre_latitude )

draw_patient_map( data           = postcode.objects,
                  data_patients  = postcode.objects.PATIENTS,
                  data_postcodes = postcode_holder,
                  focus_label    = "RAH",
                  centre_long    = centre_longitude,
                  centre_lat     = centre_latitude,
                  summarise      = FALSE )


add_annotations_to_basic_map( basic_map = BASIC_MAP,
                              data_patients  = postcode.objects.PATIENTS,
                              data_postcodes = postcode_holder,
                              focus_label    = "RAH",
                              centre_long    = centre_longitude,
                              centre_lat     = centre_latitude )


