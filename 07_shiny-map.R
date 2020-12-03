#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library( shiny    )
library( dplyr    )
library( magrittr )
library( ggplot2  )
library( stringr  )
library( leaflet  )
library( purrr    )

load( "dat/00_PREPARATION.Rdat" )
load( "dat/01c_MAPPING-INFORMATION_n=2000.Rdat")
load( "dat/05a_SYNTHETIC-DATA_n=1958.Rdat")


trial_data = trial_data.synthetic %>% 
    inner_join( PATIENT.DATA %>% 
                    rename( idnum = id ) %>% 
                    select( idnum, postcode ) )

metric_list = trial_data %>% colnames %>%
    keep( ~str_detect(.x, "^METRIC_") )

names(metric_list) = metric_list %>%
    str_replace( "METRIC_", "" ) %>% 
    str_replace_all( "_", " " ) %>% 
    str_to_sentence( )

### Initialising slider - use the first metric as default
this_metric = metric_list[1]

### Define some nice breakpoints
these_breaks = trial_data %>% 
    pull( sym(this_metric) ) %>% 
    base::pretty()

this_step = these_breaks[2]-these_breaks[1]
this_min  = min(these_breaks)
this_max  = max(these_breaks)


ui = fluidPage(
    title="A Shiny New Layout",
    fluidRow(
        column(9,
               h1("MAP"),
               leafletOutput(outputId = "mapPlot")),
        column(3,
               h4("Dataset selection"),
               selectInput("metric", "Travel metric:", metric_list),
               plotOutput(outputId = "metric_histogram"),
               sliderInput(inputId = "metric_threshold",
                           label = "Threshold:",
                           min = this_min,
                           max = this_max,
                           value = this_max ),
               textOutput(outputId = "patient_selection_message") )
    )
)

### ============================================================= ###
### SERVER LOGIC                                                  ###
### ============================================================= ###

server <- function(input, output, session) {
    
    ### Edit the parameters of the threshold slider
    ### according to the metric that is chosen in the dropdown
    ### box.
    observe({
        this_metric = input$metric
        
        ### Define some nice breakpoints
        these_breaks = trial_data %>% 
            pull( sym(this_metric) ) %>% 
            base::pretty()
        
        this_step = these_breaks[2]-these_breaks[1]
        this_min  = min(these_breaks)
        this_max  = max(these_breaks)
        
        updateSliderInput( session,
                           "metric_threshold",
                           min=this_min,
                           max=this_max,
                           step=this_step,
                           val=this_max )
        
       
    })
    
    observe({ ### Add filtered data
        }) ### or eventReactive function
    
    output$mapPlot <- renderLeaflet({
        filter_string = sprintf( "%s < %f",
                                 input$metric,
                                 input$metric_threshold ) 
        
        this_leaflet_data = trial_data %>%
            filter( rlang::eval_tidy( rlang::parse_expr(filter_string) ) )
        
        # leaflet(this_leaflet_data ) %>%
        #         setView( lng=centre_longitude,
        #                  lat=centre_latitude,
        #                  zoom = 9 ) %>% 
        #     addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        #     # addMarkers(~longitude,
        #     #            ~latitude,
        #     #            popup = ~as.character(id),
        #     #            label = ~as.character(postcode))
        #     addCircleMarkers(
        #         ~longitude,
        #         ~latitude,
        #         radius = ~ifelse(group == "Hospital",20, 5),
        #         color  = ~marker_palette(group),
        #         stroke = TRUE,
        #         fillOpacity = 0.5
        #     )
        
        this_patient_data = subset( postcode.objects.PATIENTS,
                                    postcode.objects.PATIENTS$Postcode %in% this_leaflet_data$postcode )
        
        # draw_patient_map( data           = postcode.objects,
        #                   data_patients  = this_patient_data,
        #                   data_postcodes = postcode_holder,
        #                   focus_label    = "RAH",
        #                   centre_long    = centre_longitude,
        #                   centre_lat     = centre_latitude )
        # 
        # add_annotations_to_basic_map( basic_map = BASIC_MAP,
        #                               data_patients  = this_patient_data,
        #                               data_postcodes = postcode_holder,
        #                               focus_label    = "RAH",
        #                               centre_long    = centre_longitude,
        #                               centre_lat     = centre_latitude )

        icon.fa = makeAwesomeIcon( icon = 'flag',
                                   markerColor = 'red',
                                   iconColor = 'black' )

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
            addPolygons( data = this_patient_data,
                         weight = 15,
                         fillOpacity = 0.5,
                         color = "yellow",
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
                               popup = "RAH" )
    })
    
    output$metric_histogram <- renderPlot({
        ggplot( trial_data,
                aes_string(input$metric)) +
            geom_histogram( )
        
    })
    
    output$patient_selection_message <- renderText({
        filter_string = sprintf( "%s < %f",
                                 input$metric,
                                 input$metric_threshold ) 
        tmp = trial_data %>%
            filter( rlang::eval_tidy( rlang::parse_expr(filter_string) ) )
        
        sprintf( "This filter chooses %d patients\n", nrow(tmp) )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
