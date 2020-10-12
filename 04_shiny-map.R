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

load( "../CT_transport/dat/20_random-postcodes.Rdat")
load( "../CT_transport/dat/20_synthetic-data.Rdat" )
load( "../CT_transport/dat/20_postcode_map.Rdat" )


trial_data = postcode_holder %>% 
    inner_join( trial_data.synthetic.LOADED )

metric_list = trial_data %>% colnames %>%
    keep( ~str_detect(.x, "^METRIC_") )
names(metric_list) = metric_list %>%
    str_replace( "METRIC_", "" ) %>% 
    str_replace( "_", " " ) %>% 
    str_to_sentence( )

centre_longitude  = trial_data %>% pull( longitude ) %>% mean
centre_latitude   = trial_data %>% pull( latitude  ) %>% mean

marker_palette = colorFactor( c("navy","red"),
                              domain = c("Hospital","Participant"))


# leaflet(postcode_holder) %>%
#     setView( lng=centre_longitude,
#              lat=centre_latitude,
#              zoom = 10 ) %>%
#     #addTiles() %>% 
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

# Define UI for app that draws a histogram ----
# ui <- fluidPage(
#     
#     # App title ----
#     titlePanel("Hello Shiny map!"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             # Input: Slider for the number of bins ----
#             sliderInput(inputId = "zoom_coef",
#                         label = "Zoom:",
#                         min = 1,
#                         max = 20,
#                         value = 10)
#             
#         ),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             # Output: Histogram ----
#             leafletOutput(outputId = "mapPlot")
#             
#         )
#     )
# )

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
                           min = min(trial_data[,metric_list[1]]),
                           max = max(trial_data[,metric_list[1]]),
                           value = max(trial_data[,metric_list[1]])))
    )
)

# ui <- fluidPage(
#     
#     title = "A Shiny New Layout",
#     
#     leafletOutput(outputId = "mapPlot"),
#     
#     hr(),
#     
#     fluidRow(
#         column(3,
#                h4("Diamonds Explorer"),
#                sliderInput(inputId = "zoom_coef",
#                            label = "Zoom:",
#                            min = 1,
#                            max = 20,
#                            value = 10),
#                br(),
#                checkboxInput('jitter', 'Jitter'),
#                checkboxInput('smooth', 'Smooth')
#         )#,
#         # column(4, offset = 1,
#         #        selectInput('x', 'X', names(dataset)),
#         #        selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
#         #        selectInput('color', 'Color', c('None', names(dataset)))
#         # ),
#         # column(4,
#         #        selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
#         #        selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
#         # )
#     )
# )


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    ### Edit the parameters of the threshold slider
    ### according to the metric that is chosen in the dropdown
    ### box.
    observe({
        this_metric = input$metric
        updateSliderInput( session,
                           "metric_threshold",
                           min=min(trial_data[,this_metric]),
                           max=max(trial_data[,this_metric]),
                           val=max(trial_data[,this_metric]) )
    })
    
    output$mapPlot <- renderLeaflet({
        filter_string = sprintf( "%s < %f",
                                 input$metric,
                                 input$metric_threshold ) 
        
        this_leaflet_data = trial_data %>% 
            filter( rlang::eval_tidy( rlang::parse_expr(filter_string) ) )
        
        leaflet(this_leaflet_data ) %>%
                setView( lng=centre_longitude,
                         lat=centre_latitude,
                         zoom = 9 ) %>% 
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
        
    })
    
    output$metric_histogram <- renderPlot({
        ggplot( trial_data,
                aes_string(input$metric)) +
            geom_histogram( )
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
