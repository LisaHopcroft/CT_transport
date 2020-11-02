library( dplyr    )
library( magrittr )
library( ggplot2 )
library( stringr )
library( xml2 )
library( tidyr )
library( purrr )
library( lubridate )

XML_content_OK = function(f) {
  f_content = readLines( this_file )
  
  content_acceptable = TRUE
  if ( any( f_content %>% str_detect( "502 Bad Gateway" ) ) ) {
    content_acceptable = FALSE
  } else if ( any( f_content %>% str_detect( "Fault occurred while processing.") ) ) {
    content_acceptable = FALSE
  } else if ( any( f_content %>% str_detect( "No matching results found") ) ) {
    content_acceptable = FALSE
  } 
  
  return( content_acceptable )
  
}

file_location = "dat/BJP_XML/"

file_list = dir( file_location,
                 full.names=TRUE,
                 pattern="BJP_\\d{4}_\\w*_\\w*.xml" )

track_progress = tibble(
  file = file_list,
  pass = NA
)



# transport_modes.all = xml.itinerary_list %>%
#   xml_find_all(  xpath=".//Mode"  ) %>%
#   xml_contents %>%
#   as.character() %>%
#   unique
# 
# for ( this.mode in transport_modes.all ) {
#   parsed_journey_times.blank = parsed_journey_times.blank %>% 
#     mutate( !!sym(this.mode) := 0 )
# }


parsed_journey_times.blank = tibble( processing_number = NA,
                                     origin = NA,
                                     destination = NA,
                                     start  = NA,
                                     end    = NA ) %>% 
  mutate( bus   = 0,
          rail  = 0,
          walk  = 0,
          metro = 0,
          ferry = 0 )

parsed_journey_times = parsed_journey_times.blank


for ( this_file in file_list ) {
  
  this.processing_information = this_file %>%
    basename() %>%
    str_match( "BJP_(\\d{4})_(\\w*)_(\\w*).xml" ) %>%
    as.list() %>%
    unlist %>% set_names( c( "file",
                             "processing_number",
                             "origin",
                             "destination") )
  
  this.file_mask = track_progress$file == this_file 
  
  if ( this_file %>% XML_content_OK() ) {
    track_progress[ this.file_mask, "pass" ] = TRUE
    
  xml.in  = read_xml( this_file )
  xml_ns_strip(xml.in)
  
  xml.itinerary_list = xml_find_all( xml.in, ".//Itinerary")
  this.n_itineraries = xml.itinerary_list %>% length
  
  this.home_string = this.processing_information[["origin"]]
  this.destination_string = this.processing_information[["destination"]]
  
  # tmp_journey_times = tibble( 
  #   origin = this.home_string,
  #   file = rep.int( this.processing_information["file"], this.n_itineraries ),
  #   processing_number = rep.int( this.processing_information["processing_number"], this.n_itineraries ),
  #   start = NA,
  #   end = NA
  #   )

  tmp_journey_times = parsed_journey_times.blank %>% 
    slice( rep.int(1,this.n_itineraries) ) %>% 
    mutate( processing_number = this.processing_information["processing_number"],
            file = this.processing_information["file"],
            origin = this.home_string,
            destination = this.destination_string )
  
  
  for ( option.i in 1:this.n_itineraries ) {
    this.itinerary = xml.itinerary_list[[option.i]]
    
    # xml_structure(this.itinerary)

    these.departures = xml_find_all( this.itinerary, xpath=".//ScheduledDepartureTime" )
    these.arrivals   = xml_find_all( this.itinerary, xpath=".//ScheduledArrivalTime" )
    
    these.modes = xml_find_all( this.itinerary, xpath=".//Mode" ) %>%
      xml_contents( ) %>%
      as.character( ) %>% 
      table

    this.start_text = these.departures %>% xml_text() %>% min
    this.end_text = these.arrivals %>% xml_text() %>% max
    
    this.start = this.start_text %>% ymd_hms() 
    this.end = this.end_text %>% ymd_hms() 
    
    tmp_journey_times$start[option.i] = this.start_text
    tmp_journey_times$end[option.i] = this.end_text
    
    for ( mode.i in 1:length( these.modes ) ) {
      tmp_journey_times[[these.modes[mode.i] %>% names]][option.i] = these.modes[[mode.i]]
    }
  }

  parsed_journey_times = parsed_journey_times %>% 
    bind_rows( tmp_journey_times )
  
  
  } else {
    cat( sprintf( "[%s] FAILED quality check\n", this_file ) )
    track_progress[ this.file_mask, "pass" ] = FALSE
    
  }
  
}

cat( sprintf( "%d files not parsed due to errors\n", track_progress %>% filter(!pass) %>% nrow() ) )

PUBLIC_journey_times.all = parsed_journey_times %>% 
  filter( !is.na( processing_number ) ) %>% 
  mutate( duration = as.numeric(difftime(ymd_hms(end),ymd_hms(start)),units="hours") ) %>% 
  mutate( num_legs = bus+rail+walk+metro+ferry )

PUBLIC_journey_times.fastest = PUBLIC_journey_times.all %>% 
  group_by( processing_number, origin, destination, file ) %>% 
  summarise( minimum_journey_time = min( duration ),
             journey_n = which.min( duration ),
             minimum_journey_legs = num_legs[journey_n] )

ggplot( PUBLIC_journey_times.fastest,
        aes( minimum_journey_time) ) +
  geom_histogram( aes(y = ..density..) ) +
  geom_density()

ggplot( PUBLIC_journey_times.fastest,
        aes( x=1, y=minimum_journey_time ) ) +
  geom_violin() + geom_jitter( width=0.1 )

save( track_progress,
      PUBLIC_journey_times.all,
      PUBLIC_journey_times.fastest,
      file=sprintf( "dat/02b_PUBLIC-journey-times_n=%d.Rdat",
                    number_of_participants ) )

