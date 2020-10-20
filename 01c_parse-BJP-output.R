library( dplyr    )
library( magrittr )
library( ggplot2 )
library( stringr )
library( xml2 )
library( tidyr )

file_location = "BJP"
file_list = dir( file_location,
                 full.names=TRUE,
                 pattern="journey_\\d+_of_\\d+.xml" )

parsed_journey_times = tibble( processing_number = NA,
                               processing_batchsize = NA,
                               origin = NA,
                               start  = NA,
                               end    = NA )

for ( this_file in file_list ) {
  
  this.processing_information = this_file %>%
    basename() %>%
    str_match( "journey_(\\d+)_of_(\\d+).xml" ) %>%
    as.list() %>%
    unlist %>% set_names( c( "file",
                             "processing_number",
                             "processing_batchsize") )
  
  xml.in  = read_xml( this_file )
  xml_ns_strip(xml.in)
  
  xml.itinerary_list = xml_find_all( xml.in, ".//Itinerary")
  this.n_itineraries = xml.itinerary_list %>% length
  
  this.home_string = xml.itinerary_list %>%
    xml_find_all( ".//Name" ) %>%
    xml_text() %>%
    keep( ~str_detect(.x,"^Home") )
  
  tmp_journey_times = tibble( 
    origin = this.home_string,
    file = rep.int( this.processing_information["file"], this.n_itineraries ),
    processing_number = rep.int( this.processing_information["processing_number"], this.n_itineraries ),
    processing_batchsize = rep.int( this.processing_information["processing_batchsize"], this.n_itineraries ),
    start = NA,
    end = NA
    )
  
  for ( option.i in 1:length(xml.itinerary_list ) ) {
    this.itinerary = xml.itinerary_list[[option.i]]
    
    # xml_structure(this.itinerary)

    these.departures = xml_find_all( this.itinerary, xpath=".//ScheduledDepartureTime" )
    these.arrivals   = xml_find_all( this.itinerary, xpath=".//ScheduledArrivalTime" )
    
    this.start_text = these.departures %>% xml_text() %>% min
    this.end_text = these.arrivals %>% xml_text() %>% max
    
    this.start = this.start_text %>% ymd_hms() 
    this.end = this.end_text %>% ymd_hms() 
    
    tmp_journey_times$start[option.i] = this.start_text
    tmp_journey_times$end[option.i] = this.end_text
  }

  parsed_journey_times = parsed_journey_times %>% 
    bind_rows( tmp_journey_times )
  
}

PUBLIC_journey_times.all = parsed_journey_times %>% 
  filter( !is.na( processing_number ) ) %>% 
  mutate( duration = as.numeric(difftime(ymd_hms(end),ymd_hms(start)),units="hours") ) %>% 
  separate( origin, c(NA,"entry_number","Postcode"), sep="\\|" )

PUBLIC_journey_times.fastest = PUBLIC_journey_times.all %>% 
  group_by( entry_number, processing_number, processing_batchsize, Postcode, file ) %>% 
  summarise( minimum_journey_time = min( duration ),
             journey_n = which.min( duration ) )

save( PUBLIC_journey_times.all,
      PUBLIC_journey_times.fastest,
      file=sprintf( "dat/01c_PUBLIC-journey-times_n=%d.Rdat",
                    number_of_participants ) )

