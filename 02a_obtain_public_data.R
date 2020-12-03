library( magrittr )
library( stringr )
library( dplyr )
library( RCurl )

load("dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat")


BASE_QUERY_STRING = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:v4=\"http://Livetravelsuite.trapezegroup.co.uk/v4-4\" xmlns:sch=\"http://xml.trapezegroup.co.uk/schemas\">
   <soapenv:Header/>
   <soapenv:Body>
      <v4:ItineraryRequestStructureElement RequestId=\"VALUE_REQUEST_ID\">
         <sch:Origins>
            <sch:Origin>
               <sch:OriginPlace>
                  <sch:Geocode>
                     <sch:Easting>VALUE_ORIGIN_EASTING</sch:Easting>
                     <sch:Northing>VALUE_ORIGIN_NORTHING</sch:Northing>
                  </sch:Geocode>
                </sch:OriginPlace>
            </sch:Origin>
         </sch:Origins>
         <sch:Destinations>
            <sch:Destination>
               <sch:DestinationPlace>
                  <sch:Geocode>
                     <sch:Easting>VALUE_DESTINATION_EASTING</sch:Easting>
                     <sch:Northing>VALUE_DESTINATION_NORTHING</sch:Northing>
                  </sch:Geocode>
               </sch:DestinationPlace>
            </sch:Destination>
         </sch:Destinations>
         <sch:Arrival>
            <sch:TargetArrivalTime>VALUE_ARRIVAL_TIME</sch:TargetArrivalTime>
         </sch:Arrival>
         <sch:SearchVariations>
            <sch:SearchVariation SearchVariationId=\"Timely\"/>
         </sch:SearchVariations>
         <sch:ResponseCharacteristics>
            <sch:MaxEarlierItineraries>3</sch:MaxEarlierItineraries>
            <sch:MaxLaterItineraries>1</sch:MaxLaterItineraries>
         </sch:ResponseCharacteristics>
      </v4:ItineraryRequestStructureElement>
   </soapenv:Body>
</soapenv:Envelope>"

generate_SOAP_query = function( request_id,
                                #origin_name,
                                origin_easting,
                                origin_northing,
                                #destination_name,
                                destination_easting,
                                destination_northing,
                                arrival_time
) {
  
  s = BASE_QUERY_STRING %>%
    str_replace( "VALUE_REQUEST_ID", request_id) %>% # 777000000001
    str_replace( "VALUE_ORIGIN_EASTING" , origin_easting ) %>% 
    str_replace( "VALUE_ORIGIN_NORTHING" , origin_northing ) %>%
    str_replace( "VALUE_DESTINATION_EASTING" , destination_easting ) %>% 
    str_replace( "VALUE_DESTINATION_NORTHING" , destination_northing ) %>%
    str_replace( "VALUE_ARRIVAL_TIME", arrival_time )
  
  return( s )
}



API_URL = "https://api-2445581400627.apicast.io:443/travelsuite-webservice/services/v4/?WSDL"
API_KEY = "a1fe53720f5c5261d2043dbab2775c45"



### HOSPITAL
destination_postcode = hospital_postcode
destination_postcode.info = postcode_lookup( destination_postcode )
destination_postcode.eastings = destination_postcode.info$eastings %>% as.character()
destination_postcode.northings = destination_postcode.info$northings %>% as.character()


XML_tracker = postcode_holder %>%
  filter( group == "Participant") %>%
  select( idnum, postcode ) %>% 
  mutate( SUCCESS = NA )

for ( i in 1:nrow( postcode_holder %>% filter( group == "Participant" ) ) ) {
  
  origin_postcode = (postcode_holder %>% pull(postcode)) [i]
  
  journey.file = sprintf( "dat/BJP_XML/BJP_%04i_%s_%s.xml",
                          i,
                          origin_postcode %>% str_replace(" ", ""),
                          destination_postcode %>% str_replace(" ", "") )
  
  if ( !file.exists( journey.file) ) {
    
    origin_postcode.info = postcode_lookup( origin_postcode )
    origin_postcode.eastings = origin_postcode.info$eastings %>% as.character()
    origin_postcode.northings = origin_postcode.info$northings %>% as.character()
    
    h = basicTextGatherer()
    
    curl.out = curlPerform(url = API_URL,
                              httpheader = headerFields,
                              postfields = generate_SOAP_query(
                                request_id = sprintf( "%s -> %s",
                                                      origin_postcode,
                                                      destination_postcode),
                                origin_easting = origin_postcode.eastings,
                                origin_northing = origin_postcode.northings,
                                destination_easting = destination_postcode.eastings ,
                                destination_northing = destination_postcode.northings,
                                arrival_time = "2020-11-03T13:00:00"
                              ),
                              writefunction = h$update
    )

    fileConn<-file(journey.file)
    writeLines(h$value(), fileConn)
    close(fileConn)
    
    cat( sprintf( "[%04d] %8s : XML written to [%s]\n",
                  i,
                  origin_postcode,
                  journey.file ) )
    
    XML_tracker$SUCCESS[i] = TRUE
    
    Sys.sleep( 2 )
    
  } else {
    cat( sprintf( "[%04d] %8s : XML already saved to [%s]\n",
                  i,
                  origin_postcode,
                  journey.file ) )
  }
  
}