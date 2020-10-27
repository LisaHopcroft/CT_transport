library( magrittr )
library( stringr )

# <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v4="http://Livetravelsuite.trapezegroup.co.uk/v4-4" xmlns:sch="http://xml.trapezegroup.co.uk/schemas">
#   <soapenv:Header/>
#   <soapenv:Body>
#     <v4:ItineraryRequestStructureElement RequestId="ItnQry2">
#       <sch:Origins>
#         <sch:Origin>
#           <sch:OriginPlace>
#             <sch:StopLabel>60709108</sch:StopLabel>
#           </sch:OriginPlace>
#         </sch:Origin>
#       </sch:Origins>
#       
#       <sch:Destinations>
#         <sch:Destination>
#           <sch:DestinationPlace>
#             <sch:StopLabel>60903826</sch:StopLabel>
#           </sch:DestinationPlace>
#         </sch:Destination>
#       </sch:Destinations>
#       
#       <sch:Arrival>
#         <sch:TargetArrivalTime>2016-03-30T12:00:00</sch:TargetArrivalTime>
#       </sch:Arrival>
#       
#       <sch:SearchVariations>
#         <sch:SearchVariation SearchVariationId="Timely"/>
#       </sch:SearchVariations>
#         
#       <sch:ResponseCharacteristics>
#         <sch:MaxEarlierItineraries>3</sch:MaxEarlierItineraries>
#         <sch:MaxLaterItineraries>1</sch:MaxLaterItineraries>
#       </sch:ResponseCharacteristics>
#           
#     </v4:ItineraryRequestStructureElement>
#   </soapenv:Body>
# </soapenv:Envelope>
# 

BASE_QUERY_STRING.1 = "<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/'>
    <soapenv:Header>
    </soapenv:Header>
    <soapenv:Body>
        <v3-0:ItineraryRequestStructureElement xmlns:v3-0='http://Livetravelsuite.trapezegroup.co.uk/v3-0' xmlns='http://xml.trapezegroup.co.uk/schemas' RequestId='VALUE_REQUEST_ID'>
            <Origins>
                <Origin>
                    <OriginPlace>
                        <StopLabel>VALUE_ORIGIN_PLACE</StopLabel>
                        <Name>VALUE_ORIGIN_NAME</Name>
                    </OriginPlace>
                </Origin>
            </Origins>
            <Destinations>
                <Destination>
                    <DestinationPlace>
                        <Geocode>
                            <Easting>VALUE_DESTINATION_EASTING/Easting>
                            <Northing>VALUE_DESTINATION_NORTHING</Northing>
                        </Geocode>
                        <Name>VALUE_DESTINATION_NAME</Name>
                    </DestinationPlace>
                </Destination>
            </Destinations>
            <Arrival>
                <TargetArrivalTime>VALUE_ARRIVAL_TIME</TargetDepartureTime>
            </Arrival>
            <SearchVariations>
                <SearchVariation SearchVariationId='Timely'>
                </SearchVariation>
            </SearchVariations>
            <ResponseCharacteristics>
                <MaxEarlierItineraries>1</MaxEarlierItineraries>
            </ResponseCharacteristics>
        </v3-0:ItineraryRequestStructureElement>
    </soapenv:Body>
</soapenv:Envelope>"

BASE_QUERY_STRING.2 = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:v4=\"http://Livetravelsuite.trapezegroup.co.uk/v4-4\" xmlns:sch=\"http://xml.trapezegroup.co.uk/schemas\">
   <soapenv:Header/>
   <soapenv:Body>
      <v4:ItineraryRequestStructureElement RequestId=\"ItnQry2\">
         <sch:Origins>
            <sch:Origin>
               <sch:OriginPlace>
                  <sch:StopLabel>60709108</sch:StopLabel>
               </sch:OriginPlace>
            </sch:Origin>
         </sch:Origins>
         <sch:Destinations>
            <sch:Destination>
               <sch:DestinationPlace>
                  <sch:StopLabel>60903826</sch:StopLabel>
               </sch:DestinationPlace>
            </sch:Destination>
         </sch:Destinations>
         <sch:Arrival>
            <sch:TargetArrivalTime>2020-11-01T12:00:00</sch:TargetArrivalTime>
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


BASE_QUERY_STRING.3 = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:v4=\"http://Livetravelsuite.trapezegroup.co.uk/v4-4\" xmlns:sch=\"http://xml.trapezegroup.co.uk/schemas\">
   <soapenv:Header/>
   <soapenv:Body>
      <v4:ItineraryRequestStructureElement RequestId=\"VALUE_REQUEST_ID\">
         <sch:Origins>
            <sch:Origin>
               <sch:OriginPlace>
                 <sch:StopLabel>VALUE_ORIGIN_PLACE</sch:StopLabel>
                 <sch:Name>VALUE_ORIGIN_NAME</sch:Name>
               </sch:OriginPlace>
            </sch:Origin>
         </sch:Origins>
         <sch:Destinations>
            <sch:Destination>
               <sch:DestinationPlace>
                  <sch:StopLabel>VALUE_DESTINATION_PLACE</sch:StopLabel>
                  <sch:Name>VALUE_DESTINATION_NAME</sch:Name>
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


BASE_QUERY_STRING.4 = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:v4=\"http://Livetravelsuite.trapezegroup.co.uk/v4-4\" xmlns:sch=\"http://xml.trapezegroup.co.uk/schemas\">
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
                                origin_place,
                                origin_name,
                                destination.easting,
                                destination.northing,
                                destination.name,
                                arrival.time
                                ) {

  s = BASE_SOAP_STRING %>%
    str_replace( "VALUE_REQUEST_ID", request_id) %>% # 777000000001
    str_replace( "VALUE_ORIGIN_PLACE", origin_place) %>% # 777000000001
    str_replace( "VALUE_ORIGIN_NAME" , origin_name ) %>% 
    str_replace( "VALUE_DESTINATION_EASTING", destination.easting ) %>% 
    str_replace( "VALUE_DESTINATION_NORTHING", destination.northing ) %>% 
    str_replace( "VALUE_DESTINATION_NAME", destination.name ) %>% 
    str_replace( "VALUE_ARRIVAL_TIME", arrival.time )
    
  return( s )
}

generate_SOAP_query.3 = function( request_id,
                                origin_place,
                                origin_name,
                                destination_place,
                                destination_name,
                                arrival_time
) {
  
  s = BASE_QUERY_STRING.3 %>%
    str_replace( "VALUE_REQUEST_ID", request_id) %>% # 777000000001
    str_replace( "VALUE_ORIGIN_PLACE", origin_place) %>% # 777000000001
    str_replace( "VALUE_ORIGIN_NAME" , origin_name ) %>% 
    str_replace( "VALUE_DESTINATION_PLACE", destination_place ) %>% 
    str_replace( "VALUE_DESTINATION_NAME", destination_name ) %>% 
    str_replace( "VALUE_ARRIVAL_TIME", arrival_time )
  
  return( s )
}


generate_SOAP_query.4 = function( request_id,
                                  #origin_name,
                                  origin_easting,
                                  origin_northing,
                                  #destination_name,
                                  destination_easting,
                                  destination_northing,
                                  arrival_time
) {
  
  s = BASE_QUERY_STRING.4 %>%
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

# TEST_query.3 = generate_SOAP_query.3(
#     request_id = "TEST",
#     origin_place = "60709108" ,
#     origin_name = "Home",
#     destination_place = "60903826",
#     destination_name = "Office",
#     arrival_time = "2020-11-01T17:00:00"
# )

### https://api-2445581400627.apicast.io:443/travelsuite-webservice/services/v4/?WSDL
headerFields = c('user_key' = API_KEY )


origin_postcode = "PA12 4AW"
origin_postcode.info = postcode_lookup( origin_postcode )
origin_postcode.eastings = origin_postcode.info$eastings %>% as.character()
origin_postcode.northings = origin_postcode.info$northings %>% as.character()

destination_postcode = "KA13 7HR"
destination_postcode.info = postcode_lookup( destination_postcode )
destination_postcode.eastings = destination_postcode.info$eastings %>% as.character()
destination_postcode.northings = destination_postcode.info$northings %>% as.character()

curlPerform(url = API_URL,
            httpheader = headerFields,
            postfields = generate_SOAP_query.4(
              request_id = sprintf( "%s -> %s",
                                    origin_postcode,
                                    destination_postcode),
              origin_easting = origin_postcode.eastings,
              origin_northing = origin_postcode.northings,
              destination_easting = destination_postcode.eastings ,
              destination_northing = destination_postcode.northings,
              arrival_time = "2020-11-03T13:00:00"
            )
)

# library(httr)
# req <- GET(API_URL,
#            add_headers(user_key = API_KEY))
# stop_for_status(req) 
# content(req)