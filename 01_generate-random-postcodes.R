library( dplyr    )
library( magrittr )
library( readr )
library( ggplot2 )
library( stringr )
### To install PostcodesioR
### (1) In terminal: find /../miniconda3/* -name "libcurl.pc" 
###     Find the pkgconfig entry, call it X
### (2) In R: Sys.setenv(PKG_CONFIG_PATH = X )
library( PostcodesioR )

set.seed( "5482" )

number_of_participants = 20

list_of_outcodes = c( sprintf( "PA%d", c( 1:6, 10:11 ) ),
                      sprintf( "G%d" , c( 52, 53, 60, 77, 78, 81, 82) )
)

hospital_postcode = "PA29PN"
hospital_postcode_information = postcode_lookup( hospital_postcode )

# postcode_holder = tibble()
# 
# postcode_columns = c(
#   "outcode"          , "postcode"  ,
#   "longitude"        , "latitude"  ,
#   "zone_intermediate", "zone_lower",
#   "admin_authority"   
# )
# 
# postcode_holder = postcode_columns %>%
#   purrr::map_dfc(setNames, object = list(character())) %>% 
#   mutate( id = integer()) %>%
#   mutate( longitude = double() ) %>% 
#   mutate( latitude = double() ) %>% 
#   select( id, everything() )

postcode_holder = tibble(
  idnum = NA,
  outcode  = hospital_postcode_information$outcode,
  postcode = hospital_postcode_information$postcode,
  longitude = hospital_postcode_information$longitude,
  latitude  = hospital_postcode_information$latitude,
  zone_intermediate = hospital_postcode_information$msoa,
  zone_lower        = hospital_postcode_information$lsoa,
  admin_authority   = hospital_postcode_information$admin_district
)

for ( i in 1:number_of_participants ) {
  
  this.outcode = sample( list_of_outcodes, 1 )
  
  this.postcode_information = random_postcode( this.outcode )
  
  if ( i %% 25 == 0 ) {
    cat( sprintf( "%04d Random postcodes generated\n",
                  i ) )
  }
  
  postcode_holder = postcode_holder %>% 
    add_row( idnum = i,
             outcode  = this.outcode,
             postcode = this.postcode_information$postcode,
             longitude = this.postcode_information$longitude,
             latitude  = this.postcode_information$latitude,
             zone_intermediate = this.postcode_information$msoa,
             zone_lower        = this.postcode_information$lsoa,
             admin_authority   = this.postcode_information$admin_district )
  
  
}

postcode_holder = postcode_holder %>%
  arrange( idnum ) %>% 
  mutate( group = ifelse( is.na(idnum), "Hospital", "Participant") )

save( postcode_holder,
      list_of_outcodes,
      hospital_postcode,
      hospital_postcode_information,
      number_of_participants,
      file=sprintf( "dat/%d_random-postcodes.Rdat",
                    number_of_participants ) )

postcode_holder %>% 
  filter( group == "Participant" ) %>% 
  write_csv( "dat/participants.csv" )

postcode_holder %>% 
  filter( group == "Hospital" ) %>% 
  write_csv( "dat/hospital.csv" )



