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

number_of_participants = 2000

list_of_outcodes = c( sprintf( "PA%d", c( 1:19 ) ),
                      sprintf( "G%d" , c( 41:46,
                                          51:53,
                                          60,
                                          73:78,
                                          81, 82, 84) )
)

hospital_postcode = "PA29PN"
hospital_postcode_information = postcode_lookup( hospital_postcode )

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
  filter( !is.na( idnum ) ) %>% 
  arrange( idnum ) %>% 
  mutate( group = ifelse( is.na(idnum), "Hospital", "Participant") )

save( postcode_holder,
      list_of_outcodes,
      hospital_postcode,
      hospital_postcode_information,
      number_of_participants,
      file=sprintf( "dat/01_RANDOM-POSTCODES_n=%d.Rdat",
                    number_of_participants ) )

postcode_holder %>% 
  filter( group == "Participant" ) %>% 
  write_csv( "dat/01_RANDOM-POSTCODES_participants.csv" )

postcode_holder %>% 
  filter( group == "Hospital" ) %>% 
  write_csv( "dat/01_RANDOM-POSTCODES_hospital.csv" )

### Writing output file for Traveline Scotland.
traveline_output = tibble(
  PostcodeOrigin = postcode_holder$postcode %>% str_replace( " ", "" ) ,
  PostcodeDestination = hospital_postcode,
  OriginName = sprintf( "Home|%d|%s",
                        1:number_of_participants,
                        postcode_holder$postcode ),
  DestinationName = "RAH",
  ArriveTime = "13:00",
  ArriveDays = "Tu",
  DepartTime = "",
  DepartDays = "",
  JourneyDuration = "",
  JourneyChanges = "",
  EmailAddress = "",
  TransportModes = ""
)

batch_size = 800
num_batches = ceiling( number_of_participants/batch_size )
batch_start = seq(1,number_of_participants,by=batch_size)

for ( i in 1:num_batches ) {
  this_start = batch_start[i] 
  this_end   = min(c(this_start + batch_size - 1,
                     number_of_participants))
  this_batch = traveline_output[this_start:this_end,]
  
  this_batch_filename = sprintf( "dat/01_TRAVELINE-UPLOAD_BATCH%d_%d-%d.csv",
                                 i,
                                 this_start,
                                 this_end )
  
  cat( sprintf( "Writing out Traveline batch #%d [%4d-%4d, %3d records]\n",
                i,
                this_start,
                this_end,
                nrow( this_batch )) )
  
  this_batch %>% 
    write.table( this_batch_filename,
                 row.names=FALSE )

}


