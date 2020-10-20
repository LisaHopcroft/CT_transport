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

load( "dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat")

postcode_holder %>% 
  filter( group == "Participant" ) %>% 
  write_csv( "dat/01b_RANDOM-POSTCODES_NEW_participants.csv" )

postcode_holder %>% 
  filter( group == "Hospital" ) %>% 
  write_csv( "dat/01b_RANDOM-POSTCODES_NEW_hospital.csv" )


### Writing output file for Traveline Scotland.

traveline_output.data = postcode_holder %>% 
  filter( group == "Participant" ) 

traveline_output = tibble(
  PostcodeOrigin = traveline_output.data$postcode %>% str_replace( " ", "" ) ,
  PostcodeDestination = hospital_postcode,
  OriginName = sprintf( "Home|%d|%s",
                        1:number_of_participants,
                        traveline_output.data$postcode ),
  DestinationName = "RAH",
  ArriveTime = "1
  13:00",
  ArriveDays = "Tu",
  DepartTime = "",
  DepartDays = "",
  JourneyDuration = "",
  JourneyChanges = "",
  EmailAddress = "",
  TransportModes = ""
)

### Note that Traveline aren't good at handling situations
### where a journey isn't found.  When first submitting
### these jobs, I would get errors for a few lines and the
### whole job would fail (I think that there must be a 
### maximum tolerated number of errors, because the whole
### job does not always fail).
### 
### I am going to handle this manually in the meantime,
### as I know where the errors are occuring.

lines_to_remove = c( 52, 81, 169 )
traveline_output.clean = traveline_output[-1*lines_to_remove,]
traveline_output.n = traveline_output.clean %>% nrow
  
batch_size = 900
num_batches = ceiling( number_of_participants/batch_size )
batch_start = seq(1,number_of_participants,by=batch_size)

for ( i in 1:num_batches ) {
  this_start = batch_start[i] 
  this_end   = min(c(this_start + batch_size - 1,
                     traveline_output.n))
  this_batch = traveline_output.clean[this_start:this_end,]
  
  this_batch_filename = sprintf( "dat/01_TRAVELINE-UPLOAD_NEW_CLEAN-BATCH%d_%d-%d.csv",
                                 i,
                                 this_start,
                                 this_end )
  
  cat( sprintf( "Writing out Traveline (clean) batch #%d [%4d-%4d, %3d records]\n",
                i,
                this_start,
                this_end,
                nrow( this_batch )) )
  
  this_batch %>% 
    write.table( this_batch_filename,
                 row.names = FALSE,
                 quote = FALSE,
                 sep=",",
                 eol = "\r\n" )

}


