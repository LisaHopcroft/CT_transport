library( dplyr    )
library( magrittr )
library( ggplot2 )
library( stringr )
library( tidyr )
library( data.table )

### Tried to install via CRAN but encountered problems
### that weren't immediately clear
### Required installation of udunits2:
### conda install -c conda-forge udunits2 
### and installation of KernSmooth from CRAN - might not have been necessary?!
### ...also required an update of conda:
### conda update -n base -c defaults conda
library(osrm)

load("dat/00_PREPARATION.Rdat")
load("dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat")
#load("dat/02_MAPPING-INFORMATION_n=2000.Rdat")

# data("berlin")
# # Travel time matrix
# distA <- osrmTable(loc = apotheke.sf[1:5,])
# distA$durations

library(sf)

# Process is:
# (1) Convert to simple feature format (sf package)
#     https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-r
#     Note that methodology will be different if using version 0.2-7.
# (2) Use osrmTable (osrm package)
#     https://github.com/rCarto/osrm
#     Requests need to be batched as only 10000 durations can be generated
#     at a time.

#####################################################################
### Convert to simple feature format                              ###
#####################################################################

postcode_holder.sf = st_as_sf(as.data.table(postcode_holder),
                              coords = c("longitude", "latitude"), 
                              crs = 4326, agr = "constant")
### Quick plot of data (x,y coordinates) + overlay of various parameters.
# plot(postcode_holder.sf)


#####################################################################
### Calculate travel time distance                                ###
#####################################################################

# distance_matrix = osrmTable(loc = postcode_holder.sf[1:100,])
# distance_matrix$durations

### Batch time distance calculations (can only do 100 at a time,

batch_size = 100-1 # Hospital needs to be included
num_batches = ceiling( number_of_participants/batch_size )
batch_start = seq(1,number_of_participants,by=batch_size)

hospital_mask = which( postcode_holder.sf$group=="Hospital" )

duration_time_to_hospital = rep.int( NA, number_of_participants+1 )

for ( i in 1:num_batches ) {
  
  this_start = batch_start[i] 
  this_end   = min(c(this_start + batch_size - 1,
                     number_of_participants))
  
  this_batch = postcode_holder.sf[c(this_start:this_end,hospital_mask),]
  
  cat( sprintf( "Calculating travel time for batch #%d [%4d-%4d (plus hospital), %3d records]\n",
                i,
                this_start,
                this_end,
                nrow( this_batch )) )
  
  ### Travel time distance
  this_duration_time_matrix = osrmTable(loc = this_batch)
  
  ### Extract the distances to the hospital
  this_duration_time_to_hospital = this_duration_time_matrix$durations[,as.character( hospital_mask )]
  
  ### Save the distances to the hospital
  duration_time_to_hospital[this_duration_time_to_hospital %>% names %>% as.numeric] = this_duration_time_to_hospital
  
}


### Visual check that this is doing what it should do.

PRIVATE_journey_times.fastest.OSRM = postcode_holder %>%
  mutate( PRIVATE_journey = duration_time_to_hospital )
  
check.sf = st_as_sf(as.data.table(PRIVATE_journey_times.fastest.OSRM),
                    coords = c("longitude", "latitude"), 
                    crs = 4326, agr = "constant")
### Quick plot of data (x,y coordinates) + overlay of various parameters.
### Checking whether the journey duration looks sensible.
plot( check.sf )

save( PRIVATE_journey_times.fastest.OSRM,
      file=sprintf( "dat/03b_PRIVATE-journey-times_n=%d.Rdat",
                    number_of_participants ) )

