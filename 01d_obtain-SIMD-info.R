library(magrittr)
library(simdr)
library(PostcodesioR)
library(dplyr)

load("dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat")

### Obtained from https://www2.gov.scot/Topics/Statistics/SIMD
SIMD_mapping = read.csv( "dat/SIMD/SIMD 00547017 Postcode-DataZone.csv" )

these_postcodes = postcode_holder %>% filter( group=="Participant")

SIMD_holder = these_postcodes %>%
  select( idnum, postcode ) %>% 
  mutate( SIMD16_Vigintile = NA,
          SIMD16_Decile    = NA, 
          SIMD16_Quintile  = NA )

for ( i in 1:nrow(SIMD_holder) ) {
  
  if ( i %% 50 == 0 ) {
    cat( sprintf( "Obtaining SIMD information for %4d/%4d\n",
                i, number_of_participants ) )
  }
  
  this.postcode = ( SIMD_holder %>% pull( postcode ) )[i]

  this.data = SIMD_mapping %>%
    filter( Postcode == this.postcode ) %>%
    select( SIMD16_Vigintile,
            SIMD16_Decile,
            SIMD16_Quintile)
  
  SIMD_holder = SIMD_holder %>% 
    mutate( SIMD16_Vigintile = ifelse( idnum==i,
                                       this.data$SIMD16_Vigintile,
                                       SIMD16_Vigintile)) %>% 
    mutate( SIMD16_Decile = ifelse( idnum==i,
                                    this.data$SIMD16_Decile,
                                    SIMD16_Decile)) %>% 
    mutate( SIMD16_Quintile = ifelse( idnum==i,
                                      this.data$SIMD16_Quintile,
                                      SIMD16_Quintile))
  
  
  
}


save( SIMD_holder,
      file=sprintf( "dat/01d_SIMD-information_n=%d.Rdat",
                    number_of_participants ) )

