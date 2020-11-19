library( dplyr )
library( magrittr )
library( ggplot2 )
library( GGally )
library( plyr )
library( tidyr )
library( ggbeeswarm )
### conda install -c conda-forge r-matrix 
### conda install -c r r-mgcv 
### Sys.setenv(PKG_CONFIG_PATH = "/../miniconda3/pkgs/r-base-3.6.1-haffb61f_2/lib/R/lib")
### install.packages("mgcv")
### To get this working, required a reinstall of R packages
library( mgcv )
library( cowplot )

# load( "dat/00_PREPARATION.Rdat" )
load( "dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat" )
load( "dat/02b_PUBLIC-journey-times_n=2000.Rdat" )
load( "dat/03a_PRIVATE-journey-times_n=2000.Rdat" )
load( "dat/03b_PRIVATE-journey-times_n=2000.Rdat" )
load( "dat/01d_SIMD-information_n=2000.Rdat" )

PRIVATE_journey_times.fastest.ArcPro %>% nrow
PRIVATE_journey_times.fastest.OSRM %>% nrow
PUBLIC_journey_times.fastest %>% nrow

PUBLIC_journey_times.fastest.BJP = PUBLIC_journey_times.fastest %>% 
  ungroup() %>% 
  mutate( idnum = as.numeric( processing_number ) ) %>% 
  mutate( public_time.BJP = 60 * minimum_journey_time ) %>% 
  inner_join( postcode_holder %>% select( idnum, postcode ) ) %>% 
  select( -origin ) %>% 
  dplyr::rename( origin = postcode ) %>% 
  select( origin, public_time.BJP ) %>% 
  unique()

PRIVATE_journey_times.fastest.ArcPro = PRIVATE_journey_times.fastest.ArcPro %>% 
  dplyr::rename( idnum = OBJECTID ) %>% 
  dplyr::rename( private_time.ArcPro = Total_Drive ) %>% 
  dplyr::mutate( Total_Length_km = Total_Length/1000) %>% 
  dplyr::rename( private_distance.ArcPro = Total_Length_km ) %>% 
  separate( Name, c("origin", "destination"), sep=" - " ) %>% 
  select( origin, private_time.ArcPro, private_distance.ArcPro ) %>% 
  unique()
  
PRIVATE_journey_times.fastest.OSRM = PRIVATE_journey_times.fastest.OSRM %>% 
  dplyr::rename( private_time.OSRM = PRIVATE_journey ) %>% 
  dplyr::rename( origin = postcode ) %>% 
  select( origin, private_time.OSRM ) %>% 
  unique()


TRAVEL.data = PUBLIC_journey_times.fastest.BJP %>%
  inner_join( PRIVATE_journey_times.fastest.ArcPro, by="origin" ) %>%
  inner_join( PRIVATE_journey_times.fastest.OSRM, by="origin" ) %>%
  dplyr::rename( postcode = origin )


### Correlation between travel time from ArcPro and OSRM




upper_limit = max( TRAVEL.data$private_time.ArcPro,
     TRAVEL.data$private_time.OSRM ) %>% round_any( 10 )

ArcPro_OSRM.correlation = cor( TRAVEL.data$private_time.ArcPro,
                               TRAVEL.data$private_time.OSRM )

ggplot( TRAVEL.data,
        aes(x=private_time.ArcPro,
            y=private_time.OSRM)) +
  geom_abline( intercept = 0 , slope=1, col="blue",linetype="dashed") +
  geom_point() +
  coord_fixed() +
  xlim( 0, upper_limit ) +
  ylim( 0, upper_limit ) +
  xlab( "Drive time (ArcPro)" ) +
  ylab( "Drive time (OSRM)" ) +
  annotate( "text",
            x=0, y=upper_limit,
            label = sprintf( "r=%.2f", ArcPro_OSRM.correlation ),
            hjust=0, vjust=1 ) +
  labs( title="Drive time (ArcPro v OSRM)",
        subtitle="Identity indicated by dashed blue line")


### Correlation between travel time and travel distance

upper_limit = max( TRAVEL.data$private_distance.ArcPro ) %>% round_any( 5 )

ArcPro_time_distance.correlation = cor( TRAVEL.data$private_time.ArcPro,
                               TRAVEL.data$private_distance.ArcPro )

ggplot( TRAVEL.data,
        aes(x=private_time.ArcPro,
            y=private_distance.ArcPro)) +
  geom_point() +
  geom_smooth(method="lm") +
  xlab( "Drive time (ArcPro)" ) +
  ylab( "Drive distance (ArcPro)" ) +
  annotate( "text",
            x=0, y=upper_limit,
            label = sprintf( "r=%.2f", ArcPro_time_distance.correlation ),
            hjust=0, vjust=1 ) +
  labs( title="Drive time ve distance (ArcPro)",
        subtitle="Identity indicated by dashed blue line")

### Correlation between private travel time and public travel time

upper_limit = max( TRAVEL.data$private_time.ArcPro,
                   TRAVEL.data$public_time.BJP ) %>% round_any( 10 )

ArcPro_BJP.correlation = cor( TRAVEL.data$private_time.ArcPro,
                               TRAVEL.data$public_time.BJP )

ggplot( TRAVEL.data,
        aes(x=private_time.ArcPro,
            y=public_time.BJP)) +
  geom_abline( intercept = 0 , slope=1, col="blue",linetype="dashed") +
  geom_point() +
  coord_fixed() +
  xlim( 0, upper_limit ) +
  ylim( 0, upper_limit ) +
  xlab( "Drive time (ArcPro)" ) +
  ylab( "Public transport time (BJP)" ) +
  annotate( "text",
            x=0, y=upper_limit,
            label = sprintf( "r=%.2f", ArcPro_BJP.correlation ),
            hjust=0, vjust=1 ) +
  labs( title="Travel time (ArcPro v BJP)",
        subtitle="Identity indicated by dashed blue line")

PATIENT.DATA = postcode_holder %>%
  inner_join( TRAVEL.data, by="postcode" ) %>% 
  inner_join( SIMD_holder, by=c("postcode","idnum") )

PATIENT.DATA = PATIENT.DATA %>%
  mutate( id = 1:nrow( PATIENT.DATA )) %>% 
  dplyr::rename( original_id = idnum ) %>% 
  select( id, original_id, everything() )

psych::describe( PATIENT.DATA %>% select( public_time.BJP, private_time.ArcPro, private_time.OSRM ) )

PATIENT.DATA.TRAVEL_boxplot_data = PATIENT.DATA %>% 
  select( id, public_time.BJP, private_time.ArcPro, private_time.OSRM ) %>% 
  pivot_longer( -id,
                names_to="source",
                values_to="minutes")

ggplot( PATIENT.DATA.TRAVEL_boxplot_data,
        aes( x=source,
             y=minutes )) +
  geom_violin()


SIMD16_Decile.barplot = ggplot( PATIENT.DATA,
        aes( x=SIMD16_Decile )) +
  geom_histogram() +
  scale_x_continuous(breaks=1:10) 


SIMD16_Quintile.barplot = ggplot( PATIENT.DATA,
                                aes( x=SIMD16_Quintile )) +
  geom_histogram() +
  scale_x_continuous(breaks=1:5) 

SIMD16_Vigintile.barplot = ggplot( PATIENT.DATA,
                                aes( x=SIMD16_Vigintile )) +
  geom_histogram() +
  scale_x_continuous(breaks=1:20) 



save( PATIENT.DATA,
      file="dat/04_COMPILED-DATASET.Rdat" )
