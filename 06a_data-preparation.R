library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )
library( cowplot )
library( tidyr )
library( lubridate )
# library( tibble )

load( "dat/05a_SYNTHETIC-DATA_n=1958.Rdat" )

n.timepoints = timepoint.list %>% length

generate_additional_variables = function(d) {
  d.new = d %>% 
    group_by(idnum) %>%
    ####################################################################
    ### NUMBER OF APPOINTMENTS
    ####################################################################
  
  ### Public biased
  mutate( num_appointments_attended.public =
            sum( ATT.T0.public_time,
                 ATT.T1.public_time,
                 ATT.T2.public_time,
                 ATT.T3.public_time,
                 ATT.T4.public_time
            )) %>%
    ### Public random
    mutate( num_appointments_attended.public_RANDOM =
              sum( ATT.T0.public_time_RANDOM,
                   ATT.T1.public_time_RANDOM,
                   ATT.T2.public_time_RANDOM,
                   ATT.T3.public_time_RANDOM,
                   ATT.T4.public_time_RANDOM
              )) %>% 
    ### Private biased
    mutate(num_appointments_attended.private =
             sum( ATT.T0.private_time,
                  ATT.T1.private_time,
                  ATT.T2.private_time,
                  ATT.T3.private_time,
                  ATT.T4.private_time
             )) %>%
    ### Private random
    mutate( num_appointments_attended.private_RANDOM =
              sum( ATT.T0.private_time_RANDOM,
                   ATT.T1.private_time_RANDOM,
                   ATT.T2.private_time_RANDOM,
                   ATT.T3.private_time_RANDOM,
                   ATT.T4.private_time_RANDOM
              )) %>% 
    ####################################################################
  ### PERENTAGE OF APPOINTMENTS
  ####################################################################
  
  mutate( perc_appointments_attended.public = num_appointments_attended.public/n.timepoints,
          perc_appointments_attended.public_RANDOM = num_appointments_attended.public_RANDOM/n.timepoints,
          perc_appointments_attended.private = num_appointments_attended.private/n.timepoints,
          perc_appointments_attended.private_RANDOM = num_appointments_attended.private_RANDOM/n.timepoints
  ) %>%
    
    ####################################################################
  ### DROP OUT
  ####################################################################
  
  mutate( dropout_appointment.public = (DO.public_time %>% 
                                          str_replace( "ATT\\.T", "") %>%
                                          str_replace( "\\.public_time", "") %>%
                                          as.numeric() %>% 
                                          tidyr::replace_na(5) ) ) %>% 
    
    mutate( dropout_appointment.public_RANDOM = (DO.public_time_RANDOM %>% 
                                                   str_replace( "ATT\\.T", "") %>%
                                                   str_replace( "\\.public_time_RANDOM", "") %>%
                                                   as.numeric() %>% 
                                                   tidyr::replace_na(5) ) ) %>% 
    
    mutate( dropout_appointment.private = (DO.private_time %>% 
                                             str_replace( "ATT\\.T", "") %>%
                                             str_replace( "\\.private_time", "") %>%
                                             as.numeric() %>% 
                                             tidyr::replace_na(5) ) ) %>% 
    
    mutate( dropout_appointment.private_RANDOM = (DO.private_time_RANDOM %>% 
                                                    str_replace( "ATT\\.T", "") %>%
                                                    str_replace( "\\.private_time_RANDOM", "") %>%
                                                    as.numeric() %>% 
                                                    tidyr::replace_na(5) ) )
  
  return( d.new )
}

EXTREME_d  = generate_additional_variables( trial_data.synthetic.EXTREME.DO  )
MODERATE_d = generate_additional_variables( trial_data.synthetic.MODERATE.DO )

subsets_to_generate = tribble(
  ~dataset_type, ~att_string   , ~calculatedvars_string, ~new_name                ,
  "EXTREME"    , ".public_time" , ".public"              , "EXTREME_PUBLIC_BIAS.d"  ,
  "EXTREME"    , ".private_time", ".private"             , "EXTREME_PRIVATE_BIAS.d" ,
  "MODERATE"   , ".public_time" , ".public"              , "MODERATE_PUBLIC_BIAS.d" ,
  "MODERATE"   , ".private_time", ".private"             , "MODERATE_PRIVATE_BIAS.d",
  "EXTREME"    , ".public_time_RANDOM", ".public_RANDOM" , "NEUTRAL.d"
)

remove_text = function(x,s) {
  return( str_remove( x, s ) )
}

for ( i in 1:nrow( subsets_to_generate ) ) {
  
  this.dataset_type = ( subsets_to_generate %>% pull( dataset_type ) ) [i]
  this.att_string = ( subsets_to_generate %>% pull( att_string ) ) [i]
  this.calculatedvars_string = ( subsets_to_generate %>% pull( calculatedvars_string ) ) [i]
  this.new_name = ( subsets_to_generate %>% pull( new_name ) ) [i]
  
  cat( sprintf( "Generating %s...", this.new_name) )
  
  this.d = get( sprintf( "%s_d", this.dataset_type ) ) %>% 
    select( idnum, DOR, arm, gender, age, has_car,
            starts_with("SIMD16"),
            starts_with("RAW"),
            matches( sprintf( "^ATT\\..*%s$",this.att_string), perl=TRUE ),
            matches( sprintf("^num.*%s$",this.calculatedvars_string), perl=TRUE ),
            matches( sprintf("^perc.*%s$",this.calculatedvars_string), perl=TRUE ) ) %>%
    rename_at( vars( starts_with("ATT") ),
               ~remove_text(.,this.att_string) ) %>% 
    rename_at( vars( starts_with("num") ),
               ~remove_text(.,this.calculatedvars_string) ) %>% 
    rename_at( vars( starts_with("perc") ),
               ~remove_text(.,this.calculatedvars_string) ) %>% 
  mutate( DATE.T0 = DOR + weeks(1) ) %>% 
  mutate( DATE.T1 = ifelse( ATT.T1==1, DOR + months( 1), NA) ) %>% 
  mutate( DATE.T2 = ifelse( ATT.T2==1, DOR + months( 3), NA) ) %>% 
  mutate( DATE.T3 = ifelse( ATT.T3==1, DOR + months( 6), NA) ) %>% 
  mutate( DATE.T4 = ifelse( ATT.T4==1, DOR + months(12), NA) )
  
  this.complete_mask = complete.cases( this.d %>% select( -starts_with("DATE")))
  
  assign( this.new_name, this.d[this.complete_mask,] )
  
  this.n = this.complete_mask %>% sum
  
  cat( sprintf( "done (%d entries).\n", this.n ) )

}

number_of_participants = nrow( EXTREME_PUBLIC_BIAS.d )

# Randomize the data in neutral.
NEUTRAL.d.RANDOM = NEUTRAL.d %>% 
  ungroup() %>% 
  mutate( age = sample( age ) ) %>% 
  mutate( RAW.public_time  = sample( RAW.public_time  ) ) %>% 
  mutate( RAW.private_time = sample( RAW.private_time ) ) %>% 
  mutate( RAW.distance     = sample( RAW.distance ) )

save( trial_data.synthetic,
      EXTREME_PUBLIC_BIAS.d,
      EXTREME_PRIVATE_BIAS.d,
      MODERATE_PUBLIC_BIAS.d,
      MODERATE_PRIVATE_BIAS.d,
      NEUTRAL.d,
      NEUTRAL.d.RANDOM,
      number_of_participants,
      timepoint.list,
      file=sprintf( "dat/06a_LEARNING-DATASET_n=%d_NEW.Rdat",
                    number_of_participants ) )
