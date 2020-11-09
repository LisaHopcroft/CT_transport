library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )
library( cowplot )
library( tidyr )
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

save( trial_data.synthetic,
      EXTREME_d,
      MODERATE_d,
      number_of_participants,
      timepoint.list,
      file=sprintf( "dat/06a_LEARNING-DATASET_n=%d.Rdat",
                    number_of_participants ) )
