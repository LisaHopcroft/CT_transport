library( simstudy )
library( sigmoid )

#####################
##### FUNCTIONS #####
#####################

get_patient_distance = function ( id ) {
  return( patient_distance[id] ) 
}

######################
##### Processing #####
######################


set.seed(5482)

load( "dat/20_random_postcodes.Rdat" )

patient_distance = rnorm( number_of_participants,
                          mean=25,
                          sd=8 )

### Needing to add the attendance data now

# trial_data.definition = 
#   ### Defining the arm
#   defData( varname = "ARM_categorical",
#            dist = "categorical",
#            formula = catProbs(0.50,0.50),
#            id = "idnum")

### Defining the site
# defData( varname = "Site",
#          dist = "nonrandom",
#          formula = "RAH")

### Defining the attendance
### - this could just be random
### - or could be dependent on the distance from the site
### 
### tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
### 

### Placeholder for distance
# defData( varname = "distance",
#         dist = "gamma",
#         formula = 10,
#         variance = 1 ) %>% 

### Test incorporating functions
# defData( varname = "test",
#          dist = "nonrandom",
#          formula = "sqrt(distance)") #%>% 
# defData( varname = "identification",
#          dist = "categorical",
#          formula = catProbs(rep.int( 1/num_patients,times=num_patients) )) %>% 
# defData( varname = "test2",
#          dist = "nonrandom",
#          formula = "get_patient_info(idnum)" )


trial_data.definition = 
  ### Defining the arm
  defData( varname = "arm",
           dist = "categorical",
           formula = catProbs(0.50,0.50),
           id = "idnum")

#####################################################################
### Adding STATIC attendance data ###################################
#####################################################################

trial_data.definition = trial_data.definition %>% 
  defData(varname = "attendance_T0_STATIC", dist = "binary", formula = 1  ) %>% 
  defData(varname = "attendance_T1_STATIC", dist = "binary", formula = 0.9) %>% 
  defData(varname = "attendance_T2_STATIC", dist = "binary", formula = 0.8)

trial_data.synthetic.STATIC = genData( number_of_participants,
                                       trial_data.definition )

#####################################################################
### Generate attendance data based on distance ######################
#####################################################################

### Generating distance information
distance_definition = defDataAdd( varname = "distance",
                                  dist    = "nonrandom",
                                  formula = "get_patient_distance(idnum)" )

### Let's say we know that the relationship between
### distance (x axis) and the likelihood of attendance
### corresponds to 1-sigmoidal function (for example).
plot( -10:10, 1-sigmoid(-10:10) )

### Could we rescale the data to correspond to values
### between -10 and 10 and then use the 1-sigmoid
### function to define the binary distributions from
### which to sample our attendance data?

trial_data.synthetic.LOADED = addColumns(distance_definition,
                                         trial_data.synthetic.STATIC)
### Rescaling

# r_min = 5
# r_max = 50
# t_min = -10
# t_max = 10
# m = trial_data.synthetic.LOADED$distance

rescale = function(x,
                   r_min = 5,
                   r_max = 50,
                   t_min = -10,
                   t_max = 10 ) {
  return( ((x-r_min)/(r_max-r_min)) * (t_max-t_min) + t_min )
}

### Rescale the distance  
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( distance_rescaled = rescale( distance ) )

### Calculate the resulting probability
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( parameter = 1-sigmoid(distance_rescaled) )

### Rescale the probability
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( parameter_rescaled = rescale(parameter,
                                       r_min=-1,r_max=1,t_min=0,t_max=1) ) 


ggplot( data=trial_data.synthetic.LOADED,
        aes(x=distance,
            y=distance_rescaled)) +
  geom_point() +
  xlab( "original distance" ) +
  ylab( "scaled distance" ) +
  theme_bw()

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=distance,
            y=parameter)) +
  geom_point() +
  xlab( "original distance" ) +
  ylab( "binary parameter" ) +
  theme_bw()

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=distance_rescaled,
            y=parameter)) +
  geom_point() +
  xlab( "scaled distance" ) +
  ylab( "binary parameter" ) +
  ylim(0,1) +
  theme_bw()

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=parameter,
            y=parameter_rescaled)) +
  geom_point() +
  xlab( "binary parameter" ) +
  ylab( "binary parameter rescaled" ) +
  xlim(0,1) +
  ylim(0,1) +
  theme_bw()

attendance_T0_definition = defDataAdd( varname = "attendance_T0",
                                    dist    = "binary",
                                    formula = "parameter_rescaled" )
trial_data.synthetic.LOADED = addColumns(attendance_T0_definition,
                                         trial_data.synthetic.LOADED)

attendance_T1_definition = defDataAdd( varname = "attendance_T1",
                                       dist    = "binary",
                                       formula = "parameter_rescaled" )
trial_data.synthetic.LOADED = addColumns(attendance_T1_definition,
                                         trial_data.synthetic.LOADED)

attendance_T2_definition = defDataAdd( varname = "attendance_T2",
                                       dist    = "binary",
                                       formula = "parameter_rescaled" )
trial_data.synthetic.LOADED = addColumns(attendance_T2_definition,
                                         trial_data.synthetic.LOADED)

trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>% 
  mutate( attendance_T0.f = factor( attendance_T0) ) %>% 
  mutate( attendance_T1.f = factor( attendance_T1) ) %>% 
  mutate( attendance_T2.f = factor( attendance_T2) ) %>% 
  mutate( attendance_T0_STATIC.f = factor( attendance_T0_STATIC) ) %>% 
  mutate( attendance_T1_STATIC.f = factor( attendance_T1_STATIC) ) %>% 
  mutate( attendance_T2_STATIC.f = factor( attendance_T2_STATIC) ) 
  

ggplot( data=trial_data.synthetic.LOADED,
        aes( x = attendance_T2.f,
             y = distance )) + geom_boxplot()

ggplot( data=trial_data.synthetic.LOADED,
        aes( x = attendance_T2_STATIC.f,
             y = distance )) + geom_boxplot()

### Questions for Thanos
### --------------------
### (1) How do I include a second variable when drawing from the
###     binary distribution (variable could be TRUE/FALSE) or
###     ordinal (SIMD level).
### (2) Informing the relationship between distance from trial
###     centre and the participant - how to choose the relationship?
###     (NB. I will look in the literature).
### (3) Including postcode boundaries in leaflet - would like to try
###     different presentations (e.g. hex)



