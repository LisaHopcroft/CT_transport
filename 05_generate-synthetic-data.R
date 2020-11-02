library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )

#####################
##### FUNCTIONS #####
#####################

get_public_distance = function ( id ) {
  return( public_distance[id] ) 
}

get_public_time = function ( id ) {
  return( public_time[id] ) 
}


######################
##### Processing #####
######################


set.seed(5482)

load( "dat/00_PREPARATION.Rdat" )
load( "dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat" )

### Random placeholder data for the time being.
### This data will be read in from ArcPro/Traveline output
### once I have it.
public_distance = rnorm( number_of_participants,
                         mean=25,
                         sd=8 )
public_time = public_distance + rnorm( number_of_participants,
                                       mean=30,
                                       sd=5 )

#####################################################################
### Initialise the data definition object                         ###
#####################################################################

trial_data.definition = 
  ### Defining the arm variable
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

### NB. Any data that we want to include in a dropdown box in the
###     resulting Shiny app should be prefixed with "METRIC_".  Also,
###     make sure that these are readable, as text in the dropdown
###     bow will be derived from the column names.  For example:
###     METRIC_public_distance will be shown as Public distance in 
###     the drop down box.

### Generating distance information
public_distance_definition = defDataAdd( varname = "METRIC_public_distance",
                                         dist    = "nonrandom",
                                         formula = "get_public_distance(idnum)" )

### Generating distance information
public_time_definition = defDataAdd( varname = "METRIC_public_time",
                                     dist    = "nonrandom",
                                     formula = "get_public_time(idnum)" )

### Let's say we know that the relationship between
### distance (x axis) and the likelihood of attendance
### corresponds to 1-sigmoidal function (for example).
plot( -10:10, 1-sigmoid(-10:10) )

### Could we rescale the data to correspond to values
### between -10 and 10 and then use the 1-sigmoid
### function to define the binary distributions from
### which to sample our attendance data?

trial_data.synthetic.LOADED = addColumns(public_distance_definition,
                                         trial_data.synthetic.STATIC)
trial_data.synthetic.LOADED = addColumns(public_time_definition,
                                         trial_data.synthetic.LOADED)
### Rescaling

rescale = function(x,
                   r_min = 5,
                   r_max = 50,
                   t_min = -10,
                   t_max = 10 ) {
  return( ((x-r_min)/(r_max-r_min)) * (t_max-t_min) + t_min )
}

### Rescale the distance  
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( public_distance_rescaled = rescale( public_distance ) )

### Calculate the resulting probability
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( parameter = 1-sigmoid(public_distance_rescaled) )

### Rescale the probability
trial_data.synthetic.LOADED = trial_data.synthetic.LOADED %>%
  mutate( parameter_rescaled = rescale(parameter,
                                       r_min=-1,r_max=1,t_min=0,t_max=1) ) 

### Some plots just to reassure me that the right things are
### happening.

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=public_distance,
            y=public_distance_rescaled)) +
  geom_point() +
  xlab( "original distance" ) +
  ylab( "scaled distance" ) +
  theme_bw()

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=public_distance,
            y=parameter)) +
  geom_point() +
  xlab( "original distance" ) +
  ylab( "binary parameter" ) +
  theme_bw()

ggplot( data=trial_data.synthetic.LOADED,
        aes(x=public_distance_rescaled,
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
             y = public_distance )) + geom_boxplot()

ggplot( data=trial_data.synthetic.LOADED,
        aes( x = attendance_T2_STATIC.f,
             y = public_distance )) + geom_boxplot()

save( trial_data.synthetic.LOADED,
      trial_data.synthetic.STATIC,
      file=sprintf( "dat/03_SYNTHETIC-DATA_n=%d.Rdat",
                    number_of_participants ) )

