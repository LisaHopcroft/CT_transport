library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )

#####################
##### FUNCTIONS #####
#####################

get_public_time = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(public_time.BJP) ) 
}

get_private_time = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(private_time.ArcPro) ) 
}

get_public_time_random = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(public_time.RANDOM) ) 
}

get_private_time_random = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(private_time.RANDOM) ) 
}


rescale = function(x,
                   r_min,
                   r_max,
                   t_min,
                   t_max ) {
  return( ((x-r_min)/(r_max-r_min)) * (t_max-t_min) + t_min )
}

map_to_metric = function( x, r_min, r_max ) {
  x.rescaled = rescale( x, r_min=r_min, r_max=r_max, t_min=-10, t_max=10 ) 
  y = 1-sigmoid( x.rescaled )
  y.rescaled = rescale( y, r_min=-1, r_max=1, t_min=0, t_max=1 )
  return( y.rescaled )
}


# get_public_time = function ( id, d) {
#   return( public_time[id] ) 
# }


######################
##### Processing #####
######################


set.seed(5482)

load( "dat/04_COMBILED-DATASET.Rdat" )

number_of_participants = PATIENT.DATA %>% nrow 
timepoint.list = sprintf( "T%d", 0:4 ) 

### ADDING A PERMUTED DATASET

PATIENT.DATA$private_time.RANDOM = PATIENT.DATA %>% pull( private_time.ArcPro ) %>% sample
PATIENT.DATA$public_time.RANDOM = PATIENT.DATA %>% pull( public_time.BJP ) %>% sample


# ### Random placeholder data for the time being.
# ### This data will be read in from ArcPro/Traveline output
# ### once I have it.
# public_distance = rnorm( number_of_participants,
#                          mean=25,
#                          sd=8 )
# public_time = public_distance + rnorm( number_of_participants,
#                                        mean=30,
#                                        sd=5 )

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

# trial_data.definition = trial_data.definition %>% 
#   defData(varname = "attendance_T0_STATIC", dist = "binary", formula = 1  ) %>% 
#   defData(varname = "attendance_T1_STATIC", dist = "binary", formula = 0.9) %>% 
#   defData(varname = "attendance_T2_STATIC", dist = "binary", formula = 0.8)

trial_data.synthetic = genData( number_of_participants,
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
# public_distance_definition = defDataAdd( varname = "METRIC_public_distance",
#                                          dist    = "nonrandom",
#                                          formula = "get_public_distance(idnum)" )

### Generating distance information
public_time_definition = defDataAdd( varname = "IN.public_time",
                                     dist    = "nonrandom",
                                     formula = "get_public_time(idnum)" )

private_time_definition = defDataAdd( varname = "IN.private_time",
                                     dist    = "nonrandom",
                                     formula = "get_private_time(idnum)" )

public_time_definition_RANDOM = defDataAdd( varname = "IN.public_time_random",
                                            dist    = "nonrandom",
                                            formula = "get_public_time_random(idnum)" )

private_time_definition_RANDOM = defDataAdd( varname = "IN.private_time_random",
                                             dist    = "nonrandom",
                                             formula = "get_private_time_random(idnum)" )

### Let's say we know that the relationship between
### distance (x axis) and the likelihood of attendance
### corresponds to 1-sigmoidal function (for example).
# plot( -10:10, 1-sigmoid(-10:10) )

### Could we rescale the data to correspond to values
### between -10 and 10 and then use the 1-sigmoid
### function to define the binary distributions from
### which to sample our attendance data?


trial_data.synthetic = addColumns(public_time_definition ,trial_data.synthetic)
trial_data.synthetic = addColumns(private_time_definition,trial_data.synthetic)
trial_data.synthetic = addColumns(public_time_definition_RANDOM ,trial_data.synthetic)
trial_data.synthetic = addColumns(private_time_definition_RANDOM,trial_data.synthetic)

variables_for_synthesis = 
  trial_data.synthetic %>%
  colnames %>%
  keep( ~str_detect(.x, "^IN\\.") )

# for ( this.v in variables_for_synthesis ) {
#   this.d = trial_data.synthetic %>% pull( sym(this.v) )
#   
#   trial_data.synthetic = trial_data.synthetic %>%
#     mutate( v.rescaled = map_to_metric( this.d, r_min=15, r_max=90 ) )
#   
#   for( this.timepoint in timepoint.list ) {
#     
#     this.new_column = this.v %>% 
#       str_replace( "IN\\.",
#                    sprintf( "attendance_%s.", this.timepoint ) )
#     
#     this.definition = defDataAdd( varname = this.new_column,
#                                   dist    = "binary",
#                                   formula = "v.rescaled" )
#     
#     trial_data.synthetic = addColumns(this.definition,
#                                       trial_data.synthetic)
#   }
#     
#   
# }

### Rescale the distance
# trial_data.synthetic = trial_data.synthetic %>%
#   mutate( METRIC_public_time_rescaled = rescale( METRIC_public_time ) )
# 
# ### Calculate the resulting probability
# trial_data.synthetic = trial_data.synthetic %>%
#   mutate( parameter = 1-sigmoid(METRIC_public_time_rescaled) )
# 
# ### Rescale the probability
# trial_data.synthetic = trial_data.synthetic %>%
#   mutate( parameter_rescaled = rescale(parameter,
#                                        r_min=-1,r_max=1,t_min=0,t_max=1) ) 

trial_data.synthetic = trial_data.synthetic %>%
  mutate( p = map_to_metric(IN.public_time,r_min=15,r_max=90) )

ggplot( trial_data.synthetic, aes(x=IN.public_time,y=p) ) + geom_point()

for( this.timepoint in timepoint.list ) {
  this.new_v = "IN.public_time" %>% 
          str_replace( "IN\\.",
                       sprintf( "attendance_%s.", this.timepoint ) )
  
  this.definition = defDataAdd( varname = this.new_v,
                                dist    = "binary",
                                formula = "p" )
  trial_data.synthetic = addColumns(this.definition,
                                    trial_data.synthetic)
}

### Need to do this for all "IN." variables.
### Do this using an extremely loaded dataset and moderately loaded dataset.





###
### Some plots just to reassure me that the right things are
### happening.
# 
# ggplot( data=trial_data.synthetic,
#         aes(x=METRIC_public_time,
#             y=METRIC_public_time_rescaled)) +
#   geom_point() +
#   xlab( "original distance" ) +
#   ylab( "scaled distance" ) +
#   theme_bw()
# 
# ggplot( data=trial_data.synthetic,
#         aes(x=METRIC_public_time,
#             y=parameter)) +
#   geom_point() +
#   xlab( "original distance" ) +
#   ylab( "binary parameter" ) +
#   theme_bw()
# 
# ggplot( data=trial_data.synthetic,
#         aes(x=METRIC_public_time_rescaled,
#             y=parameter)) +
#   geom_point() +
#   xlab( "scaled distance" ) +
#   ylab( "binary parameter" ) +
#   ylim(0,1) +
#   theme_bw()
# 
# ggplot( data=trial_data.synthetic,
#         aes(x=parameter,
#             y=parameter_rescaled)) +
#   geom_point() +
#   xlab( "binary parameter" ) +
#   ylab( "binary parameter rescaled" ) +
#   xlim(0,1) +
#   ylim(0,1) +
#   theme_bw()


# trial_data.synthetic = trial_data.synthetic %>% 
#   mutate( attendance_T0.f = factor( attendance_T0) ) %>% 
#   mutate( attendance_T1.f = factor( attendance_T1) ) %>% 
#   mutate( attendance_T2.f = factor( attendance_T2) ) %>% 
#   mutate( attendance_T0_STATIC.f = factor( attendance_T0_STATIC) ) %>% 
#   mutate( attendance_T1_STATIC.f = factor( attendance_T1_STATIC) ) %>% 
#   mutate( attendance_T2_STATIC.f = factor( attendance_T2_STATIC) ) 
#   
# 
# ggplot( data=trial_data.synthetic,
#         aes( x = attendance_T2.f,
#              y = public_distance )) + geom_boxplot()
# 
# ggplot( data=trial_data.synthetic,
#         aes( x = attendance_T2_STATIC.f,
#              y = public_distance )) + geom_boxplot()

save( trial_data.synthetic,
      file=sprintf( "dat/05_SYNTHETIC-DATA_n=%d.Rdat",
                    number_of_participants ) )

