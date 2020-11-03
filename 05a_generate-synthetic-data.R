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

map_to_metric = function( x, r_min, r_max, t_min ) {
  x.rescaled = rescale( x, r_min=r_min, r_max=r_max, t_min=-10, t_max=10 ) 
  y = 1-sigmoid( x.rescaled )
  y.rescaled = rescale( y, r_min=-1, r_max=1, t_min=t_min, t_max=1 )
  return( y.rescaled )
}


generate_attendance_data = function( base_var,
                                     trial_vars,
                                     map_vars,
                                     timepoints,
                                     rmin=15, rmax=90, tmin=0 ) {
  
  map_var = base_var %>% str_replace( "RAW.", "MAP." )
  att_var = base_var %>% str_replace( "RAW.", "ATT." )
  
  raw_dat = trial_vars %>% pull( {{ base_var }} )
  map_dat = map_to_metric( raw_dat, r_min=rmin, r_max=rmax, t_min=tmin )
  
  map_vars[base_var] = raw_dat
  map_vars[map_var] = map_dat
  
  trial_vars = trial_vars %>% 
    mutate( p = map_dat )
  
  # ggplot( trial_vars, aes(x=!!sym(base_var),y=p) ) +
  #   geom_point() + ggtitle( base_var )
  
  for( this.t in timepoints ) {
    this.new_v = att_var %>%
      str_replace( "\\.", sprintf(".%s.", this.t) )
    
    this.definition = defDataAdd( varname = this.new_v,
                                  dist    = "binary",
                                  formula = "p" )
    trial_vars = addColumns(this.definition, trial_vars)
    
  }
  
  trial_vars = trial_vars %>% select( -p )
  
  return( list( trial_data  = trial_vars,
                mapping_data = map_vars ) )
  
}

######################
##### Processing #####
######################


set.seed(5482)

load( "dat/04_COMPILED-DATASET.Rdat" )

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
           id = "idnum" )


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



### Generating distance information
public_time_definition = defDataAdd( varname = "RAW.public_time",
                                     dist    = "nonrandom",
                                     formula = "get_public_time(idnum)" )

private_time_definition = defDataAdd( varname = "RAW.private_time",
                                     dist    = "nonrandom",
                                     formula = "get_private_time(idnum)" )

public_time_definition_RANDOM = defDataAdd( varname = "RAW.public_time_random",
                                            dist    = "nonrandom",
                                            formula = "get_public_time_random(idnum)" )

private_time_definition_RANDOM = defDataAdd( varname = "RAW.private_time_random",
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
  keep( ~str_detect(.x, "^RAW\\.") )

map.vars = tibble(
  id.num = trial_data.synthetic %>% pull( idnum )
)

trial_data.synthetic.MODERATE = trial_data.synthetic
map.vars.MODERATE = map.vars

for ( this.raw_var in variables_for_synthesis ) {
  attendance.out = generate_attendance_data( this.raw_var,
                                             trial_data.synthetic.MODERATE,
                                             map.vars.MODERATE,
                                             timepoint.list,
                                             rmin = 15,
                                             rmax = 90,
                                             tmin = 0 )
  
  trial_data.synthetic.MODERATE = attendance.out$trial_data
  map.vars.MODERATE = attendance.out$mapping_data
  
}


trial_data.synthetic.EXTREME = trial_data.synthetic
map.vars.EXTREME = map.vars

for ( this.raw_var in variables_for_synthesis ) {
  attendance.out = generate_attendance_data( this.raw_var,
                                             trial_data.synthetic.EXTREME,
                                             map.vars.EXTREME,
                                             timepoint.list,
                                             rmin = 5,
                                             rmax = 30,
                                             tmin = -0.5 )
  
  trial_data.synthetic.EXTREME = attendance.out$trial_data
  map.vars.EXTREME = attendance.out$mapping_data
  
}



### NB. Any data that we want to include in a dropdown box in the
###     resulting Shiny app should be prefixed with "METRIC_".  Also,
###     make sure that these are readable, as text in the dropdown
###     bow will be derived from the column names.  For example:
###     METRIC_public_distance will be shown as Public distance in 
###     the drop down box.

trial_data.synthetic = trial_data.synthetic %>% 
  mutate( METRIC_public_time  = RAW.public_time,
          METRIC_private_time = RAW.private_time )

save( trial_data.synthetic,
      trial_data.synthetic.EXTREME,
      map.vars.EXTREME,
      trial_data.synthetic.MODERATE,
      map.vars.MODERATE,
      number_of_participants,
      timepoint.list,
      variables_for_synthesis,
      file=sprintf( "dat/05a_SYNTHETIC-DATA_n=%d.Rdat",
                    number_of_participants ) )

