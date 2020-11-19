library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )
library( lubridate )

#####################
##### FUNCTIONS #####
#####################

get_public_time = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(public_time.BJP) ) 
}

get_private_time = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(private_time.ArcPro) ) 
}

get_distance = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(private_distance.ArcPro) ) 
}
 
get_SIMD16_Decile = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(SIMD16_Decile) )
}

get_SIMD16_Quintile = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(SIMD16_Quintile) )
}

get_SIMD16_Vigintile = function ( id ) {
  return( PATIENT.DATA %>% filter( id == id ) %>% pull(SIMD16_Vigintile) )
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
                                     modify_mask = NULL,
                                     modify_val,
                                     rmin=15, rmax=90, tmin=0 ) {
  
  map_var = base_var %>% str_replace( "RAW.", "MAP." )
  att_var = base_var %>% str_replace( "RAW.", "ATT." )
  
  raw_dat = trial_vars %>% pull( {{ base_var }} )
  map_dat = map_to_metric( raw_dat, r_min=rmin, r_max=rmax, t_min=tmin )
  
  map_vars[base_var] = raw_dat
  map_vars[map_var] = map_dat
  
  trial_vars = trial_vars %>% 
    mutate( p = map_dat )
  
  if ( !is.null( modify_mask ) ) {
    ### Need to chose the max so that we don't end up with values
    ### less than 0.
    trial_vars$p[ modify_mask ] = max(c(trial_vars$p[ modify_mask ] + modify_val,
                                        0))
  }
  
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


add_dropouts = function( d,
                         trajectory,
                         target_perc = 0.10,
                         verbose = FALSE ) {
  # d = trial_data.synthetic.MODERATE
  # trajectory = colnames( trial_data.synthetic.MODERATE %>% select( ends_with( "public_time")) %>% select( starts_with("ATT")))  
  
  n_updates_made = 0
  
  this.focus = d %>% select( idnum, {{trajectory}} )
  
  total_n = this.focus %>% nrow  
  dropout_n = ( total_n * target_perc ) %>% plyr::round_any(1)
  
  these.dropouts.sample = sample( this.focus %>% pull( idnum ),
                                  dropout_n,
                                  replace=FALSE )
  
  mutate_holder = rep.int( NA, nrow(d) )
  
  for( i in these.dropouts.sample ) {
    this.idnum = these.dropouts.sample[i]
    this.dropout_from_here = sample(trajectory,1)
    
    if ( verbose ) {
      cat( this.dropout_from_here, "\n" )
      cat( this.idnum, "\n" )
    }
    
    these.tochange = trajectory %>% purrr::keep( ~ .x >= this.dropout_from_here )
    
    row.i = which( d$idnum==this.idnum )
    
    mutate_holder[ row.i ] = this.dropout_from_here
    
    for ( col.name in these.tochange ) {
      if ( verbose ) cat( sprintf( " +" ) ) 
      # cat( sprintf( "[%s] pre  = %d\n", col.name, d[[col.name]][row.i]  ))
      d[[col.name]][row.i] = 0
      # cat( sprintf( "[%s] post = %d\n", col.name, d[[col.name]][row.i]  ))
      n_updates_made = n_updates_made + 1
    }
    
    if ( verbose ) cat( sprintf("\n") )
    
  }
  
  cat( sprintf( "In adding dropouts, %d updates made.\n",
                n_updates_made ) )
  d$DROPOUT = mutate_holder
  
  return( d )
  
}

######################
##### Processing #####
######################


set.seed(5482)

load( "dat/04_COMPILED-DATASET.Rdat" )

number_of_participants = PATIENT.DATA %>% nrow 
timepoint.list = sprintf( "T%d", 0:4 ) 

### ADDING A PERMUTED DATASET
# PATIENT.DATA$private_time.RANDOM = PATIENT.DATA %>% pull( private_time.ArcPro ) %>% sample
# PATIENT.DATA$public_time.RANDOM = PATIENT.DATA %>% pull( public_time.BJP ) %>% sample

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
  ### Defining the gender variable
  defData( varname = "gender",
           dist = "categorical",
           formula = genCatFormula(0.50,0.50),
           id = "idnum" ) %>% 
  ### Defining the car variable
  ### https://www.gov.scot/publications/scotlands-people-results-2015-scottish-household-survey/pages/8/
  defData( varname = "has_car",
           dist = "categorical",
           formula = genCatFormula(0.70,0.30) ) %>% 
  ### Defining the age variable
  defData( varname = "age",
           dist = "uniform",
           formula = "18;80") 



#####################################################################
### Adding STATIC attendance data ###################################
#####################################################################

# trial_data.definition = trial_data.definition %>% 
#   defData(varname = "attendance_T0_STATIC", dist = "binary", formula = 1  ) %>% 
#   defData(varname = "attendance_T1_STATIC", dist = "binary", formula = 0.9) %>% 
#   defData(varname = "attendance_T2_STATIC", dist = "binary", formula = 0.8)

trial_data.synthetic = genData( number_of_participants,
                                       trial_data.definition )

### Defining the arm variable
trial_data.synthetic = trial_data.synthetic %>% 
  trtAssign( nTrt = 2,
             balanced = TRUE,
             grpName = "arm" )

### Updating gender to Female/Male
trial_data.synthetic = trial_data.synthetic %>% 
  mutate( gender = recode( gender,
                           "1"="Female",
                           "2"="Male") )

### Updating has_car to Yes/No
trial_data.synthetic = trial_data.synthetic %>% 
  mutate( has_car = recode( has_car,
                           "1"="Yes",
                           "2"="No") )

### Adding a random start date
date_range = seq( ymd("2018-01-01"),
                  ymd("2019-12-31"), by="day")
trial_data.synthetic = trial_data.synthetic %>% 
  mutate( DOR = sample(date_range,
                       size=number_of_participants,
                       replace=TRUE) )


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

distance_definition = defDataAdd( varname = "RAW.distance",
                                      dist    = "nonrandom",
                                      formula = "get_distance(idnum)" )

SIMD16_Vigintile_definition = defDataAdd( varname = "SIMD16_Vigintile",
                                            dist    = "nonrandom",
                                            formula = "get_SIMD16_Vigintile(idnum)" )

SIMD16_Decile_definition = defDataAdd( varname = "SIMD16_Decile",
                                          dist    = "nonrandom",
                                          formula = "get_SIMD16_Decile(idnum)" )

SIMD16_Quintile_definition = defDataAdd( varname = "SIMD16_Quintile",
                                          dist    = "nonrandom",
                                          formula = "get_SIMD16_Quintile(idnum)" )


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
trial_data.synthetic = addColumns(distance_definition,trial_data.synthetic)
trial_data.synthetic = addColumns(SIMD16_Quintile_definition,trial_data.synthetic)
trial_data.synthetic = addColumns(SIMD16_Decile_definition,trial_data.synthetic)
trial_data.synthetic = addColumns(SIMD16_Vigintile_definition,trial_data.synthetic)



# trial_data.synthetic = addColumns(public_time_definition_RANDOM ,trial_data.synthetic)
# trial_data.synthetic = addColumns(private_time_definition_RANDOM,trial_data.synthetic)

variables_for_synthesis = 
  trial_data.synthetic %>%
  colnames %>%
  keep( ~str_detect(.x, "^RAW\\.") ) %>% 
  # Don't want to map the distance variable
  discard( ~str_detect(.x, "distance"))

map.vars = tibble(
  idnum = trial_data.synthetic %>% pull( idnum )
)

trial_data.synthetic.MODERATE = trial_data.synthetic
map.vars.MODERATE = map.vars

lower_percentile = 0.20
upper_percentile = 0.50

private_time.mapping_vars = list(
  rmin = 15, #(trial_data.synthetic$RAW.private_time %>% stats::quantile(lower_percentile)),
  rmax = 35, #(trial_data.synthetic$RAW.private_time %>% stats::quantile(upper_percentile)),
  tmin = 0.25
)

public_time.mapping_vars = list(
  rmin = 25, #(trial_data.synthetic$RAW.public_time %>% stats::quantile(lower_percentile) ),
  rmax = 75, #(trial_data.synthetic$RAW.public_time %>% stats::quantile(upper_percentile) ),
  tmin = 0.25
)


for ( this.raw_var in variables_for_synthesis ) {

  these.mapping_parameters = sprintf( "%s.mapping_vars", 
                                      this.raw_var %>%
                                        str_replace( "^RAW\\.", "" ) %>% 
                                        str_replace( "_random", "" )) %>% get
  this.mask = ( trial_data.synthetic.MODERATE %>% pull( has_car ) ) == "Yes"
  
  attendance.out = generate_attendance_data( base_var = this.raw_var,
                                             trial_vars = trial_data.synthetic.MODERATE,
                                             map_vars = map.vars.MODERATE,
                                             timepoints = timepoint.list,
                                             modify_mask = this.mask,
                                             modify_val = -0.10,
                                             rmin = these.mapping_parameters$rmin,
                                             rmax = these.mapping_parameters$rmax,
                                             tmin = these.mapping_parameters$tmin ) 
  
  trial_data.synthetic.MODERATE = attendance.out$trial_data
  map.vars.MODERATE = attendance.out$mapping_data
  
}


trial_data.synthetic.EXTREME = trial_data.synthetic
map.vars.EXTREME = map.vars

private_time.mapping_vars$tmin = -0.5
public_time.mapping_vars$tmin  = -0.5


for ( this.raw_var in variables_for_synthesis ) {
  
  these.mapping_parameters = sprintf( "%s.mapping_vars", 
                                      this.raw_var %>%
                                        str_replace( "^RAW\\.", "" ) %>% 
                                        str_replace( "_random", "" )) %>% get
  this.mask = ( trial_data.synthetic.EXTREME %>% pull( has_car ) ) == "Yes"
  
  attendance.out = generate_attendance_data( this.raw_var,
                                             trial_data.synthetic.EXTREME,
                                             map.vars.EXTREME,
                                             timepoint.list,
                                             modify_mask = this.mask,
                                             modify_val = -0.20,                                             rmin = these.mapping_parameters$rmin,
                                             rmax = these.mapping_parameters$rmax,
                                             tmin = these.mapping_parameters$tmin
                                               )
  
  trial_data.synthetic.EXTREME = attendance.out$trial_data
  map.vars.EXTREME = attendance.out$mapping_data
  
}

attendance_columns = trial_data.synthetic.MODERATE %>%
  select( starts_with("ATT.")) %>%
            colnames

dataset_lists = c( "EXTREME", "MODERATE" )

for ( this.d_type in dataset_lists ) {
  this.d_name = sprintf("trial_data.synthetic.%s",
                        this.d_type) 
  
  this.d = get( this.d_name )
  
  for ( this.attendance_column in attendance_columns ) {
    this.randomized_column = paste(this.attendance_column,"RANDOM",sep="_")
    
    r = this.d %>%
      pull( !!sym(this.attendance_column)) %>% 
      sample
    
    this.d[[this.randomized_column]] = r
  }
  
  assign( this.d_name, this.d )
}

#####################################################################
### ADD DROPOUTS
#####################################################################


public_time.trajectory = colnames( trial_data.synthetic.MODERATE %>%
                                     select( ends_with( "public_time")) %>%
                                     select( starts_with("ATT")) )  %>%
  purrr::discard(~ str_detect( .x, "T0" ) )
private_time.trajectory = colnames( trial_data.synthetic.MODERATE %>%
                                      select( ends_with( "private_time")) %>%
                                      select( starts_with("ATT")) ) %>%
  purrr::discard(~ str_detect( .x, "T0" ) )
public_time_RANDOM.trajectory = sprintf( "%s_RANDOM", public_time.trajectory )
private_time_RANDOM.trajectory = sprintf( "%s_RANDOM", private_time.trajectory )

EXTREME.DO_rate = 0.25

trial_data.synthetic.EXTREME.DO = trial_data.synthetic.EXTREME %>%
  add_dropouts( public_time.trajectory, target_perc = EXTREME.DO_rate ) %>% 
  dplyr::rename( DO.public_time = DROPOUT ) %>% 
  add_dropouts( private_time.trajectory, target_perc = EXTREME.DO_rate ) %>%
  dplyr::rename( DO.private_time = DROPOUT ) %>% 
  add_dropouts( public_time_RANDOM.trajectory, target_perc = EXTREME.DO_rate ) %>% 
  dplyr::rename( DO.public_time_RANDOM = DROPOUT ) %>% 
  add_dropouts( private_time_RANDOM.trajectory, target_perc = EXTREME.DO_rate ) %>%  
  dplyr::rename( DO.private_time_RANDOM = DROPOUT )

MODERATE.DO_rate = 0.10

trial_data.synthetic.MODERATE.DO = trial_data.synthetic.MODERATE %>%
  add_dropouts( public_time.trajectory, target_perc = MODERATE.DO_rate ) %>% 
  dplyr::rename( DO.public_time = DROPOUT ) %>% 
  add_dropouts( private_time.trajectory, target_perc = MODERATE.DO_rate ) %>%
  dplyr::rename( DO.private_time = DROPOUT ) %>% 
  add_dropouts( public_time_RANDOM.trajectory, target_perc = MODERATE.DO_rate ) %>% 
  dplyr::rename( DO.public_time_RANDOM = DROPOUT ) %>% 
  add_dropouts( private_time_RANDOM.trajectory, target_perc = MODERATE.DO_rate ) %>%  
  dplyr::rename( DO.private_time_RANDOM = DROPOUT )

  
### NB. Any data that we want to include in a dropdown box in the
###     resulting Shiny app should be prefixed with "METRIC_".  Also,
###     make sure that these are readable, as text in the dropdown
###     bow will be derived from the column names.  For example:
###     METRIC_public_distance will be shown as Public distance in 
###     the drop down box.

trial_data.synthetic = trial_data.synthetic %>% 
  mutate( METRIC_public_transport_time  = RAW.public_time,
          METRIC_private_transport_time = RAW.private_time,
          METRIC_distance = RAW.distance )

save( trial_data.synthetic,
      trial_data.synthetic.EXTREME.DO,
      map.vars.EXTREME,
      trial_data.synthetic.MODERATE.DO,
      map.vars.MODERATE,
      number_of_participants,
      timepoint.list,
      variables_for_synthesis,
      file=sprintf( "dat/05a_SYNTHETIC-DATA_n=%d.Rdat",
                    number_of_participants ) )

