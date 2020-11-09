library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )
library( cowplot )
# library( tibble )

load( "dat/05a_SYNTHETIC-DATA_n=1958.Rdat" )

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


RAW.columns = trial_data.synthetic.EXTREME.DO %>% colnames %>% keep( ~str_detect(.x, "^RAW\\." ) )
ATT.columns = trial_data.synthetic.EXTREME.DO %>% colnames %>% keep( ~str_detect(.x, "^ATT\\." ) )

#####################################################################
### Comparing mapping values to eventual attendance #################
#####################################################################

sets_to_check = expand.grid( RAW.columns, ATT.columns ) %>% 
  set_names( "input", "output") %>% 
  mutate( metric = str_replace( input, "RAW\\.", "" ) ) %>% 
  filter( str_detect( output, sprintf( "%s$", metric ) ) )

for( dataset_type in c( "EXTREME",
                        "MODERATE" ) ) {
  
  this_data.name = sprintf( "trial_data.synthetic.%s", dataset_type )
  this_mapping.name = this_data.name %>% str_replace( "trial_data.synthetic",
                                                      "map.vars")
  
  this_data = get( this_data.name )
  this_mapping = get( this_mapping.name )
  
  
  for ( i in 1:nrow(sets_to_check) ) {
    
    input_variable  = sets_to_check$input[i]  %>% as.character()
    output_variable = sets_to_check$output[i] %>% as.character()
    mapping_variable = input_variable %>% str_replace( "RAW", "MAP" )
    this_timepoint = ( output_variable %>% str_match( "^.*\\.(T\\d{1})\\..*$" ) ) [2]
    
    i.d = this_data %>% select( idnum,
                                {{input_variable}},
                                {{output_variable}}) %>% 
      inner_join( this_mapping %>% select( idnum,
                                           {{mapping_variable}}),
                  by="idnum")
    
    p1 = ggplot( i.d,
                 aes( x=!!sym(input_variable),
                      y=!!sym(mapping_variable)) ) +
      geom_point() +
      geom_line() +
      ylim(0,1)
    
    p2 = ggplot( i.d,
                 aes( x=!!sym(output_variable),
                      y=!!sym(input_variable),
                      group=!!sym(output_variable)
                 ) ) +
      geom_boxplot( )
    
    plot_title <- ggdraw() +
      draw_label(
        sprintf("Mapping from %s to attendance at %s (%s)",
                input_variable,
                this_timepoint,
                dataset_type),
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
    
    plot_row = plot_grid( p1, p2, labels=c("A", "B") )
    
    final_plot = plot_grid(
      plot_title, plot_row,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
    
    save_plot(
      sprintf( "fig/05b_CHECK-MAPPING_%s-%s-%s.png",
               input_variable,
               this_timepoint,
               dataset_type),
      final_plot,
      base_asp = 2)
    
  }
  
}


#####################################################################
### Comparing true values to random values ##########################
#####################################################################

sets_to_check = tibble(
  true = ATT.columns %>% purrr::keep( ~ !str_detect( .x, "RANDOM" ) ),
  random = sprintf( "%s_RANDOM", true ) )

for( dataset_type in c( "EXTREME",
                        "MODERATE" ) ) {
  
  this_data.name = sprintf( "trial_data.synthetic.%s.DO", dataset_type )
  # this_mapping.name = this_data.name %>% str_replace( "trial_data.synthetic",
  #                                                     "map.vars")
  
  this_data = get( this_data.name )
  # this_mapping = get( this_mapping.name )
  
  for ( i in 1:nrow(sets_to_check) ) {
    
    true_variable  = sets_to_check$true[i]  %>% as.character()
    random_variable = sets_to_check$random[i] %>% as.character()
    raw_variable = true_variable %>% str_replace( "ATT\\.T\\d{1}", "RAW" )
    #mapping_variable = true_variable %>% str_replace( "RAW", "MAP" )
    this_timepoint = ( random_variable %>% str_match( "^.*\\.(T\\d{1})\\..*$" ) ) [2]
    
    i.d = this_data %>% select( idnum,
                              {{raw_variable}},
                             {{true_variable}},
                               {{random_variable}}) %>%
       pivot_longer( starts_with( "ATT"),
                     names_to = "variable",
                     values_to = "value" ) %>%
      mutate( RANDOM = str_detect( variable, "RANDOM" ) ) %>%
      mutate( variable = str_replace( variable, "_RANDOM", "" ))
    
    p = ggplot( i.d,
                 aes( x=value,
                      y=!!sym(raw_variable),
                      group=value
                 ) ) +
      facet_wrap( ~RANDOM ) +
      geom_boxplot( )
    
    ggsave( sprintf( "fig/05b_CHECK-RANDOMvTRUE_%s-%s-%s.png",
               raw_variable,
               this_timepoint,
               dataset_type) )
    
  }
  
}

#####################################################################
### Checking dropout rates ##########################################
#####################################################################

strings_to_check = c("ATT\\..*public_time$",
                     "ATT\\..*private_time$",
                     "ATT\\..*public_time_RANDOM$",
                     "ATT\\..*private_time_RANDOM$")

for ( this.string in strings_to_check ) {
dropouts.EXTREME = trial_data.synthetic.EXTREME.DO %>%
  select( idnum, matches( this.string, perl=TRUE ) ) %>%
  pivot_longer( -idnum,
                names_to = "appointment",
                values_to = "attendance" ) %>%
  group_by( idnum ) %>%
  dplyr::summarise( total = sum(attendance) )

dropouts.MODERATE = trial_data.synthetic.MODERATE.DO %>%
  select( idnum, matches( this.string, perl=TRUE ) ) %>%
  pivot_longer( -idnum,
                names_to = "appointment",
                values_to = "attendance" ) %>%
  group_by( idnum ) %>%
  dplyr::summarise( total = sum(attendance) )

dropouts.BOTH = dropouts.EXTREME %>%
  inner_join( dropouts.MODERATE, by="idnum",
              suffix = c( ".EXTREME", ".MODERATE" ) ) %>%
  pivot_longer( -idnum,
                names_to="dataset",
                values_to="attendance_count" )

ggplot( dropouts.BOTH,
         aes( x=dataset,
              y=attendance_count)) +
  geom_boxplot()

ggplot( dropouts.BOTH,
        aes( fill=dataset,
             x=attendance_count)) +
  geom_bar( position="dodge")

ggsave( sprintf( "fig/05b_CHECK-DROPOUTS_%s.png",
        this.string ) )
}


