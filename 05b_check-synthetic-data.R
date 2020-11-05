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

# 
# this_data = trial_data.synthetic.EXTREME 
# this_mapping = map.vars.EXTREME 


RAW.columns = trial_data.synthetic.EXTREME %>% colnames %>% keep( ~str_detect(.x, "^RAW\\." ) )
ATT.columns = trial_data.synthetic.EXTREME %>% colnames %>% keep( ~str_detect(.x, "^ATT\\." ) )

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
      geom_line()
    
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
      sprintf( "fig/05b_CHECK_%s-%s-%s.png",
               input_variable,
               this_timepoint,
               dataset_type),
      final_plot,
      base_asp = 2)
    
  }
  
}
