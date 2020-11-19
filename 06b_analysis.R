library( dplyr    )
library( magrittr )
library( ggplot2  )
# library( stringr  )
library( purrr    )
library(tidypredict)
library(tibble)
library(corrplot)
library( tidymodels )
# library( rules )
library( knitr )
library( rpart.plot )
library( RColorBrewer )
# library( baguette ) # bagged trees
# library( future ) # parallel processing & decrease computation time
# library( xgboost ) # boosted trees

load( "dat/06a_LEARNING-DATASET_n=1912.Rdat" )

analysis_colours = c(
   NEUTRAL = grey(0.3),
   PRIVATE_BIAS = brewer.pal(7,"Paired")[2],
   PUBLIC_BIAS = brewer.pal(7,"Paired")[4]
)

### https://cran.r-project.org/web/packages/parsnip/vignettes/parsnip_Intro.html
### https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c


data = NEUTRAL.d %>% ungroup()

rand_forest.model = rand_forest(trees = 200, min_n = 5) %>% 
   set_engine( "ranger",
               verbose = TRUE,
               importance = "impurity") %>% 
   set_mode( "regression" )

rand_forest.model %>% translate()




predict_function = function(split,
                             id,
                             model) {
   # https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c
   
   cat( sprintf( "Processing fold: %s\n", id ) )
   
   analysis_set = split %>% analysis()
   
   # analysis_prepped <- analysis_set %>% recipe_rf()
   # analysis_baked <- analysis_prepped %>% bake(new_data = analysis_set) 
   # 
   # this.m  = rand_forest(trees = 200, min_n = 5) %>% 
   #    set_engine( "ranger", verbose = TRUE ) %>% 
   #    set_mode( "regression" )
   
   this.m.fit = model %>% fit( TARGET ~ ., analysis_set )
   these.variable_importance = this.m.fit$fit$variable.importance
   
   # rand_forest.model.fit = rand_forest.model %>%
   #    fit(num_appointments_attended ~ ., train %>%
   #           select(age,gender,arm,RAW.distance,has_car,starts_with("RAW"),num_appointments_attended) )
   # 
   # rand_forest.model.fit.PREDICTIONS = rand_forest.model.fit %>% 
   #    predict( new_data = test ) %>%
   #    bind_cols( test ) %>% 
   #    mutate( .pred_round = plyr::round_any( .pred, 1 ) )
   
   assessment_set <- split %>% assessment()
   # assessment_prepped <- assessment_set %>% recipe_rf()
   # assessment_baked <- assessment_prepped %>% bake(new_data = assessment_set) 
   

   these.predictions = tibble(
      "id" = id,
      "truth" = assessment_set$TARGET,
      "prediction" = this.m.fit %>%
         predict(new_data = assessment_set) %>%
         unlist()
   )
   
   return( list( predictions = these.predictions,
                 variable_importance = these.variable_importance ) )
}




perform_random_forest_analysis = function( data,
                                           model,
                                           predictor_variables,
                                           target_variable
) {
   
   data.modified = data %>%
      ungroup() %>% 
      mutate( TARGET = !!target_variable ) %>% 
      select( !!!predictor_variables, TARGET  )
   
   ### 0. Consider visualisations
   
   ### 1. Split the data into a test and training set
   ###    and create cross validation sets
   split      = initial_split(data.modified)
   data_train = training(split)
   data_test  = testing(split)
   CV10_partitions = vfold_cv(data_train)
   
   ### 2. Recipes?
   ### https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c
   ### recipe_rf <- function(dataset) {
   # recipe(Churn ~ ., data = dataset) %>%
   #    step_string2factor(all_nominal(), -all_outcomes()) %>%
   #    step_dummy(all_nominal(), -all_outcomes()) %>%
   #    step_center(all_numeric()) %>%
   #    step_scale(all_numeric()) %>%
   #    prep(data = dataset)
   # }
   
   # http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
   
   # model.fit = rand_forest.model %>% fit( TARGET ~ ., data_train )
   
   variable_importance.tibble = NULL
   predictions.tibble = tibble( id = NA,
                                truth = NA,
                                prediction = NA )

   for ( this.partition in 1:nrow( CV10_partitions ) ) {
      
      results_cv = predict_function(split  = CV10_partitions$splits[[this.partition]],
                                     id    = CV10_partitions$id[this.partition],
                                     model = model )
      
      variable_importance.update = tibble( id = CV10_partitions$id[this.partition] ) 
      
      for ( result.i in 1:length(results_cv$variable_importance) ) {
         this.variable = ( results_cv$variable_importance %>% names )[result.i]
         this.value = ( results_cv$variable_importance )[[result.i]]
         variable_importance.update = variable_importance.update %>%
            mutate( !!sym(this.variable) := this.value)
      }

      
      if ( is.null( variable_importance.tibble ) ) {
         variable_importance.tibble = variable_importance.update
      } else {
         variable_importance.tibble = variable_importance.tibble %>% 
            add_row( variable_importance.update )
      }
      
      predictions.tibble = predictions.tibble %>% 
         add_row( results_cv$predictions ) %>% 
         filter(!is.na(id))
   }
   
   return( list( predictions = predictions.tibble,
                 variable_importance = variable_importance.tibble ))
}

DEFAULT.predictor_variables = quos( age,
                                    gender,
                                    arm,
                                    has_car,
                                    RAW.public_time,
                                    RAW.private_time,
                                    RAW.distance,
                                    SIMD16_Quintile,
                                    SIMD16_Decile,
                                    SIMD16_Vigintile)

### To do:
### (1) Performance statistics
### (2) Graph of performance statistics
### (3) Graph of variable importance

NEUTRAL_NUMAPPT.out = perform_random_forest_analysis( data  = NEUTRAL.d,
                                                      model = rand_forest.model,
                                                      predictor_variables = DEFAULT.predictor_variables,
                                                      target_variable = quo(num_appointments_attended)
)

MODERATE_PRIVATE_BIAS_NUMAPPT.out = perform_random_forest_analysis( data  = MODERATE_PRIVATE_BIAS.d,
                                                                    model = rand_forest.model,
                                                                    predictor_variables = DEFAULT.predictor_variables,
                                                                    target_variable = quo(num_appointments_attended)
)


MODERATE_PUBLIC_BIAS_NUMAPPT.out = perform_random_forest_analysis( data  = MODERATE_PUBLIC_BIAS.d,
                                                                   model = rand_forest.model,
                                                                   predictor_variables = DEFAULT.predictor_variables,
                                                                   target_variable = quo(num_appointments_attended)
)

#####################################################################
### VARIABLE IMPORTANCE #############################################
#####################################################################

OVERALL.variable_importance = NEUTRAL_NUMAPPT.out$variable_importance %>%
   mutate( analysis = "NEUTRAL" ) %>% 
   bind_rows( MODERATE_PRIVATE_BIAS_NUMAPPT.out$variable_importance %>% 
                 mutate( analysis = "PRIVATE_BIAS") ) %>% 
   bind_rows( MODERATE_PUBLIC_BIAS_NUMAPPT.out$variable_importance %>% 
                 mutate( analysis = "PUBLIC_BIAS") )

OVERALL.variable_importance.long = OVERALL.variable_importance %>% 
   pivot_longer( -c("id","analysis"),
                 names_to  ="variable",
                 values_to = "importance")
   
ggplot( OVERALL.variable_importance.long,
        aes( x=analysis,
             y=importance,
             colour=analysis) ) +
   geom_boxplot( ) +
   geom_jitter( ) +
   facet_wrap( ~variable, scales="free_y" ) +
   scale_colour_manual( values=analysis_colours ) +
   theme_bw()


ggplot( OVERALL.variable_importance.long,
        aes( x=variable,
             y=importance,
             col=analysis) ) +
   geom_boxplot( ) +
   facet_wrap( ~analysis, scales="free_y" ) +
   geom_point( position = position_jitterdodge() ) +
   scale_colour_manual( values=analysis_colours ) +
   theme_bw() +
   theme( axis.text.x = element_text(angle=90,hjust=1))
   
importance_baseline = OVERALL.variable_importance %>% 
   filter( analysis == "NEUTRAL" )  %>% 
   pivot_longer( -c("id","analysis"),
                 names_to  ="variable",
                 values_to = "importance") %>%
   group_by( variable ) %>% 
   summarise( average_baseline = mean( importance ))
   
OVERALL.variable_importance_normalised.long = OVERALL.variable_importance.long %>%
   inner_join( importance_baseline,
               by="variable" ) %>% 
   mutate( normalised_importance = log2( importance / average_baseline ) )

ggplot( OVERALL.variable_importance_normalised.long,
        aes( x=variable,
             y=normalised_importance,
             col=analysis) ) +
   geom_boxplot( ) +
   facet_wrap( ~analysis, scales="free_y" ) +
   geom_point( position = position_jitterdodge() ) +
   scale_colour_manual( values=analysis_colours ) +
   theme_bw() +
   theme( axis.text.x = element_text(angle=90,hjust=1))

#####################################################################
### PERFORMANCE STATISTICS ##########################################
#####################################################################

### Regression problem

OVERALL.predictions = NEUTRAL_NUMAPPT.out$predictions %>%
   mutate( analysis = "NEUTRAL" ) %>% 
   bind_rows( MODERATE_PRIVATE_BIAS_NUMAPPT.out$predictions %>% 
                 mutate( analysis = "PRIVATE_BIAS") ) %>% 
   bind_rows( MODERATE_PUBLIC_BIAS_NUMAPPT.out$predictions %>% 
                 mutate( analysis = "PUBLIC_BIAS") )

OVERALL.predictions.metrics = OVERALL.predictions %>%
   group_by(analysis) %>% 
   metrics(truth, prediction)

ggplot( OVERALL.predictions.metrics,
        aes(x=analysis,
            y=.estimate,
            colour=.metric) ) +
   facet_wrap( ~.metric ) +
           geom_point() +
   theme_bw() +
   theme( axis.text.x = element_text(angle=90,hjust=1))

### Classification problem

OVERALL.predictions.classification = OVERALL.predictions %>% 
   mutate( prediction.class = floor( prediction ) ) %>% 
   mutate( prediction.class = factor( prediction.class, levels=0:5  )) %>% 
   mutate( truth = factor( truth, levels=0:5  ))
   
OVERALL.predictions.classification.metrics = OVERALL.predictions.classification %>%
   group_by( analysis ) %>% 
   metrics(truth, prediction.class) %>%
   select(-.estimator) %>% 
   ### ADD PRECISION
   bind_rows( OVERALL.predictions.classification %>%
                 group_by( analysis ) %>% 
                 precision(truth, prediction.class) %>%
                 select(-.estimator) ) %>% 
   ### ADD RECALL
   bind_rows( OVERALL.predictions.classification %>%
                group_by( analysis ) %>% 
                recall(truth, prediction.class) %>% 
                select( -.estimator ) ) %>% 
   ### ADD F_MEAS
   bind_rows( OVERALL.predictions.classification %>%
                 group_by( analysis ) %>%
                 f_meas(truth, prediction.class) %>%
                 select(-.estimator) )


ggplot( OVERALL.predictions.classification.metrics,
        aes(x=analysis,
            y=.estimate,
            colour=.metric) ) +
   facet_wrap( ~.metric, scales="free_y" ) +
   geom_point() +
   theme_bw() +
   theme( axis.text.x = element_text(angle=90,hjust=1))



OVERALL.predictions.classification.confusion_matrix = OVERALL.predictions.classification %>% 
   group_by( analysis ) %>% 
   conf_mat(truth, prediction.class) 

for ( i in nrow( OVERALL.predictions.classification.confusion_matrix ) ) {
   this.analysis = ( OVERALL.predictions.classification.confusion_matrix %>% 
      pull( analysis ) )[i]
   this.cf = ( OVERALL.predictions.classification.confusion_matrix %>% 
      pull( conf_mat ) )[[ i ]] %>% pluck(1) %>% as_tibble()
   
   ggplot( this.cf,
           aes(Prediction, Truth, alpha = n) ) +
      geom_tile(show.legend = FALSE) +
      geom_text(aes(label = n), colour = "white", alpha = 1, size = 8) +
      ggtitle( this.analysis ) +
      theme_minimal()
}

###
### TO DO:
### (1) Extract tree
### This should work, but takes a long time!
### https://tidypredict.tidymodels.org/articles/rf.html
# tidypredict_fit(rand_forest.model.fit)
### (2) Do traditional Cox regression



#####################################################################
### Cox regression
#####################################################################




