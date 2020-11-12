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
# library( baguette ) # bagged trees
# library( future ) # parallel processing & decrease computation time
# library( xgboost ) # boosted trees

load( "dat/06a_LEARNING-DATASET_n=1958.Rdat" )

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
   
   this.m.fit = this.m %>% fit( TARGET ~ ., analysis_set )
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
                                     model = this.m )
      
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
                                    RAW.distance )

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



# ggplot( data=variable_importance.update %>% 
#            pivot_longer( -id,
#                          names_to  ="variable",
#                          values_to = "importance") %>% 
#            mutate( variable2 = factor( variable )) %>% 
#            mutate( variable2 = fct_reorder( variable2, desc(importance) )),
#         aes(x=variable2,
#             y=importance,
#             group=1)) +
#    geom_point( ) +
#    geom_line( ) +
#    ggtitle( "Neutral dataset" )

# 
#    
# rand_forest.model.fit.PREDICTIONS = rand_forest.model.fit %>% 
#    predict( new_data = test ) %>%
#    bind_cols( test ) %>% 
#    mutate( .pred_round = plyr::round_any( .pred, 1 ) )
# 
# ### Direct comparison of predictions (y) and actual data (x)
# ggplot( data = rand_forest.model.fit.PREDICTIONS,
#         aes( x=num_appointments_attended,
#              y=.pred) ) + geom_point()
# 
# 
# ### Measurement of fit, data are numeric
# rand_forest.model.fit.PREDICTIONS %>%
#    metrics(num_appointments_attended, .pred_round) %>%
#    select(-.estimator) 
# 
# ### Measurement of fit, data are classes
# rand_forest.model.fit.PREDICTIONS.classification = rand_forest.model.fit.PREDICTIONS %>%
#    mutate( num_appointments_attended = factor( num_appointments_attended, levels=0:5 ) ) %>% 
#    mutate( .pred_round = factor( .pred_round, levels=0:5 ) )
# 
# rand_forest.model.fit.PREDICTIONS.classification %>% 
#    metrics(num_appointments_attended, .pred_round) %>% 
#    select(-.estimator) %>%
#    filter(.metric == "accuracy") 
# 
# tibble(
#    "precision" = 
#       precision(rand_forest.model.fit.PREDICTIONS.classification,
#                 num_appointments_attended,
#                 .pred_round) %>%
#       select(.estimate),
#    "recall" = 
#       recall(rand_forest.model.fit.PREDICTIONS.classification,
#              num_appointments_attended,
#              .pred_round) %>%
#       select(.estimate)
# ) %>%
#    unnest() %>%
#    kable()
# 
# rand_forest.model.fit.PREDICTIONS.classification %>%
#    f_meas(num_appointments_attended,
#           .pred_round) %>%
#    select(-.estimator) %>%
#    kable()
#    
# ### Confusion matrix, data are classes
# rand_forest.model.fit.PREDICTIONS.classification %>% 
#    conf_mat(num_appointments_attended, .pred_round) %>%
#    pluck(1) %>%
#    as_tibble() %>%
#    ggplot(aes(Prediction, Truth, alpha = n)) +
#    geom_tile(show.legend = FALSE) +
#    geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

###
### TO DO:
### (1) Extract tree

### Extract tree
### This should work, but takes a long time!
### https://tidypredict.tidymodels.org/articles/rf.html
# tidypredict_fit(rand_forest.model.fit)




#####################################################################
### Cox regression
#####################################################################




