#####################################################################
#####################################################################
### SynthPop ########################################################
#####################################################################
#####################################################################

library(synthpop) # to load package

# Code obtained from https://synthpop.org.uk/assets/firstsynthesis.r
# Via https://synthpop.org.uk/get-started.html

# This code will use the supplied data frame SD2011
help(SD2011)                   # this will give you information about it
dim(SD2011)                    # get size of data frame 
codebook.syn(SD2011)$tab       # get summary info about variables

# Notice that eduspec has 27 levels - so leave this for later.
# Note the negative values in some continuous variables (e.g. nociga).
# SD2011 has 35 variables, so lets go for a smaller number.
# Also bmi should be derived from height and weight, 
# so we'll leave it out for now.

mydata <- SD2011[, c(1, 3, 6, 8, 11, 17, 18, 19, 20, 10)] 
codebook.syn(mydata)$tab 

# Only remaining item to check is the negative income values
table(mydata$income[mydata$income < 0], useNA = "ifany")

# We can see that income has both NA values and -8. 
# To make the synthetic data like the original we keep both types of 
# missingness by setting cont.na for income
mysyn <- syn(mydata, cont.na = list(income = -8))  # default syntheis but adding -8 missing values for income

summary(mysyn)
compare(mysyn, mydata, stat = "counts")

# Export to SPSS
write.syn(mysyn, filename = "mysyn", filetype = "SPSS")

# Well done for getting to here
# Now some extra exploration of mysyn
names(mysyn)
mysyn$method
mysyn$predictor.matrix
mysyn$visit.sequence
mysyn$cont.na

multi.compare(mysyn, mydata, var = "marital", by = "sex")
multi.compare(mysyn, mydata, var = "income", by = "agegr")
multi.compare(mysyn, mydata, var = "income", by = "edu", cont.type = "boxplot")

#####################################################################
#####################################################################
### SimStudy ########################################################
#####################################################################
#####################################################################

library( simstudy )
# 
# def <- defData(varname = "nr", dist = "nonrandom", formula = 7, id = "idnum")
# def <- defData(def, varname = "x1", dist = "uniform", formula = "10;20")
# def <- defData(def, varname = "y1", formula = "nr + x1 * 2", variance = 8)
# def <- defData(def, varname = "y2", dist = "poisson", formula = "nr - 0.2 * x1",
#                link = "log")
# def <- defData(def, varname = "xnb", dist = "negBinomial", formula = "nr - 0.2 * x1",
#                variance = 0.05, link = "log")
# def <- defData(def, varname = "xCat", formula = "0.3;0.2;0.5", dist = "categorical")
# def <- defData(def, varname = "g1", dist = "gamma", formula = "5+xCat", variance = 1,
#                link = "log")
# def <- defData(def, varname = "b1", dist = "beta", formula = "1+0.3*xCat", variance = 1,
#                link = "logit")
# def <- defData(def, varname = "a1", dist = "binary", formula = "-3 + xCat",
#                link = "logit")
# def <- defData(def, varname = "a2", dist = "binomial", formula = "-3 + xCat",
#                variance = 100, link = "logit")

# dt <- genData(1000, def)
 
# 
# tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
# tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
# tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5 * T", 
#                 variance = 1)
# tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 10 + 5 * T", 
#                 variance = 1)


# dtTrial <- genData(500, tdef)
# dtTrial

#####
#####
#####
#####
#####

num_patients = 10
patient_info = rnorm( num_patients, mean=10, sd=3 )

get_patient_info = function ( id ) {
  # return( 2*id )
  return( patient_info[id] ) 
}

trial_data.definition = 
  ### Defining the arm
  defData( varname = "ARM_categorical",
           dist = "categorical",
           formula = catProbs(0.50,0.50),
           id = "idnum")
  ### Alternative version for arm
  # defData( varname = "ARM_binary",
  #        dist = "binary",
  #        formula = 0.5 ) %>% 
  
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

### Adding STATIC attendance data
trial_data.definition = trial_data.definition %>% 
  defData(varname = "attendance_T0", dist = "binary", formula = 1  ) %>% 
  defData(varname = "attendance_T1", dist = "binary", formula = 0.9) %>% 
  defData(varname = "attendance_T2", dist = "binary", formula = 0.8)


trial_data.synthetic.0 = genData( num_patients, trial_data.definition )

### Generating distance information
distance_information = defDataAdd( varname = "distance",
                                   dist    = "nonrandom",
                                   formula = "get_patient_info(idnum)" )

trial_data.synthetic.1 = addColumns(distance_information, trial_data.synthetic.0) %>% 
  mutate( distance_norm = ( distance/sum(distance) ) ) %>% 
  mutate( distance_norm = 1-distance_norm / max(distance_norm )) 


###
###
###
# tdef <- defData(varname = "attendance",
#                 dist = "binary", formula = 0.5 )
# tdef <- defData(tdef, varname = "T0", dist = "uniform", formula = 1)
# tdef <- defData(tdef, varname = "T1", dist = "binary", formula = 0.9)
# tdef <- defData(tdef, varname = "T2", dist = "binary", formula = 0.8)
# 
# dtTrial <- genData(500, tdef)
# dtTrial
