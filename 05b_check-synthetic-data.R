library( dplyr )
library( magrittr )
library( simstudy )
library( sigmoid )
library( ggplot2 )
library( purrr )
library( stringr )


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

ggplot( data=trial_data.synthetic.MODERATE,
        aes( x = ATT.T0.public_time,
             y = RAW.public_time,
             group = ATT.T0.public_time )) + geom_boxplot()

ggplot( data=trial_data.synthetic.MODERATE,
        aes( x = ATT.T0.public_time_random,
             y = RAW.public_time_random,
             group = ATT.T0.public_time_random )) + geom_boxplot()

ggplot( data = map.vars.MODERATE,
        aes( x=RAW.public_time,
             y=MAP.public_time)) + geom_point()

ggplot( data=trial_data.synthetic.EXTREME,
        aes( x = ATT.T0.public_time,
             y = RAW.public_time,
             group = ATT.T0.public_time )) + geom_boxplot()

ggplot( data=trial_data.synthetic.EXTREME,
        aes( x = ATT.T0.public_time_random,
             y = RAW.public_time_random,
             group = ATT.T0.public_time_random )) + geom_boxplot()

ggplot( data = map.vars.EXTREME,
        aes( x=RAW.public_time,
             y=MAP.public_time)) + geom_point()

# 
# save( trial_data.synthetic,
#       trial_data.synthetic.EXTREME,
#       map.vars.EXTREME,
#       trial_data.synthetic.MODERATE,
#       map.vars.MODERATE,
#       file=sprintf( "dat/05a_SYNTHETIC-DATA_n=%d.Rdat",
#                     number_of_participants ) )

