# library( dplyr    )
# library( magrittr )
# library( readr )
# library( ggplot2 )
# library( stringr )
library( readxl )

load( "dat/01a_RANDOM-POSTCODES_NEW_n=2000.Rdat" )

PRIVATE_journey_times.fastest.ArcPro = readxl::read_excel( "dat/ArcPro/DRIVE_TIME_ArcPro_TableToExcel.xlsx")

save( PRIVATE_journey_times.fastest.ArcPro,
      file=sprintf( "dat/03a_PRIVATE-journey-times_n=%d.Rdat",
                    number_of_participants ) )
