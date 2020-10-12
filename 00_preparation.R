# library( dplyr )
library( rgdal ) 
library( magrittr )
library( stringr )
library( purrr )

### Read in shapefiles, the output is an object of the class
### SpatialPolygonsDataFrame
postcode.objects.in = readOGR( dsn = "NRS_shapefiles/spd-unit-boundaries-cut-19-1", layer = "PC_Cut_19_1" )
### Transform data in shapefile to the necessary format
postcode.objects.sp = spTransform( postcode.objects.in, CRS( "+proj=longlat +datum=WGS84" ) )

### Identify the postcode objects that we are interested in
### and retain only the data in the SpatialPolygonsDataFrame that
### is relevant for the areas of interest 
postcode.objects.sp.outcodes = postcode.objects.sp$Postcode %>% map_chr( ~str_replace(.x, " .*", "" ) )

save( postcode.objects.in,
      postcode.objects.sp,
      postcode.objects.sp.outcodes,
      file="dat/00_PREPARATION.Rdat" )