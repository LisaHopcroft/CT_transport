library(tidyverse)

## devtools::install_github("TheDataLabScotland/simdr")
library(simdr)

simdr::simd16_domains %>%
  filter( Council_area == "Renfrewshire" &
            Intermediate_Zone == "Lochwinnoch" )

### Access_domain?  What about private access?

simdr::simd16_indicators %>%
  filter( Council_area == "Renfrewshire" &
            Intermediate_Zone == "Lochwinnoch" ) %>%
  select (Data_Zone, starts_with("drive") )




