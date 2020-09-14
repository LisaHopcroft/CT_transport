library( tidyverse )
library(PostcodesioR)

### NB, must include number in the 'outcode'"
### random_postcode("PA") doesn't work
### random_postcode("PA12 4") doesn't work

random_pa = random_postcode("PA12")

random_pa$postcode
random_pa$longitude
random_pa$latitude