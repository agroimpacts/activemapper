## Ghana Agroecozone dataset

# Data accessed from here:
# https://wheregeospatial.com/agro-ecological-zones-ghana/
# Stored locally and unzipped

library(sf)
library(dplyr)

"~/Dropbox/data/agricultural/ghana/aez/Ghana_agroecologicalzones.shp" %>%
  read_sf() %>%
  st_transform(., crs = 4326) %>%
  st_write(., "inst/extdata/ghana_aez.geojson")
