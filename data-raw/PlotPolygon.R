## code to prepare `PlotPolygon` dataset goes here

#### Packages libraries ####
library(sf)

# Create polygon
PlotPolygon <- st_as_sf(st_sfc(st_polygon(list(
  rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))))))

st_crs(PlotPolygon) <- 4326 # set fake crs

#### Save this test data in the package ####

usethis::use_data(PlotPolygon, overwrite = TRUE)
