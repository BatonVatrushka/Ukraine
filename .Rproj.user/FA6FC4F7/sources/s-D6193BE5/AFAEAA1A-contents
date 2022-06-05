library(pacman)
p_load(tmap, rgdal, sp, raster, ggmap)
tmap_options(check.and.fix = TRUE)

# read in the Europe Shape file
ukraine <- readOGR("Ukraine/UKR_adm0.shp")
roads <- readOGR('roads', 'roads')
rails <- readOGR('rails', 'railways')
water <- readOGR('waterways', 'waterways')


proj4string(ukraine)
proj4string(roads)

# quick map
tm_shape(ukraine) +
  tm_borders() +
  
  tm_shape(roads) +
  tm_lines(col = "grey60", alpha = 0.5) +
  
  tm_shape(water) +
  tm_lines(col = "blue", alpha = 0.25)

tmap_save(filename = "plot.html")

