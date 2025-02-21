## Cecilia Martinez 
## Nov 12 2024
## cecimartinez333@gmail.com




# load packages -----------------------------------------------------------

library(sf)
library(ggmap)
library(leaflet)
library(basemaps)
library(ggplot2)
library(tidyverse)
library(ggspatial)  
library(grid)    
library(patchwork)
library(tigris)
library(dplyr)


# reading in data ---------------------------------------------------------

# this is whitebark pine shapefile
wbp_range_data <- sf::st_read("WBP_Range_2014_v11/WBP_range_2014_d.shp") 
# converting to lat/lon
wbp_range_sf <- st_transform(wbp_range_data, crs = st_crs(4326))
st_bbox(wbp_range_sf)

bbox <- c(left = -128.00361, bottom = 36.45467, right = -108.65401, top = 55.41828)
wbp_map_area <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 2)
plot(wbp_range_sf)


