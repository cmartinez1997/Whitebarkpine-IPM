## Cecilia Martinez 
## July 26 2024
## cecimartinez333@gmail.com




# load packages -----------------------------------------------------------

library(sf)
library(dplyr)
library(leaflet)
library(basemaps)
library(ggplot2)
library(tidyverse)
library(ggspatial)  
library(grid)    
library(patchwork)
library(leaflet_extras)
library(tigris)



# reading in data ---------------------------------------------------------

wbp_range_data <- sf::st_read("WBP_Range_2014_v11/WBP_range_2014_d.shp") 
wbp_range_data_transformed <- st_transform(wbp_range_data, crs = st_crs(3857))


# Set the basemap defaults
set_defaults(map_service = "esri", map_type = "world_topo_map")

# transform the bounding box to the correct CRS (EPSG 3857)
# this uses basemaps

wbp_ext <- draw_ext()
wbp_ext_transformed <- st_transform(wbp_ext, crs = st_crs(3857))


# Prepare the FIA plots data
whitebark_fia_map <- whitebark_pine_repeats  %>% 
  select(TREE_COMPOSITE_ID, LAT, LON, STATECD, INVYR) %>% 
  group_by(TREE_COMPOSITE_ID, LAT, LON, STATECD) %>% 
  mutate(STATECD = as.factor(STATECD)) %>% 
  ungroup()

# Make spatial
whitebark_fia_sf <- st_as_sf(whitebark_fia_map, coords = c("LON", "LAT"), crs = 4326) %>%
  st_transform(crs = st_crs(3857))

# get the extent of the fia data as a sf polygon
fia_extent_sf <- st_as_sfc(st_bbox(whitebark_fia_sf), crs = st_crs(3857))

#clip polygon we want to zoom in on
wbp_range_clipped <- st_intersection(wbp_range_data_transformed, fia_extent_sf)

#okay now read in tree ring data

tree_ring_map <- read_csv("data_processed/wbp_fiadb.csv")
tree_ring_map <- tree_ring_map %>% 
  select(c(CN, LAT, LON))

treering_points_sf <- st_as_sf(tree_ring_map, coords = c("LON", "LAT"), crs = st_crs(4326)) %>%
  st_transform(crs = st_crs(3857))  # Transform to the same CRS as the FIA data

treering_extent_sf <- st_as_sfc(st_bbox(treering_points_sf), crs = st_crs(3857))

# Calculate combined bounding box manually
xmin <- -12993790
xmax <- -12139280
ymin <- 5253893
ymax <- 6273750

# corners of the bounding box
bbox_corners <- matrix(c(xmin, ymin,
                         xmax, ymin,
                         xmax, ymax,
                         xmin, ymax,
                         xmin, ymin), # Close the polygon by repeating the first point
                       ncol = 2, byrow = TRUE)


# sf polygon from the bounding box
bbox_polygon <- st_polygon(list(bbox_corners))
bbox_sf <- st_sfc(bbox_polygon, crs = 3857)  # wgs 84 / Pseudo-Mercator CRS

# Convert the single polygon into an sf object
bbox_sf_object <- st_sf(geometry = bbox_sf)
bbox_for_basemap <- st_bbox(bbox_sf_object)

# Convert to an sf bbox for use in basemap_gglayer or other functions



wbp_range_clipped <- st_intersection(wbp_range_data_transformed, bbox_sf_object)


main_plot <- ggplot() +
  basemaps::basemap_gglayer(bbox_for_basemap) +
  geom_sf(data = bbox_sf_object, color = "darkred", fill = "darkred", alpha = 0.5) +
  geom_sf(data = whitebark_fia_sf, size = 1.5, color = "orange") + # Adding FIA points
  geom_sf(data = treering_points_sf, size = 1.5, color = "green") + # Adding FIA points
  labs(title = "Remeasured Whitebark Pine (WBP) Distribution Map") +
  coord_sf(crs = st_crs(3857)) +
  scale_fill_identity() + 
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 30, l = 5, unit = "pt")
  ) 


inset_plot <- ggplot() +
  basemaps::basemap_gglayer(wbp_ext_transformed) +
  geom_sf(data = wbp_range_data, color = "#145A32", fill = "#145A32", alpha = 0.25) +
  geom_rect(aes(xmin = bbox_for_basemap$xmin, xmax = bbox_for_basemap$xmax, ymin = bbox_for_basemap$ymin, ymax = bbox_for_basemap$ymax),
            fill = NA, color = "darkred") +
  coord_sf(crs = st_crs(3857)) +
  scale_fill_identity() + 
  theme_void() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )

north_arrow <- ggplot() +
  coord_sf(crs = st_crs(3857), datum = NA) + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_void()

scale_bar <- ggplot() +
  coord_sf(crs = st_crs(3857), datum = NA) + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  theme_void()


final_plot <- (main_plot + inset_plot) / 
  (north_arrow + scale_bar) +
  plot_layout(heights = c(4, 1))

# Print the final plot
print(final_plot)


# make this look better with leaflet maybe? 

whitebark_fia_leaflet <- st_as_sf(whitebark_fia_map, coords = c("LON", "LAT"), crs = 4326) 
treering_points_leaflet <- st_as_sf(tree_ring_map, coords = c("LON", "LAT"), crs = st_crs(4326))
wbp_range_data_leaflet <- st_transform(wbp_range_data_transformed, crs = st_crs(4326))
wbp_range_data_leaflet <- st_simplify(wbp_range_data_leaflet, preserveTopology = TRUE)
bbox_sf_object_leaflet <- st_transform(bbox_sf_object, crs = st_crs(4326))
plot(st_geometry(wbp_range_data_leaflet))

#getting state boundaries
state_boundaries <- states(cb = TRUE, resolution = "20m")
state_boundaries <- st_transform(state_boundaries, crs = st_crs(4326))


north_arrow <- htmltools::HTML('<div style="position: absolute; top: 10px; left: 10px; z-index: 9999;">
                                  <svg width="40" height="40" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                                    <path d="M12 2L15 8L9 8L12 2ZM12 22V10H11V22H12Z" fill="black"/>
                                  </svg>
                                </div>')

# Start a leaflet map
wbp_map <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%  # adding topo basemap
  setView(lng = mean(st_coordinates(treering_points_leaflet)[,1]), lat = mean(st_coordinates(treering_points_leaflet)[,2]), zoom = 6) %>%  # set the initial view
  # for the range map 
  addPolygons(data = wbp_range_data_leaflet, 
              weight = 2, 
              color = "#17202A", 
              fillColor = "#145A32", 
              fillOpacity = 0.25) %>% 
 
  addPolygons(data = bbox_sf_object_leaflet, 
              weight = 5, 
              color = "darkred", 
              fillOpacity = 0) %>% 
  
  addPolylines(data = state_boundaries, 
               color = "black", 
               weight = 1, 
               opacity = 1) %>% 
  # for the tree ring data

  # for the FIA data
  addCircles(data = whitebark_fia_leaflet,
                   radius = 2,
                   color = "#1F618D",
                   fillOpacity = 0.6) %>% 
  addCircles(data = treering_points_leaflet,
             radius = 2,
             color = "#DC4D01",  # Ensuring both color and fillColor are set
             fillOpacity = 1) %>%
 
  
  # adding an inset map 
  addMiniMap(
             toggleDisplay = TRUE,
             position = "topright",
             width = 100, height = 100,
             zoomLevelOffset = -3.5) %>% 
  addScaleBar("bottomright") %>% 
# adding a legend
  addLegend(position = "bottomright", 
            colors = c("transparent", "transparent", "transparent", "transparent"), 
            labels = c(htmltools::HTML("<svg width='10' height='10'>
                                         <rect width='10' height='10' style='fill:#73BE73;'/>
                                       </svg> WBP Range"),
                       htmltools::HTML("<svg width='10' height='10'>
                                         <circle cx='5' cy='5' r='5' style='fill:#1F618D;'/>
                                       </svg> WBP FIA remeasurement data"),
                       htmltools::HTML("<svg width='10' height='10'>
                                         <circle cx='5' cy='5' r='5' style='fill:#DC4D01;'/>
                                       </svg> WBP tree-ring data"), 
                       htmltools::HTML("<svg width='20' height='10'>
                                         <line x1='0' y1='5' x2='20' y2='5' style='stroke:black;stroke-width:2'/>
                                       </svg> State Boundaries")), 
            opacity = 0.6)


  # addLegend(position = "bottomright",
  #           colors = c("darkred", "cadetblue", "purple4"),
  #           labels = c("Species Range", "Whitebark Pine FIA data", "Whitebark Pine tree-ring data"),
  #           opacity = 0.5)



mapshot(main_map, file = 'wbp_map.png')
