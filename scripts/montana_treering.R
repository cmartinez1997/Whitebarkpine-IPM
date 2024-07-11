## Creating maps of whitebark pine tree ring data in the interior west
## July 24 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------

library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)
library(viridis)
library(dplyr)
library(terra)

# Load necessary data -----------------------------------------------------
# load in tree level data from fiadb

tree_dat_wbp_mt <- read_csv("data_processed/tree_dat_wbp_mt.csv")

# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("data_raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("data_raw", "T_iwfia_whitebark.txt"))


## merge the inventory data from FIAdb with tree ring 

mt_data <-  wbp_meta %>% 
  filter(STATECD == 30) #170 samples

mt_data <- wbp_meta %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) %>% 
  filter(STATECD == 30)  #only 116 samples for montana

mt_fiadb <- left_join(mt_data, tree_dat_wbp_mt)

# okay this adds the state codes so we filter out hte ring widths thaat dont have state codes or county codes
wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))

mt_rw <- wbp_rw %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) %>% 
  filter(STATECD == 30)  #70 cores from these counties for our analysis

mt_rw <- left_join(mt_rw, mt_fiadb). # montana ring width data frame

#write data to csv so don't have to mess with this table ever again
write_csv(mt_rw, "data_processed/mt_rw.csv")
write_csv(mt_fiadb, "data_processed/mt_fiadb.csv")


##Map out where our ring width data are: 

# MAPPING

spatial_df_mt <- mt_fiadb %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON) %>%
  distinct() %>%  
  na.omit(spatial_df_mt)

# make table from dataframe for lat longs in mt
kable(spatial_df_mt, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray") %>%
  column_spec(1:3, bold = TRUE)


m = map_data('state', region = ('Montana'))
montana_counties <- subset(counties, region == "montana")

# Create a data frame for the reference MT172 chronology points
reference_point_mt172 <- data.frame(LAT = 48.8333, LON = -111.5) #MT 172
reference_point_mt158 <- data.frame(LAT = 45.1667, LON = -109.5167) #MT 158

ggplot() + 
  geom_polygon(data = m, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + 
  geom_point(data = spatial_df_mt, aes(x = LON, y = LAT), color = "turquoise4", alpha = 0.70) +
  geom_point(data = reference_point_mt172, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_mt158, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_polygon(data = montana_counties, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  xlab('Longitude') +
  ylab('Latitude') +
  coord_fixed() +
  theme_bw()
