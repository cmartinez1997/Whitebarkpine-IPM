## Creating maps of whitebark pine distribution in the interior west
## original code from: 
## updated and reformatted by Cecilia Martinez
## July 24 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------

library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)
library(viridis)
library(dplyr)
library(kableExtra)

# Load necessary data -----------------------------------------------------


# Read tree data and subset PIAL (WBP)

tree_dat <- read_csv("data_processed/TREE_WBP_IW.csv")
tree_dat_wbp_iw <- tree_dat %>% filter(SPCD == 101) #36,403 WBP trees 

# read in plot data to add to df
tree_plots <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

#add new columns to data fram from plot table
tree_dat_wbp_iw <- tree_dat_wbp_iw %>%  
  mutate(LAT = tree_plots$LAT[match(tree_dat_wbp_iw$PLT_CN, tree_plots$CN)]) %>% 
  mutate(LON = tree_plots$LON[match(tree_dat_wbp_iw$PLT_CN, tree_plots$CN)]) %>% 
  mutate(ELEV = tree_plots$ELEV[match(tree_dat_wbp_iw$PLT_CN, tree_plots$CN)])

write_csv(tree_dat_wbp_iw, "data_processed/tree_dat_wbp_iw.csv")
##merge the inventory data from FIAdb with tree ring 

wbp_rw <- read_csv(here::here("data_raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("data_raw", "T_iwfia_whitebark.txt"))

wbp_meta <- wbp_meta %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN)) #219 unique trees for which we have tree ring data that have a TRE_CN
##hmm for some reason, not all lat/lons are populating, 
## ok there are some points where there is no previous tree CN and there are no associated lat/lons There are CNs in the metadata file but when I try to merge them with 
## fiadb cn from the tree table, there is no match, is this an error in tree_cns?


wbp_fiadb <- left_join(wbp_meta, tree_dat_wbp_iw, by = c("TRE_CN")) %>% 
  select(CN, STATECD.x, COUNTYCD.x, PLOT.x, FIADB_PLOT, SUBP.x, TREE.x, DIA.x, MEASYEAR, LAT, LON, ELEV, CNT_HT, CNT_LGTH, CNT_X, PITH, RINGCOUNT, VERIFY, REVISIT, INVYR, PLT_CN.x, TRE_CN) %>% 
  rename(DIA = DIA.x) %>% 
  rename(CORE_CN = CN) %>% 
  rename(PLOT_CN = PLT_CN.x) %>% 
  rename(PLOT = PLOT.x) %>% 
  rename(COUNTYCD = COUNTYCD.x) %>% 
  rename(STATECD = STATECD.x) %>% 
  rename(SUBP = SUBP.x) %>% 
  rename(TREE = TREE.x)
  
write_csv(wbp_fiadb, "data_processed/wbp_meta_2018.csv")
  
  


#if (388911076489998 %in% tree_dat_wbp_iw$PREV_TRE_CN ) {
#  print("Value exists in the DataFrame")
#} else {
#  print("Value does not exist in the DataFrame")
#}



# wbp_fiadb <- left_join(wbp_meta, tree_dat_wbp_iw) #103 tres filter
# wbp_fiadb <- left_join(wbp_meta, tree_dat_wbp_iw, by = c("STATECD", "COUNTYCD", "PLOT", "SUBP", "TREE", "SPCD", "TRE_CN")) #
wbp_fiadb <- wbp_fiadb %>% 
  filter(!is.na(LAT) & !is.na(LON)) #159 plots have lat/lon associated with them - 159 total observations that can be linked bc CN is time varying
  # weird, bc if i select to join just by TRE_CN, I get 210 trees with total observations but when I do this with the other join function I get, much less treees


## Do some data exploration on wbp_fiadb
sum(wbp_fiadb$STATECD.x == 56) #70 in WY
sum(wbp_fiadb$STATECD.x == 30) #112 in MT
sum(wbp_fiadb$STATECD.x == 16) #28 in ID

occurences_counties <- wbp_fiadb %>%
  group_by(STATECD.x, COUNTYCD.x) %>%
  summarize(count = n()) %>%
  ungroup()

#filter just for mt
occurrence_table <- occurences_counties %>% 
  filter(STATECD.x == 30) %>% 
  filter(count >= 7) %>% 
  arrange(desc(count)) %>%
  mutate(STATECD.x = as.character(STATECD.x),  
         COUNTYCD.x = as.character(COUNTYCD.x)) %>%  
  kable(caption = "Occurrences by County/State") %>%
  kable_styling(full_width = FALSE)  

print(occurrence_table)


# okay this adds the state codes so we filter out hte ring widths thaat dont have state codes or county codes associated withthem, flathead county specifically
wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))


wbp_rw <- wbp_rw %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) 

wbp_rw <- left_join(wbp_rw, wbp_fiadb)

#write data to csv so don't have to mess with this table ever again
write_csv(wbp_rw, "data_processed/wbp_rw.csv")
write_csv(wbp_fiadb, "data_processed/wbp_fiadb.csv")


# MAPPING



# Making maps -------------------------------------------------------------

##Making Maps
m = map_data('state', region = c('Idaho', "Wyoming", "Montana"))

spatial_wbp_iw <- wbp_fiadb %>%
  ungroup() %>%
  select(TRE_CN, LAT,LON, STATECD.x) %>%
  distinct() %>%  
  na.omit(spatial_wbp_iw)


# Create a data frame for the reference chronology points in itrdb
reference_point_mt172 <- data.frame(LAT = 48.8333, LON = -111.5) #MT 172 #Sauchyn
reference_point_mt158 <- data.frame(LAT = 45.1667, LON = -109.5167) #MT 158 #Graumlich
reference_point_wy071 <- data.frame(LAT = 44.914, LON = -109.573) #WY 071 #Blomdahl 
reference_point_wy033 <- data.frame(LAT = 44.75, LON = -110.25) #WY 033 #King
reference_point_wy050 <- data.frame(LAT = 44.8, LON = -110.4333) #WY 050 #King
reference_point_id012 <- data.frame(LAT = 44.13, LON = -114.55) #ID012 #Perkins
reference_point_id010 <- data.frame(LAT = 43.97, LON = -114.97) #ID010  #Perkins
reference_point_id009 <- data.frame(LAT = 44.6, LON = -114.45) #ID009 #Perkins


# map it out
ggplot() + 
  geom_polygon(data = m, aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_point(data=spatial_wbp_iw,aes(x=LON,y=LAT,
                                     color=factor(STATECD.x),
                                     alpha = .9)) +
  geom_point(data = reference_point_mt172, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_mt158, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_wy071, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_wy033, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_wy050, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_id012, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_id010, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_id009, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  xlab('Longitude') +
  ylab('Latitude') +
  coord_fixed() + 
  theme_bw() + 
  ditch_the_axes

#158 trees 




















ditch_the_axes <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  #panel.border = element_blank(),
  panel.grid = element_blank(),
  #axis.title = element_blank()
)

ggplot()+
  geom_polygon(data = m, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATECD),alpha = .9)) +
  #scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
  #   "Engelmann spruce" = "gold1")) +
  theme_bw() +
  ditch_the_axes

#elev & climate with distribution

