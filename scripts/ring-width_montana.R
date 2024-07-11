##Making WBP rw data frame

library(dplyr)
library(tidyverse) # for data wrangling
library(here)
library(dplR)
library(shiny)
library(maps)
library(sf)
library(ggplot2)
library(viridis)

# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("data_raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("data_raw", "T_iwfia_whitebark.txt"))

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe

wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))

length(unique(wbp_rw$TRE_CN)) #219 unique trees with CN numbers - 219 samples of wbp that can be linked to trees - 219 trees

wbp_rw$TRE_CN <- as.character(wbp_rw$TRE_CN)
wbp_rw$PLT_CN <- as.character(wbp_rw$PLT_CN)


# Read tree data and subset PIAL (WBP)

tree_dat <- read_csv("data_raw/MT_TREE.csv")
tree_dat_wbp <- tree_dat %>% filter(SPCD == 101) #19,611 WBP trees in MT

### look up previous PLT_CN, and CONDID
tree_dat_wbp$PREV_PLT_CN <- tree_dat$PLT_CN[match(tree_dat_wbp$PREV_TRE_CN, tree_dat$CN)]
tree_dat_wbp$PREV_CONDID <- tree_dat$CONDID[match(tree_dat_wbp$PREV_TRE_CN, tree_dat$CN)]

# read in plot data to add to df
tree_plots <- read_csv("data_raw/MT_PLOT.csv")

#add new columns to data fram from plot table
tree_dat_wbp <- tree_dat_wbp %>%  
  mutate(LAT = tree_plots$LAT[match(tree_dat_wbp$PLT_CN, tree_plots$CN)]) %>% 
  mutate(LON = tree_plots$LON[match(tree_dat_wbp$PLT_CN, tree_plots$CN)]) %>% 
  mutate(ELEV = tree_plots$ELEV[match(tree_dat_wbp$PLT_CN, tree_plots$CN)]) %>% 
  mutate(MEASYEAR = tree_plots$MEASYEAR[match(tree_dat_wbp$PLT_CN, tree_plots$CN)]) %>% 
  mutate(PREV_MEASYEAR = tree_plots$MEASYEAR[match(tree_dat_wbp$PREV_PLT_CN, tree_plots$CN)]) %>% 
  mutate(REMEAS_PERIOD = tree_plots$REMPER[match(tree_dat_wbp$PLT_CN, tree_plots$CN)])

tree_dat_wbp <- tree_dat_wbp %>% 
  rename(TRE_CN = CN)

# look up previous (tree-specific) condition-level BALIVE(Basal area in square feet per acre of all live trees) 
conds_wbp <- read_csv("data_raw/MT_COND.csv")

tree_dat_wbp$BALIVE <- apply(X = tree_dat_wbp[, c("PREV_PLT_CN", "PREV_CONDID")], 
                               MARGIN = 1, # applies function to each row in wbp_survive_df
                               FUN = function(x, conds.df) {
                                 conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                   conds.df$CONDID %in% x["PREV_CONDID"]]
                               },
                               conds.df = conds_wbp)
# also look up disturbance codes and add them to the dataframe
tree_dat_wbp$DSTRBCD1 <- apply(X = tree_dat_wbp[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                 MARGIN = 1, # applies function to each row in wbp_survive_df
                                 FUN = function(x, conds.df) {
                                   conds.df$DSTRBCD1[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                       conds.df$CONDID %in% x["PREV_CONDID"]]
                                 },
                                 conds.df = conds_wbp)

tree_dat_wbp$DSTRBCD2 <- apply(X = tree_dat_wbp[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                 MARGIN = 1, # applies function to each row in wbp_survive_df
                                 FUN = function(x, conds.df) {
                                   conds.df$DSTRBCD2[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                       conds.df$CONDID %in% x["PREV_CONDID"]]
                                 },
                                 conds.df = conds_wbp)

tree_dat_wbp$DSTRBCD3 <- apply(X = tree_dat_wbp[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                 MARGIN = 1, # applies function to each row in grData_remeas
                                 FUN = function(x, conds.df) {
                                   conds.df$DSTRBCD3[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                       conds.df$CONDID %in% x["PREV_CONDID"]]
                                 },
                                 conds.df = conds_wbp)

##merge the inventory data from FIAdb with tree ring data
##read in metadata and rw files
wbp_meta <- read_csv("data_raw/T_iwfia_whitebark.txt")
wbp_rw <- read_csv("data_raw/T_iwfia_whitebark_rw.txt") # these data all suck basically


#need to filter trees from specific counties for our analysis
mt_meta <- wbp_meta %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) %>% 
  filter(STATECD == 30) %>% 
  filter(COUNTYCD %in% c(1, 7, 9, 23, 31, 39, 43, 57, 59, 67,81, 93, 95, 97)) #79 cores from these counties for our analysis

mt_meta_fiadb <- left_join(mt_meta, tree_dat_wbp)

mt_rw <- mt_rw %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) %>% 
  filter(STATECD == 30) %>% 
  filter(COUNTYCD %in% c(1, 7, 9, 23, 31, 39, 43, 57, 59, 67,81, 93, 95, 97)) #79 cores from these counties for our analysis

mt_rw <- left_join(mt_rw, mt_meta_fiadb)



##Map out where our ring width data are: 


m = map_data('state', region = ('Montana'))
# Loading map data
montana <- map_data("state", region = "Montana")
counties <- map_data("county")

spatial_df_rw <- mt_meta_fiadb %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON) %>%
  distinct() %>%  
  na.omit(spatial_df_rw)

m = map_data('state', region = ('Montana'))
montana_counties <- subset(counties, region == "montana")


ggplot() + 
  geom_polygon(data = m, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + 
  geom_point(data = spatial_df_rw, aes(x = LON, y = LAT), color = "turquoise4", alpha = 0.70) +
  geom_polygon(data = montana_counties, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  ggtitle("Whitebark pine SW Montana RW Data") +
  xlab('Longitude') +
  ylab('Latitude') +
  coord_fixed() +
  theme_bw()




rw_data_wbp <- mt_rw %>%
  ungroup() %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data_wbp <- spread(rw_data_wbp,key = "TRE_CN",value = RW,fill = NA,drop = FALSE) 


rw_data_wbp <- rw_data_wbp %>% 
  remove_rownames %>% 
  column_to_rownames(var="Year")

ncol(rw_data_wbp)

# Get column names of the dataframe
col_names <- colnames(rw_data_wbp)

# Remove the last 5 digits from each column name bc tucson decadal format can't handle more than 8 digits
new_col_names <- sub("\\d{5}$", "", col_names)

# Assign new column names to the dataframe
colnames(rw_data_wbp) <- new_col_names

#explore dataframe in dplR
rwl.report(rw_data_wbp) #descriptive statistics
rw_stats_wbp <- rwl.stats(rw_data_wbp)
head(rw_stats_wbp)

## write to text file tucson format

write.tucson(rw_data_wbp, "data_raw/rw_data_wbp_swmontana.txt", prec = 0.01, long.names = TRUE)


