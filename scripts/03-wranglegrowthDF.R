## Making WBP data frame for growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com

# Load necessary packages -------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(ggthemr)

# Read in and process data ------------------------------------------------

# read tree data and filter to only apply to whitebark pine trees (PIAL); SPCD = 101 in FIA
grow_data <- read_csv("data_processed/TREE_MT-ID-WY.csv") #773058 total trees
grow_data_WBP <- filter(grow_data, SPCD == 101) #36,403 wbp trees
grow_data_WBP <- grow_data_WBP %>% 
  rename(TRE_CN = CN) #rename TRE_CN for CN in tree table

# write csv for only wbp species in tree table for interior west
write_csv(grow_data_WBP, "data_processed/TREE_WBP_IW.csv")

# Only keep remeasured trees
wbp_remeasured <- grow_data_WBP %>%
  filter(!is.na(PREVDIA)) #8380 trees that were remeasured

unique(wbp_remeasured$STATUSCD) # there are 3 status codes
# STATUSCD 0 = no status --> tree not presently in sample, tree was incorrectly tallied in previous inventory
# or it is located in nonsampled condition, these trees have NA current DIA, were measured but not anymore RECONCILECD = 7-9
# STATUSCD 1 = lived, STATUSCD 2 = died
wbp_remeasured %>% count(STATUSCD)
# 563 STATUSCD 0, 2846 STATUSCD 1 (lived), 4971 STATUSCD 2 (dead)

# only keep trees that grew and survived
wbp_growth_data <- wbp_remeasured %>% 
  filter(STATUSCD == 1) #(n=2846) trees alive in 2nd census interval

# look up previous PLT_CN and CONDID and add columns to data frame
wbp_growth_data <- wbp_growth_data %>% 
  mutate(PREV_PLT_CN = grow_data_WBP$PLT_CN[match(PREV_TRE_CN, grow_data_WBP$TRE_CN)]) %>% 
  mutate(DIA_DIFF = wbp_growth_data$DIA - wbp_growth_data$PREVDIA) %>%  #diameter is dbh in inches
  mutate(PREV_CONDID = grow_data_WBP$CONDID[match(wbp_growth_data$PREV_TRE_CN, grow_data_WBP$TRE_CN)])
         
wbp_growth_data <- wbp_growth_data %>% dplyr::select(TRE_CN, PLT_CN, PREV_TRE_CN, PREV_PLT_CN, DIA_DIFF, INVYR, STATECD, 
                                              UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, PREVCOND, STATUSCD, DIA, 
                                              STOCKING, PREVDIA, PREV_CONDID, AGENTCD)

# Read in plot data and get coordinates and previous measurement year
plot_iw <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

# add columns, LAT/LON/ELEV/MEASYEAR/PREV_MEASYEAR/CENSUS_INTERVAL to wbp growth data frame from plot table, add census interval column
wbp_growth_data_df <- wbp_growth_data %>%  
  mutate(LAT = plot_iw$LAT[match(wbp_growth_data$PLT_CN, plot_iw$CN)]) %>% 
  mutate(LON = plot_iw$LON[match(wbp_growth_data$PLT_CN, plot_iw$CN)]) %>% 
  mutate(ELEV = plot_iw$ELEV[match(wbp_growth_data$PLT_CN, plot_iw$CN)]) %>% 
  mutate(MEASYEAR = plot_iw$MEASYEAR[match(wbp_growth_data$PLT_CN, plot_iw$CN)]) %>% 
  mutate(PREV_MEASYEAR = plot_iw$MEASYEAR[match(wbp_growth_data$PREV_PLT_CN, plot_iw$CN)]) %>% 
  mutate(CENSUS_INTERVAL = MEASYEAR - PREV_MEASYEAR)

options(scipen = 999) #gets rid of scientific notation

# look up previous (tree-specific) condition-level BALIVE(Basal area in square feet per acre of all live trees) 
cond_iw <- read_csv("data_processed/COND_MT-ID-WY.csv") %>% 
  filter(COND_STATUS_CD == 1) # "accessible forest land" by FIA classification, go from 98,000 to 26,905

wbp_growth_data_df$BALIVE <- apply(X = wbp_growth_data_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                 MARGIN = 1, # applies function to each row in grow_data_remeas
                                 FUN = function(x, conds.df) {
                                   conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                     conds.df$CONDID %in% x["PREV_CONDID"]]
                                 },
                                 conds.df = cond_iw)
# also look up disturbance codes and add them to the dataframe
wbp_growth_data_df$DSTRBCD1 <- apply(X = wbp_growth_data_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                MARGIN = 1, # applies function to each row in grData_remeas
                                FUN = function(x, conds.df) {
                                  conds.df$DSTRBCD1[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                      conds.df$CONDID %in% x["PREV_CONDID"]]
                                },
                                conds.df = cond_iw)

wbp_growth_data_df$DSTRBCD2 <- apply(X = wbp_growth_data_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                MARGIN = 1, # applies function to each row in grData_remeas
                                FUN = function(x, conds.df) {
                                  conds.df$DSTRBCD2[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                      conds.df$CONDID %in% x["PREV_CONDID"]]
                                },
                                conds.df = cond_iw)

wbp_growth_data_df$DSTRBCD3 <- apply(X = wbp_growth_data_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                MARGIN = 1, # applies function to each row in grData_remeas
                                FUN = function(x, conds.df) {
                                  conds.df$DSTRBCD3[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                      conds.df$CONDID %in% x["PREV_CONDID"]]
                                },
                                conds.df = cond_iw)

#make BALIVE numeric instead of list so that NAs pop up and then remove the NA values 
wbp_growth_data_df$BALIVE <- as.numeric(wbp_growth_data_df$BALIVE)

wbp_growth_data_df <- wbp_growth_data_df %>% filter(!is.na(BALIVE)) #now we have 2719 trees with BALIVE predictor variable

# Annualize the diameter between the census intervals 
# note that growth increments need to be moved to the positive realm (by adding a constant)
# But only IF the log transform is used
wbp_growth_data_df$DIA_INCR_NEG <- wbp_growth_data_df$DIA_DIFF / wbp_growth_data_df$CENSUS_INTERVAL
range(wbp_growth_data_df$DIA_INCR_NEG)
# The range of values for dia_incr is -0.29 to 0.28

# Add values to positive realm
constant <- 0.29
wbp_growth_data_df$DIA_INCR <- wbp_growth_data_df$DIA_INCR_NEG + constant

# Create output data frame and write to csv
write_csv(wbp_growth_data_df, "data_processed/WBP_growth.csv")

