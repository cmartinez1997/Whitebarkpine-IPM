## Making WBP data frame for survival model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com

# Load necessary packages -------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(tidyverse)

# Read in and process data ------------------------------------------------

# read wbp data and filter to only apply to whitebark pine trees (PIAL); SPCD = 101 in FIA

wbp_dat <- read_csv("data_processed/TREE_WBP_IW.csv") 

# Only keep remeasured trees (ones that have PREVDIA) coded as dead or alive
### STATUSCD = 0 = no status (not in sample)
### STATUSCD = 1 = live tree
### STATUSCD = 2 = dead tree
### STATUSCD = 3 = harvested tree
wbp_surv_dat <- wbp_dat %>%
  filter(!is.na(PREVDIA), STATUSCD %in% c(1,2)) #7817 trees 

# look up previous PLT_CN and CONDID and add columns to data frame
surv_dat <- wbp_surv_dat %>% 
  mutate(PREV_PLT_CN = wbp_dat$PLT_CN[match(PREV_TRE_CN, wbp_dat$TRE_CN)]) %>% 
  mutate(DIA_DIFF = wbp_surv_dat$DIA - wbp_surv_dat$PREVDIA) %>%  #diameter is dbh in inches
  mutate(PREV_CONDID = wbp_dat$CONDID[match(wbp_surv_dat$PREV_TRE_CN, wbp_dat$TRE_CN)])

surv_dat_final <- surv_dat %>% dplyr::select(TRE_CN, PLT_CN, PREV_TRE_CN, PREV_PLT_CN, DIA_DIFF, SPCD, INVYR, STATECD, 
                                                       UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, PREVCOND, STATUSCD, DIA, 
                                                       STOCKING, PREVDIA, PREV_CONDID, AGENTCD)

# Read in plot data and get coordinates and previous measurement year
plot_iw <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

# add columns, LAT/LON/ELEV/MEASYEAR/PREV_MEASYEAR/CENSUS_INTERVAL to wbp growth data frame from plot table, add census interval column
surv_dat_final_df <- surv_dat_final %>%  
  mutate(LAT = plot_iw$LAT[match(surv_dat_final$PLT_CN, plot_iw$CN)]) %>% 
  mutate(LON = plot_iw$LON[match(surv_dat_final$PLT_CN, plot_iw$CN)]) %>% 
  mutate(ELEV = plot_iw$ELEV[match(surv_dat_final$PLT_CN, plot_iw$CN)]) %>% 
  mutate(MEASYEAR = plot_iw$MEASYEAR[match(surv_dat_final$PLT_CN, plot_iw$CN)]) %>% 
  mutate(PREV_MEASYEAR = plot_iw$MEASYEAR[match(surv_dat_final$PREV_PLT_CN, plot_iw$CN)]) %>% 
  mutate(CENSUS_INTERVAL = MEASYEAR - PREV_MEASYEAR)

options(scipen = 999) #gets rid of scientific notation

# look up previous (tree-specific) condition-level BALIVE(Basal area in square feet per acre of all live trees) 
cond_iw <- read_csv("data_processed/COND_MT-ID-WY.csv") %>% 
  filter(COND_STATUS_CD == 1) # "accessible forest land" by FIA classification, go from 98,000 to 26,905

surv_dat_final_df$BALIVE <- apply(X = surv_dat_final_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                    MARGIN = 1, # applies function to each row in grow_data_remeas
                                    FUN = function(x, conds.df) {
                                      conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                        conds.df$CONDID %in% x["PREV_CONDID"]]
                                    },
                                    conds.df = cond_iw)

# also look up disturbance codes and add them to the dataframe
surv_dat_final_df$DSTRBCD1 <- apply(X = surv_dat_final_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in grData_remeas
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD1[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

surv_dat_final_df$DSTRBCD2 <- apply(X = surv_dat_final_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in grData_remeas
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD2[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

surv_dat_final_df$DSTRBCD3 <- apply(X = surv_dat_final_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in grData_remeas
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD3[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

# look to see what Disturbance codes exist in df, all disturbance codes are NA
unique_values <- surv_dat_final_df %>%
  distinct(DSTRBCD1, DSTRBCD2, DSTRBCD3) %>%
  unlist()
  # disturbance codes are listed: 20, 30, 10, 22, 12, 95, 31, 32, 60, 50, 46, 30
# disturbance codes are listed: 20, 30, 10, 22, 12, 95, 31, 32, 60, 50, 46, 30
# 0 - no visible damange
# 10 - insect damage
# 12 - insect damage to trees, seedlings, and saplings
# 20 - Disease damage
# 22 - disease damage to trees, including seedlings and saplings
# 30 - fire damage (from crown and ground fire, either prescribed or natural)
# 31 - ground fire damage
# 32 - crown fire damage
# 46 - domestic animal/livestock
# 50 - weather damage
# 60 - vegetation (suppression, competition, vines)
# 95 - earth movement/avalanches

#make BALIVE numeric instead of list so that NAs pop up and then remove the NA values 
surv_dat_final_df$BALIVE <- as.numeric(surv_dat_final_df$BALIVE)

surv_dat_final_df <- surv_dat_final_df %>% 
  filter(!is.na(BALIVE)) 

surv_dat_final_df$DIA_DIFF <- surv_dat_final_df$DIA - surv_dat_final_df$PREVDIA #getting the difference in diameters

# Create output data frame and write to csv
write_csv(surv_dat_final_df, "data_processed/WBP_surv.csv")

