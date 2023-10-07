## Making WBP data frame for survival model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(wesanderson)
library(tidyverse)


# Read in and process data ------------------------------------------------

# read tree data and filter to only apply to whitebark pine trees (PIAL); SPCD = 101 in FIA
tree_data <- read_csv("data_processed/TREE_MT-ID-WY.csv") #773058 total trees
tree_data_WBP <- filter(tree_data, SPCD == 101) #36,403 wbp trees
tree_data_WBP <- tree_data_WBP %>% 
  rename(TRE_CN = CN) #rename TRE_CN for CN in tree table

# Only keep remeasured trees coded as dead or alive
### STATUSCD = 0 = no status (not in sample)
### STATUSCD = 1 = live tree
### STATUSCD = 2 = dead tree
### STATUSCD = 3 = harvested tree
wbp_remeasured <- tree_data_WBP %>%
  filter(!is.na(PREVDIA), STATUSCD %in% c(1,2)) #7817 trees 

wbp_survive <- wbp_remeasured

# look up previous PLT_CN and CONDID and add columns to data frame
wbp_survive_df <- wbp_survive %>% 
  mutate(PREV_PLT_CN = tree_data_WBP$PLT_CN[match(PREV_TRE_CN, tree_data_WBP$TRE_CN)]) %>% 
  mutate(DIA_DIFF = wbp_survive$DIA - wbp_survive$PREVDIA) %>%  #diameter is dbh in inches
  mutate(PREV_CONDID = tree_data_WBP$CONDID[match(wbp_survive$PREV_TRE_CN, tree_data_WBP$TRE_CN)])

wbp_survive_df <- wbp_survive_df %>% dplyr::select(TRE_CN, PLT_CN, PREV_TRE_CN, PREV_PLT_CN, DIA_DIFF, SPCD, INVYR, STATECD, 
                                                       UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, PREVCOND, STATUSCD, DIA, 
                                                       STOCKING, PREVDIA, PREV_CONDID, AGENTCD)

# Read in plot data and get coordinates and previous measurement year
plot_iw <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

# add columns, LAT/LON/ELEV/MEASYEAR/PREV_MEASYEAR/CENSUS_INTERVAL to wbp growth data frame from plot table, add census interval column
wbp_survive_df <- wbp_survive_df %>%  
  mutate(LAT = plot_iw$LAT[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(LON = plot_iw$LON[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(ELEV = plot_iw$ELEV[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(MEASYEAR = plot_iw$MEASYEAR[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(PREV_MEASYEAR = plot_iw$MEASYEAR[match(wbp_survive_df$PREV_PLT_CN, plot_iw$CN)]) %>% 
  mutate(CENSUS_INTERVAL = MEASYEAR - PREV_MEASYEAR)

options(scipen = 999) #gets rid of scientific notation

# look up previous (tree-specific) condition-level BALIVE(Basal area in square feet per acre of all live trees) 
cond_iw <- read_csv("data_processed/COND_MT-ID-WY.csv") %>% 
  filter(COND_STATUS_CD %in% c(1,2))

wbp_survive_df$BALIVE <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                    MARGIN = 1, # applies function to each row in wbp_survive_df
                                    FUN = function(x, conds.df) {
                                      conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                        conds.df$CONDID %in% x["PREV_CONDID"]]
                                    },
                                    conds.df = cond_iw)
# also look up disturbance codes and add them to the dataframe
wbp_survive_df$DSTRBCD1 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in wbp_survive_df
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD1[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

wbp_survive_df$DSTRBCD2 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in wbp_survive_df
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD2[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

wbp_survive_df$DSTRBCD3 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in grData_remeas
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD3[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

#make BALIVE numeric instead of list so that NAs pop up and then remove the NA values 
wbp_survive_df$BALIVE <- as.numeric(wbp_survive_df$BALIVE)

wbp_survive_df <- wbp_survive_df %>% filter(!is.na(BALIVE)) #now we have 7434 trees with BALIVE predictor variable
  #hmm some values are 0 for BALIVE, this is the inital BALIVE condition, what does this mean? were they on a non-forested condition
  #explore what the condition was of these trees, are they in krummholz and <10% stocked, in which case they are not forested

#Search for CONDIDs
### CONDID = 1 = no status (not in sample)
### CONDID = 2 = Accessible nonforested land (<10% tally trees) 
### CONDID = 3 = Noncensused water? why the heck are these in there, when they have clearly been censused

### look at mortality, by PREV_MEASYEAR and MEASYEAR
surv_table1 <- table(wbp_survive_df[, c("PREV_MEASYEAR", "STATUSCD")])
wbp_survival1 <- surv_table1[,1]/(surv_table1[,1]+surv_table1[,2]) #calculates survival probabilities from the 1st census within the population at that time, for each year
  # survival probability ranges from 0.2307 in 2010, to 0.5652 in 2011

surv_table2 <- table(wbp_survive_df[, c("MEASYEAR", "STATUSCD")])
wbp_survival2 <- surv_table2[,1]/(surv_table2[,1]+surv_table2[,2])
# survival ranges from 0.2498 to 0.4756 (lower overall survival in later years, trending downwards)



# Create output data frame and write to csv
write_csv(wbp_grow_survive_df, "data_processed/WBP_growth.csv")

