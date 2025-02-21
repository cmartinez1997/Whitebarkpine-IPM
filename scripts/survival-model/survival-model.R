## Creating survival model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------
library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)
library(performance) #to check model 
library(effects)
library(sp)
library(raster)
library(terra)

# read in surv data -------------------------------------------------------

wbp_surv_dat <- read_csv("data_processed/WBP_surv.csv")

# do a little bit of exploration about distributions

# distribution of size...update to evaluate PREVDIA
hist(wbp_surv_dat$PREVDIA) # size is lognormal-ish, with a wonky bit at the small end of the scale (due to min size threshold?) only measure certain DBH
# lognormal dist Range (0,infinity), continuous, right skew, size
hist(wbp_surv_dat$PREVDIA, breaks = c(seq(0, 40, by = 1)), xlim = c(0, 40))
hist(log(wbp_surv_dat$PREVDIA))
  # okay this looks kinda weird (b/c of mon size threshold)

# distribution of other predictors
hist(wbp_surv_dat$BALIVE) # not too bad...Poisson-ish but with a large mean count
hist(log(wbp_surv_dat$BALIVE)) # log transform has a heavy left tail
  # what to do about this

# add log transforms to the data frame
wbp_surv_dat$log_size <- log(wbp_surv_dat$PREVDIA)
wbp_surv_dat$log_BALIVE <- log(wbp_surv_dat$BALIVE)

# Recode status for mortality and for survival instead of just status code
wbp_surv_dat$surv <- ifelse(wbp_surv_dat$STATUSCD == 2, 0, 1)
wbp_surv_dat$mort <- ifelse(wbp_surv_dat$STATUSCD == 1, 0, 1)
#7434 trees

# remove cases where BALIVE at time 1 = zero (should be impossible)
wbp_surv_dat2 <- wbp_surv_dat %>% 
  filter(BALIVE > 0) # goes from 7537 to 7136 observations

# look to see what disturbance codes exist in df, all disturbance codes are NA
unique_values <- wbp_surv_dat2 %>%
  distinct(DSTRBCD1, DSTRBCD2, DSTRBCD3) %>%
  unlist()
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
  # 80 - human induced damage
  # 95 - earth movement/avalanches

# remove conditions where fire or harvest occurred, what about insect damage for mortality analysis? ask Margaret about this
wbp_surv_dat3 <- wbp_surv_dat2 %>%
  filter(if_all(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ !(. %in% c(10, 12, 20, 22, 30, 31, 32, 80))))
  
# goes down to 5,574 observations

# standardize covariates, center and scale
wbp_surv_dat_scaled <- wbp_surv_dat %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -SPCD, -STATECD, -UNITCD, -INVYR, 
                                                                      -COUNTYCD, -PLOT, -SUBP, -TREE, -CONDID, -PREVCOND, -PREV_CONDID, 
                                                                      -STATUSCD, -MEASYEAR, -PREV_MEASYEAR, 
                                                                      -REMEAS_PERIOD, 
                                                                      AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                                
                                                                      -surv, -mort))

wbp_surv_dat2_scaled <- wbp_surv_dat2 %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -SPCD, -STATECD, -UNITCD, -INVYR, 
                                                                        -COUNTYCD, -PLOT, -SUBP, -TREE, -CONDID, -PREVCOND, -PREV_CONDID, 
                                                                        -STATUSCD, -MEASYEAR, -PREV_MEASYEAR, 
                                                                        -REMEAS_PERIOD,
                                                                        AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                          
                                                                        -surv, -mort))

wbp_surv_dat3_scaled <- wbp_surv_dat3 %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -SPCD, -STATECD, -UNITCD, -INVYR, 
                                                                 -COUNTYCD, -PLOT, -SUBP, -TREE, -CONDID, -PREVCOND, -PREV_CONDID, 
                                                                 -STATUSCD, -MEASYEAR, -PREV_MEASYEAR, 
                                                                 -REMEAS_PERIOD,
                                                                 AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                                 -surv, -mort))



# okay now add MAT and MAP
# make growth data spatial 
surv_spat <- SpatialPointsDataFrame(coords = cbind(wbp_surv_dat2$LON, wbp_surv_dat2$LAT), 
                                    data = wbp_surv_dat2, 
                                    proj4string = CRS("+proj=longlat +datum=NAD83"))

coords <- cbind(surv_spat$LON, surv_spat$LAT)  # Extract coordinates

#now read in climate tiffs to include them in the model, but only normals for the census interval plannes 
# Read in climate tiffs
clim_ppt <- terra::rast("PRISM/data_formatted/new_wbp_pptStack.tif")
clim_tmean <- terra::rast("PRISM/data_formatted/new_wbp_tmeanStack.tif")

ppt_extr_surv <- terra::extract(clim_ppt, coords)
tmean_extr_surv <- terra::extract(clim_tmean, coords)

# Export climate data
processed_path <- "data_processed/"
write_csv(ppt_extr_surv, paste0(processed_path, "ppt_extr_surv.csv"))
write_csv(tmean_extr_surv, paste0(processed_path, "tmean_extr_surv.csv"))

# Adding sensible column names
ppt_extr_df <- as.data.frame(ppt_extr_surv)
tmean_extr_df <- as.data.frame(tmean_extr_surv)

PRISM_path <- "PRISM/data/"
ppt_files <- list.files(path = PRISM_path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)

colNames <- lapply(strsplit(ppt_files, "4kmM[23]_"), function(x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function(x) x[1])
colNames <- unlist(colNames)

# Assign meaningful column names
colnames(ppt_extr_df) <- paste0("ppt_", colNames)
colnames(tmean_extr_df) <- paste0("tmean_", colNames)

# Add coordinates to dataframes
ppt_extr_df <- data.frame(LON = surv_spat$LON, LAT = surv_spat$LAT, ppt_extr_df)
tmean_extr_df <- data.frame(LON = surv_spat$LON, LAT = surv_spat$LAT, tmean_extr_df)

# Subset columns for years >= 2003 before merging (optimization)
ppt_cols_to_keep <- c("LON", "LAT", grep("^ppt_200[3-9]|^ppt_20[1-9][0-9]", colnames(ppt_extr_df), value = TRUE))
tmean_cols_to_keep <- c("LON", "LAT", grep("^tmean_200[3-9]|^tmean_20[1-9][0-9]", colnames(tmean_extr_df), value = TRUE))

ppt_extr_df <- ppt_extr_df[, ppt_cols_to_keep]
tmean_extr_df <- tmean_extr_df[, tmean_cols_to_keep]

# Merge climate dataframes with survival data
ppt_df <- merge(ppt_extr_df, wbp_surv_dat2[, c("LON", "LAT", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))
tmean_df <- merge(tmean_extr_df, wbp_surv_dat2[, c("LON", "LAT", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))

# Wrangle `ppt_df` into long format
ppt_df_long <- ppt_df %>%
  pivot_longer(
    cols = starts_with("ppt_"),
    names_to = "MONTH_YEAR",
    values_to = "PRECIP"
  ) %>%
  mutate(
    MONTH_YEAR = sub("ppt_", "", MONTH_YEAR),
    YEAR = as.numeric(substr(MONTH_YEAR, 1, 4)),
    MONTH = as.numeric(substr(MONTH_YEAR, 5, 6))
  )

ppt_df_long$PLT_CN <- as.character(ppt_df_long$PLT_CN)
ppt_df_long$TRE_CN <- as.character(ppt_df_long$TRE_CN)

# Filter for years >= 2003
ppt_df_filt <- ppt_df_long %>%
  filter(YEAR >= 2003)

# Aggregate MAP
map_per_tree_year <- ppt_df_filt %>%
  group_by(TRE_CN, YEAR) %>%
  summarise(MAP = sum(PRECIP, na.rm = TRUE)) %>%
  ungroup()

ppt_df_filt <- ppt_df_filt %>%
  dplyr::select(-MONTH_YEAR, -PRECIP, -MONTH) %>%
  distinct(TRE_CN, YEAR, .keep_all = TRUE) %>%
  left_join(map_per_tree_year, by = c("TRE_CN", "YEAR"))

# Wrangle `tmean_df` into long format
tmean_df_long <- tmean_df %>%
  pivot_longer(
    cols = starts_with("tmean_"),
    names_to = "MONTH_YEAR",
    values_to = "TEMP"
  ) %>%
  mutate(
    MONTH_YEAR = sub("tmean_", "", MONTH_YEAR),
    YEAR = as.numeric(substr(MONTH_YEAR, 1, 4)),
    MONTH = as.numeric(substr(MONTH_YEAR, 5, 6))
  )

tmean_df_long$PLT_CN <- as.character(tmean_df_long$PLT_CN)
tmean_df_long$TRE_CN <- as.character(tmean_df_long$TRE_CN)

# Filter for years >= 2003
tmean_df_filt <- tmean_df_long %>%
  filter(YEAR >= 2003)

# Aggregate MAT
mat_per_tree_year <- tmean_df_filt %>%
  group_by(TRE_CN, YEAR) %>%
  summarise(MAT = mean(TEMP, na.rm = TRUE)) %>%
  ungroup()

tmean_df_filt <- tmean_df_filt %>%
  dplyr::select(-MONTH_YEAR, -TEMP, -MONTH) %>%
  distinct(TRE_CN, YEAR, .keep_all = TRUE) %>%
  left_join(mat_per_tree_year, by = c("TRE_CN", "YEAR"))

wbp_surv_dat2$TRE_CN <- as.character(wbp_surv_dat2$TRE_CN)

wbp_surv_dat2 <- wbp_surv_dat2 %>%
  mutate(
    START_YEAR = MEASYEAR - REMEAS_PERIOD, 
    END_YEAR = MEASYEAR                     
  )

wbp_surv_dat2$TRE_CN <- as.character(wbp_surv_dat2$TRE_CN)
ppt_df_filt$TRE_CN <- as.character(ppt_df_filt$TRE_CN)
# Final aggregation for MAP and MAT
merged_data <- wbp_surv_dat2 %>%
  mutate(START_YEAR = MEASYEAR - round(REMEAS_PERIOD, 1), END_YEAR = MEASYEAR) %>%
  dplyr::select(TRE_CN, START_YEAR, END_YEAR, REMEAS_PERIOD) %>%
  distinct() %>%
  left_join(ppt_df_filt, by = "TRE_CN") %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAP_data <- merged_data %>%
  group_by(TRE_CN) %>%
  summarise(mean_MAP = mean(MAP, na.rm = TRUE))

final_data <- wbp_surv_dat2 %>%
  left_join(mean_MAP_data, by = "TRE_CN") %>%
  mutate(MAP = mean_MAP / round(REMEAS_PERIOD))

# Repeat for MAT
merged_data_tmean <- wbp_surv_dat2 %>%
  dplyr::select(TRE_CN, START_YEAR, END_YEAR, REMEAS_PERIOD) %>%
  distinct() %>%
  left_join(tmean_df_filt, by = "TRE_CN") %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAT_data <- merged_data_tmean %>%
  group_by(TRE_CN) %>%
  summarise(mean_MAT = mean(MAT, na.rm = TRUE))

final_data <- final_data %>%
  left_join(mean_MAT_data, by = "TRE_CN") %>%
  mutate(MAT = mean_MAT / round(REMEAS_PERIOD))

# Standardize numerical covariates
model_surv_data <- final_data %>%
  dplyr::select(TRE_CN, PREV_TRE_CN, PLT_CN, PREV_PLT_CN, LAT, LON, ELEV,
                DIA, PREVDIA, DIA_DIFF, MEASYEAR, PREV_MEASYEAR, REMEAS_PERIOD,
                BALIVE, CONDID, STATUSCD, DSTRBCD1, DSTRBCD2, DSTRBCD3, AGENTCD, MAT, MAP, surv, mort)

scaled_surv_data <- model_surv_data %>%
  mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                -MEASYEAR, -PREV_MEASYEAR, -REMEAS_PERIOD, -STATUSCD, -surv, -mort))

scaled_surv_data$PREV_PLT_CN <- as.character(scaled_surv_data$PREV_PLT_CN)
scaled_surv_data$PREV_TRE_CN <- as.character(scaled_surv_data$PREV_TRE_CN)

scaled_surv_data <- as.data.frame(scaled_surv_data)
write.csv(scaled_surv_data, "data_processed/scaled_surv_data.csv")

# now run models ----------------------------------------------------------

# run first survival model
surv_model1 <- glmer(mort ~ PREVDIA + I(PREVDIA^2) + BALIVE + I(BALIVE^2) + MAT + MAP + (1|PLT_CN) + offset(log(REMEAS_PERIOD)), 
                   family = binomial(link = "cloglog"), data = scaled_surv_data,
                   control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))

summary(surv_model1)

# make effect plots
plot(allEffects(surv_model1))
plot(effect("BALIVE",surv_model1)) #the results of BALIVE (ie: competition) is negative
#interpret these model results

plot(simulateResiduals(surv_model1))
#what do these residual plots mean
check_model(surv_model1)

plotResiduals(survData.scaled$PREVDIA, res$scaledResiduals, quantreg = T, main = "PREVDIA") #resid plot doesn't look good


#Demonstrate the effect of quadratics
surv_model2 <- glmer(mort ~ PREVDIA + BALIVE + (1|PLT_CN) + MAT + MAP + offset(log(REMEAS_PERIOD)), 
                    family = binomial(link = "cloglog"), data = scaled_surv_data,
                    control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
surv_model3 <- glmer(mort ~ PREVDIA + BALIVE + 
                    I(PREVDIA^2) + MAP + MAT + (1|PLT_CN) + offset(log(REMEAS_PERIOD)), 
                  family = binomial(link = "cloglog"), data = scaled_surv_data,
                  control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
#model selection

mod_comp<-model.sel(surv_model1,surv_model2, surv_model3)
mod_comp
# strong preference for survival model1

# specify the predictors in the "best" model (or candidate best)
surv_predictors <- c("PREVDIA","BALIVE", "MAT", "MAP")

get_scale <- function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

surv_scaling <- get_scale(scaled_surv_data, surv_predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in surv_predictors) {
  attributes(scaled_surv_data[, i]) = NULL
}

# export model for coefficients and scaling information -------------------
#save(smodel4.q, surv.scaling, file = "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/Code/IPM/SurvRescaling.Rdata")
#save(smodel4, surv.scaling, file = "./Code/IPM/SurvRescalingNoFire.Rdata")
#save(smodel3, surv.scaling, file = "./Code/IPM/SurvRescalingBA.Rdata")
save(surv_model1,
     surv_scaling, file = "models/Surv_Rescaling.Rdata")

