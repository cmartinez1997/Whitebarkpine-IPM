## Creating growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------
library(tidyverse)
library(terra)
library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)
library(performance) #to check model 
library(dplyr)
library(sp)
library(effects)

# Read in data used for model ---------------------------------------------

model_growth_data <- read_csv("data_processed/WBP_growth.csv")
range(model_growth_data$MEASYEAR)
model_growth_data <- model_growth_data %>% dplyr::select(TRE_CN, PREV_TRE_CN, PLT_CN, PREV_PLT_CN, LAT, LON, ELEV, 
                                                  DIA, PREVDIA, DIA_DIFF, DIA_INCR, MEASYEAR, PREV_MEASYEAR, CENSUS_INTERVAL, 
                                                  BALIVE, CONDID, STATUSCD, DSTRBCD1, DSTRBCD2, DSTRBCD3, AGENTCD)

# standardize numerical covariates, center and scale 
scaled_growth_data <- model_growth_data %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                          -MEASYEAR, -PREV_MEASYEAR,
                                                                          -CENSUS_INTERVAL, -DIA_INCR, -STATUSCD))
# make growth data spatial 
grow_spat <- SpatialPointsDataFrame(coords = cbind(model_growth_data$LON, model_growth_data$LAT), 
                                 data = model_growth_data, 
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))

coords <- cbind(grow_spat$LON, grow_spat$LAT)  # Extract coordinates

    
#now read in climate tiffs to include them in the model, but only normals for the census interval plannes 
clim_ppt <- terra::rast("PRISM/data_formatted/new_wbp_pptStack.tif")
clim_tmean <- terra::rast("PRISM/data_formatted/new_wbp_tmeanStack.tif")

ppt_extr_grow <- terra::extract(clim_ppt, coords)
tmean_extr_grow <- terra::extract(clim_tmean, coords)

# export climate data
processed_path <- "data_processed/"
write_csv(ppt_extr_grow, paste0(processed_path,"ppt_extr_grow.csv"))
write_csv(tmean_extr_grow, paste0(processed_path,"tmean_extr_grow.csv"))


# adding sensible column names 
ppt_extr_df <- as.data.frame(ppt_extr_grow)
tmean_extr_df <- as.data.frame(tmean_extr_grow)

PRISM_path <-  "PRISM/data/"
ppt_files <- list.files(path = PRISM_path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)

colNames <- lapply(strsplit(ppt_files, "4kmM[23]_"), function(x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)

colnames(ppt_extr_df) <- paste0("ppt_", colNames)
colnames(tmean_extr_df) <- paste0("tmean_", colNames)

#okay make this into a dataframe
ppt_extr_df <- data.frame(LON = grow_spat$LON, LAT = grow_spat$LAT, ppt_extr_df)
tmean_extr_df <- data.frame(LON = grow_spat$LON, LAT = grow_spat$LAT, tmean_extr_df)


#adding the TRE_CN, PLT_CN to the climate dataframes
ppt_df <- merge(ppt_extr_df, model_growth_data[, c("LON", "LAT", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))
tmean_df <- merge(tmean_extr_df, model_growth_data[, c("LON", "LAT", "TRE_CN",  "PLT_CN")], by = c("LON", "LAT"))

#wrangle climate datafrmae
ppt_df_long <- ppt_df %>%
  pivot_longer(
    cols = starts_with("ppt_"), 
    names_to = "MONTH_YEAR", 
    values_to = "PRECIP"
  ) %>%
  mutate(
    # Remove the "ppt_" prefix
    MONTH_YEAR = sub("ppt_", "", MONTH_YEAR),
    # Extract YEAR and MONTH
    YEAR = as.numeric(substr(MONTH_YEAR, 1, 4)),   # Characters 1-4
    MONTH = as.numeric(substr(MONTH_YEAR, 5, 6))   # Characters 5-6
  )

ppt_df_long$PLT_CN <- as.character(ppt_df_long$PLT_CN)
ppt_df_long$TRE_CN <- as.character(ppt_df_long$TRE_CN)


ppt_df_filt <- ppt_df_long %>%
  filter(YEAR >= 2003) 

map_per_tree_year <- ppt_df_filt %>%
  group_by(TRE_CN, YEAR) %>%
  summarise(
    MAP = sum(PRECIP, na.rm = TRUE)
  ) %>%
  ungroup()
 
ppt_df_filt <- ppt_df_filt %>% 
  dplyr::select(c(-MONTH_YEAR, -PRECIP, -MONTH)) %>% 
  distinct(TRE_CN, YEAR, .keep_all = TRUE)

ppt_df_filt <- ppt_df_filt %>%
  left_join(map_per_tree_year, by = c("TRE_CN", "YEAR"))


tmean_df_long <- tmean_df %>%
  pivot_longer(
    cols = starts_with("tmean_"), 
    names_to = "MONTH_YEAR", 
    values_to = "TEMP"
  ) %>%
  mutate(
    # Remove the "tmean_" prefix
    MONTH_YEAR = sub("tmean_", "", MONTH_YEAR),
    # Extract YEAR and MONTH
    YEAR = as.numeric(substr(MONTH_YEAR, 1, 4)),   # Characters 1-4
    MONTH = as.numeric(substr(MONTH_YEAR, 5, 6)), 
  )

    
tmean_df_long$PLT_CN <- as.character(tmean_df_long$PLT_CN)
tmean_df_long$TRE_CN <- as.character(tmean_df_long$TRE_CN)

tmean_df_filt <- tmean_df_long %>%
  filter(YEAR >= 2003) 


# tre <- tmean_df_filt %>% 
#   filter(TRE_CN == 251156435489998). #okay checking values for MAT and it works

mat_per_tree_year <- tmean_df_filt %>%
  group_by(TRE_CN, YEAR) %>%
  summarise(
    MAT = mean(TEMP, na.rm = TRUE)
  ) %>%
  ungroup()

tmean_df_filt <- tmean_df_long %>%
  dplyr::select(c(-MONTH_YEAR, -TEMP, -MONTH)) %>% 
  distinct(TRE_CN, YEAR, .keep_all = TRUE)

tmean_df_filt <- tmean_df_filt %>%
  left_join(mat_per_tree_year, by = c("TRE_CN", "YEAR"))


#okay aggregating data frame by MAT for each year

model_growth_data <- model_growth_data %>%
  mutate(
    START_YEAR = MEASYEAR - CENSUS_INTERVAL, 
    END_YEAR = MEASYEAR                     
  )

model_growth_data$TRE_CN <- as.character(model_growth_data$TRE_CN)
ppt_df_filt$TRE_CN <- as.character(ppt_df_filt$TRE_CN)


#okay adding MAP for the census interval
merged_data <- model_growth_data %>%
  dplyr::select(TRE_CN, START_YEAR, END_YEAR, CENSUS_INTERVAL) %>%
  distinct() %>%
  left_join(ppt_df_filt, by = "TRE_CN")

filtered_data <- merged_data %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAP_data <- filtered_data %>%
  group_by(TRE_CN) %>%
  summarise(mean_MAP = mean(MAP, na.rm = TRUE))

final_data <- model_growth_data %>%
  left_join(mean_MAP_data, by = "TRE_CN") %>%
  mutate(MAP = mean_MAP / CENSUS_INTERVAL)

##okay adding MAT
#okay adding MAP for the census interval
merged_data_tmean <- model_growth_data %>%
  dplyr::select(TRE_CN, START_YEAR, END_YEAR, CENSUS_INTERVAL) %>%
  distinct() %>%
  left_join(tmean_df_filt, by = "TRE_CN")

filtered_data_tmean <- merged_data_tmean %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAT_data <- filtered_data_tmean %>%
  group_by(TRE_CN) %>%
  summarise(mean_MAT = mean(MAT, na.rm = TRUE))

final_data <- final_data %>%
  left_join(mean_MAT_data, by = "TRE_CN") %>%
  mutate(MAT = mean_MAT / CENSUS_INTERVAL)


# okay created final datframe including temp and ppt covariates: 
# standardize numerical covariates, center and scale 
model_growth_data <- final_data %>% dplyr::select(TRE_CN, PREV_TRE_CN, PLT_CN, PREV_PLT_CN, LAT, LON, ELEV, 
                                                         DIA, PREVDIA, DIA_DIFF, DIA_INCR, MEASYEAR, PREV_MEASYEAR, CENSUS_INTERVAL, 
                                                         BALIVE, CONDID, STATUSCD, DSTRBCD1, DSTRBCD2, DSTRBCD3, AGENTCD, MAT, MAP)
scaled_growth_data <- model_growth_data %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                          -MEASYEAR, -PREV_MEASYEAR,
                                                                          -CENSUS_INTERVAL, -DIA_INCR, -STATUSCD))

scaled_growth_data <- scaled_growth_data %>% 
  filter(!is.na(DIA_INCR))
scaled_growth_data$PREV_PLT_CN <- as.character(scaled_growth_data$PREV_PLT_CN)
scaled_growth_data$PREV_TRE_CN <- as.character(scaled_growth_data$PREV_TRE_CN)

#this includes a plot level random effect 
#this is the linear model 
grow_model_01 <- lmer(DIA_INCR ~ PREVDIA + BALIVE + MAP + MAT + (1|PLT_CN), data = scaled_growth_data)
class(grow_model_01) <- "lmerMOD"

# check the model residuals in DHARMA: not looking so great 
plot(simulateResiduals(grow_model_01, integerResponse = F), quantreg = T)
check_model(grow_model_01)
#how to graph residuals with mixed models?? - ask for help

#check the residuals using DHARMA package
plot(grow_model_01)
#quantile deviations detected 
#model prediction - rank transformed

#quadratic effects - check preference for model by using AICc
grow_model_02 <- lmer(DIA_INCR ~ PREVDIA + BALIVE  + 
                       I(PREVDIA^2) + MAP + MAT + 
                       (1|PLT_CN), data = scaled_growth_data)
qqnorm(residuals(grow_model_02))

check_model(grow_model_02)
summary(grow_model_02)
plot(simulateResiduals(grow_model_02, integerResponse = F), quantreg = T)
plot(grow_model_02)

grow_model_03 <- lmer(DIA_INCR ~ PREVDIA + BALIVE  + I(BALIVE^2) +
                        I(PREVDIA^2) + MAP + MAT + 
                        (1|PLT_CN), data = scaled_growth_data)
qqnorm(residuals(grow_model_03))

# resid_pearson <- residuals(gmodel.DIA.q,type="pearson",scaled=TRUE)
# plotLowess(resid_pearson~predicted, ylab="Residuals",xlab="Predicted", main="Gaussian")


#model selection step using AIC
mod_comp <- model.sel(grow_model_01,grow_model_02, grow_model_03)
#better model is the quadratic, but linear has the lower AIC

grow_model_size <- lm(DIA ~ PREVDIA  + 
                        I(PREVDIA^2) , data = scaled_growth_data)


# growSD is used for building IPM (see BuildIPM.R), look into this mroe
grow_SD_02 <- sd(resid(grow_model_02))

#make a model to check the IPM with only size as predictor
gmodel_check <- lmer(DIA_INCR ~ PREVDIA   + 
                       I(PREVDIA^2) +
                       (1|PLT_CN), data = scaled_growth_data)
grow_SD_check <- sd(resid(gmodel_check))

# specify the predictors in the exported models
grow_model_predictors <- c("PREVDIA", "BALIVE", "MAP", "MAT") 

# deal with scaled data
get_scale <- function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

growth_scaling <- get_scale(scaled_growth_data, grow_model_predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in grow_model_predictors) {
  attributes(scaled_growth_data[, i]) = NULL
}

#visualzie effect sizes of predictor variables 
plot(effect("BALIVE", grow_model_02))
plot(effect("PREVDIA", grow_model_02))
plot(effect("MAT", grow_model_02))
plot(effect("MAP", grow_model_02))



# export model for coefficients and scaling information -------------------
#save(gmodel.7, gr.scaling, growSD, file = "./Code/IPM/GrRescaling.Rdata")
save(grow_model_02, grow_SD_02,grow_model_size, grow_SD_check,
     growth_scaling, scaled_growth_data, file = "models/Grow_Rescaling.Rdata")

#okay growth model selected

