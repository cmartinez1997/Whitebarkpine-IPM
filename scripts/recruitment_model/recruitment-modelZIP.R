## Creating recruitment ZIP model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com



# load packages -----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)
library(performance) #to check model 
library(effects)
library(ggeffects)
library(DHARMa)
library(glmmTMB)
library(dplyr)


# read in data ------------------------------------------------------------

recruitment <- read_csv("data_processed/WBP_recruit.csv")
recruitment <- recruitment %>%
    dplyr::select(-1)
recruitment$plotprev <- as.character(recruitment$plotprev)


rec_spat <- SpatialPointsDataFrame(coords = cbind(recruitment$lon, recruitment$lat), 
                                    data = recruitment, 
                                    proj4string = CRS("+proj=longlat +datum=NAD83"))

coords <- cbind(rec_spat$lon, rec_spat$lat)  # Extract coordinates

#now read in climate tiffs to include them in the model, but only normals for the census interval plannes 
# Read in climate tiffs
clim_ppt <- terra::rast("PRISM/data_formatted/new_wbp_pptStack.tif")
clim_tmean <- terra::rast("PRISM/data_formatted/new_wbp_tmeanStack.tif")

ppt_extr_rec <- terra::extract(clim_ppt, coords)
tmean_extr_rec <- terra::extract(clim_tmean, coords)

# Export climate data
processed_path <- "data_processed/"
write_csv(ppt_extr_rec, paste0(processed_path, "ppt_extr_rec.csv"))
write_csv(tmean_extr_rec, paste0(processed_path, "tmean_extr_rec.csv"))

# Adding sensible column names
ppt_extr_df <- as.data.frame(ppt_extr_rec)
tmean_extr_df <- as.data.frame(tmean_extr_rec)

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
ppt_extr_df <- data.frame(lon = rec_spat$lon, lat = rec_spat$lat, ppt_extr_df)
tmean_extr_df <- data.frame(lon = rec_spat$lon, lat = rec_spat$lat, tmean_extr_df)

# Subset columns for years >= 2003 before merging (optimization)
ppt_cols_to_keep <- c("lon", "lat", grep("^ppt_200[3-9]|^ppt_20[1-9][0-9]", colnames(ppt_extr_df), value = TRUE))
tmean_cols_to_keep <- c("lon", "lat", grep("^tmean_200[3-9]|^tmean_20[1-9][0-9]", colnames(tmean_extr_df), value = TRUE))

ppt_extr_df <- ppt_extr_df[, ppt_cols_to_keep]
tmean_extr_df <- tmean_extr_df[, tmean_cols_to_keep]

# Merge climate dataframes with survival data
ppt_df <- merge(ppt_extr_df, recruitment[, c("lon", "lat", "plot")], by = c("lon", "lat"))
tmean_df <- merge(tmean_extr_df, recruitment[, c("lon", "lat",  "plot")], by = c("lon", "lat"))

# Wrangle `ppt_df` into long format
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

ppt_df_long$plot <- as.character(ppt_df_long$plot)

# Filter for years >= 2003
ppt_df_filt <- ppt_df_long %>%
  filter(YEAR >= 2003)

# Aggregate MAP
map_per_tree_year <- ppt_df_filt %>%
  group_by(plot, YEAR) %>%
  summarise(
    MAP = sum(PRECIP, na.rm = TRUE)
  ) %>%
  ungroup()

ppt_df_filt <- ppt_df_filt %>% 
  dplyr::select(c(-MONTH_YEAR, -PRECIP, -MONTH)) %>% 
  distinct(plot, YEAR, .keep_all = TRUE)
ppt_df_filt <- ppt_df_filt %>%
  left_join(map_per_tree_year, by = c("plot", "YEAR"))


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

tmean_df_long$plot <- as.character(tmean_df_long$plot)


tmean_df_filt <- tmean_df_long %>%
  filter(YEAR >= 2003) 

# tre <- tmean_df_filt %>% 
#   filter(TRE_CN == 251156435489998). #okay checking values for MAT and it works

tmean_df_filt <- tmean_df_long %>%
  dplyr::select(c(-MONTH_YEAR, -TEMP, -MONTH)) %>% 
  distinct(plot, YEAR, .keep_all = TRUE)

tmean_df_filt <- tmean_df_filt %>%
  left_join(mat_per_tree_year, by = c("plot", "YEAR"))


#okay aggregating data frame by MAT for each year

recruitment <- recruitment %>%
  mutate(
    START_YEAR = measyear - CENSUS_INTERVAL, 
    END_YEAR = measyear
  )


recruitment$plot <- as.character(recruitment$plot)
merged_data <- recruitment %>%
  dplyr::select(plot, START_YEAR, END_YEAR, CENSUS_INTERVAL) %>%
  distinct() %>%
  left_join(ppt_df_filt, by = "plot")


filtered_data <- merged_data %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAP_data <- filtered_data %>%
  group_by(plot) %>%
  summarise(mean_MAP = mean(MAP, na.rm = TRUE))

final_data <- recruitment %>%
  left_join(mean_MAP_data, by = "plot") %>%
  mutate(MAP = mean_MAP / CENSUS_INTERVAL)

##okay adding MAT
#okay adding MAP for the census interval
merged_data_tmean <- recruitment %>%
  dplyr::select(plot, START_YEAR, END_YEAR, CENSUS_INTERVAL) %>%
  distinct() %>%
  left_join(tmean_df_filt, by = "plot")

filtered_data_tmean <- merged_data_tmean %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAT_data <- filtered_data_tmean %>%
  group_by(plot) %>%
  summarise(mean_MAT = mean(MAT, na.rm = TRUE))

final_data <- final_data %>%
  left_join(mean_MAT_data, by = "plot") %>%
  mutate(MAT = mean_MAT / CENSUS_INTERVAL)
#


# standardize covariates --------------------------------------------------
rec_dat_scaled <- final_data %>% mutate_at(scale, .vars = vars(-plot, -lat, -lon, -elev, -PAwbp,
                                                        -state, -county, -plotID, -CONDID, 
                                                        -measyear, -plotprev, -PREV_MEASYEAR,
                                                        -CENSUS_INTERVAL, -recruits1, -recruits12,
                                                        -WBPadults1,
                                                        -WBPadults4, -WBPadults8, -WBPadults148, -cumDIA.WBP, -cumDIA.others))



# model ZIP ---------------------------------------------------------------
rec_dat_scaled <- rec_dat_scaled %>% 
  filter(!is.na(WBPadults1), WBPadults1 > 0)
# choosing wbp >1 as adult
rec_model1 <- glmmTMB(
  recruits12 ~ 1
  + MAT + I(MAT^2)
  + MAP + I(MAP^2)
  + BALIVE + I(BALIVE^2)
  + offset(log(CENSUS_INTERVAL))
  + offset(log(WBPadults1)), # various alternatives for this offset
  ziformula = ~ 1 
  + BALIVE + I(BALIVE^2)
  + MAT + I(MAT^2)
  + MAP + I(MAP^2),
  data = rec_dat_scaled, 
  family = "poisson"
)



rec_model2 <- glmmTMB(recruits1 ~ 1
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2)
                      + BALIVE + I(BALIVE^2)
                      
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(WBPadults4)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2)
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2),
                      data = rec_dat_scaled, 
                      family = "poisson")

rec_model3 <- glmmTMB(recruits1 ~ 1
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2)
                      + BALIVE + I(BALIVE^2)
                      
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(WBPadults8)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2)
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2),
                      data = rec_dat_scaled, 
                      family = "poisson")


# with offset = WBPadults4, same problem
# with offset = WBPadults8 can;t ber un without adding arbitrary non-zero value since lots of plots have zero WBp trees > 8in dbh
rec_model4 <- glmmTMB(recruits1 ~ 1
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2)
                      + BALIVE + I(BALIVE^2)
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(cumDIA.WBP)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2)
                      + MAT + I(MAT^2)
                      + MAP + I(MAP^2),
                      data = rec_dat_scaled, 
                      family = "poisson")


# model selection ---------------------------------------------------------

mod_comp<-model.sel(rec_model1, rec_model4)
mod_comp

# with offset = WBPadults1 lowest AICc (752.8)

res <- simulateResiduals(rec_model1)
plot(res, quantreg = T) # ns: p = 0.032 - no significant problems detected

# must read in Effect.glmmTMB function
# see https://github.com/glmmTMB/glmmTMB/blob/7ba86a972ddb13226a8de9eab0e113e6156fccf4/glmmTMB/R/effects.R
### taken from githb repository 

Effect.glmmTMB <- function (focal.predictors, mod, ...) {
  fam <- family(mod)
  ## code to make the 'truncated_*' families work
  if (grepl("^truncated", fam$family)) 
    fam <- c(fam, make.link(fam$link))
  ## dummy functions to make Effect.default work
  dummyfuns <- list(variance=function(mu) mu,
                    initialize=expression(mustart <- y + 0.1),
                    dev.resids=function(...) poisson()$dev.res(...)
  )
  for (i in names(dummyfuns)) {
    if (is.null(fam[[i]])) fam[[i]] <- dummyfuns[[i]]
  }
  ## allow calculation of effects ...
  if (length(formals(fam$variance))>1) {
    warning("overriding variance function for effects: ",
            "computed variances may be incorrect")
    fam$variance <- dummyfuns$variance
  }
  args <- list(call = getCall(mod),
               coefficients = lme4::fixef(mod)[["cond"]],
               vcov = vcov(mod)[["cond"]],
               family=fam)
  if (!requireNamespace("effects"))
    stop("please install the effects package")
  effects::Effect.default(focal.predictors, mod, ..., sources = args)
}

plot(Effect.glmmTMB("BALIVE", rec_model1)) #recruitment decreases as BALIVE increases
plot(Effect.glmmTMB("MAT", rec_model1)) #recruitment decreases as BALIVE increases
plot(Effect.glmmTMB("MAP", rec_model1)) #recruitment decreases as BALIVE increases


### dealing with standardized covariates

# specify the predictors in the "best" model (or candidate best)
r_predictors <- c("BALIVE", "MAT", "MAP") # rec_model1

get_scale <- function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

r_scaling <- get_scale(rec_dat_scaled, r_predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in r_predictors) {
  attributes(rec_dat_scaled[, i]) = NULL
}

# export model for coefficients and scaling information -------------------
save(rec_model1,
     r_scaling, file = "models/RecruitRescaling.Rdata")


