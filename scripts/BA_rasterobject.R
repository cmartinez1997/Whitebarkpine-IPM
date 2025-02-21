###Original code from Emily Schultz DRM:https://github.com/emilylschultz/DemographicRangeModel/blob/master/Code/ClimateProcessing/normals.R
##Updated by Cecilia Martinez
##July 2023
##cecimartinez333@gmail.com

library(sp)
library(raster)
library(dplyr)
library(effects)
library(mgcv)
library(tidyverse)

### create data object with BALIVE (in the future can include other covariates)------------------------------------------------------------------------------------

conds_ba <- read_csv("data_processed/COND_MT-ID-WY.csv")
# Remove non-forest conditions (missing BALIVE, coded as 0 so causes underestimation)
# conds <- subset(conds, COND_STATUS_CD == 1) # "accessible forest land" by FIA classification

# try including nonforested land in model
conds_1 <- subset(conds_ba, COND_STATUS_CD == 1) # "accessible forest land" by FIA classification
conds_2 <- subset(conds_ba, COND_STATUS_CD == 2) # "nonforest land" by FIA classification
conds_2 <- conds_2[conds_2$PRESNFCD %in% c(20, 40, 42, 45), ] # only two categories of non-forest land that are not caused by (human) land use
conds_2$BALIVE <- 0 # populate with zeros since there is no BALIVE value in here

# join the two together
conds_ba <- rbind(conds_1, conds_2)

hist(conds_ba$BALIVE) # almost all of the data has values <=400; outliers above 400
summary(conds_ba$BALIVE) # maximum value is 2326.11
plot(conds_ba$CONDPROP_UNADJ, conds_ba$BALIVE) # reveals that one outlier for BALIVE is a cond with very low CONDPROP_UNADJ
conds_ba <- subset(conds_ba, BALIVE < 1000) # removes one outlier!
plot(conds_ba$CONDPROP_UNADJ, conds_ba$BALIVE) #visualize without the outlier

# Read in plot data and get coordinates, elevation, and previous measurement year
plots_obj <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

conds_ba$LAT <- plots_obj$LAT[match(conds_ba$PLT_CN, plots_obj$CN)]
conds_ba$LON <- plots_obj$LON[match(conds_ba$PLT_CN, plots_obj$CN)]
conds_ba$ELEV <- plots_obj$ELEV[match(conds_ba$PLT_CN, plots_obj$CN)]
conds_ba$MEASYEAR <- plots_obj$MEASYEAR[match(conds_ba$PLT_CN, plots_obj$CN)]
conds_ba$REMPER <- plots_obj$REMPER[match(conds_ba$PLT_CN, plots_obj$CN)]

# Make data spatial
CondsSpat <- SpatialPointsDataFrame(coords = cbind(conds_ba$LON, conds_ba$LAT), 
                                    data = conds_ba, 
                                    proj4string = CRS("+proj=longlat +datum=NAD83"))

##Try this using terra package and function

coords <- cbind(conds_ba$LON, conds_ba$LAT)  # Set the coordinate reference system (CRS)
# Convert the SpatVector to a SpatVectorPointsDataFrame
# to keep associated data from the original data.frame (conds_ba)

#now read in climate tiffs to include them in the model, but only normals for the census interval plannes 
clim_ppt <- terra::rast("PRISM/data_formatted/new_wbp_pptStack.tif")
clim_tmean <- terra::rast("PRISM/data_formatted/new_wbp_tmeanStack.tif")


ppt_extr_rec <- terra::extract(clim_ppt, coords)
tmean_extr_rec <- terra::extract(clim_tmean, coords)

# Convert to data frames
ppt_extr_df <- as.data.frame(ppt_extr_rec)
tmean_extr_df <- as.data.frame(tmean_extr_rec)

# export climate data
# processed_path <- "data_processed/"
# write_csv(ppt_extr_df, paste0(processed_path,"ppt_extr_rec.csv"))
# write_csv(tmean_extr_df, paste0(processed_path,"tmean_extr_rec.csv"))



PRISM_path <-  "PRISM/data/"
ppt_files <- list.files(path = PRISM_path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)

colNames <- lapply(strsplit(ppt_files, "4kmM[23]_"), function(x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)

colnames(ppt_extr_df) <- paste0("ppt_", colNames)
colnames(tmean_extr_df) <- paste0("tmean_", colNames)

conds_spat_df <- as.data.frame(conds_spat)

#okay make this into a dataframe
ppt_extr_df <- data.frame(LON = CondsSpat$LON, LAT = CondsSpat$LAT, ppt_extr_df)
tmean_extr_df <- data.frame(LON = CondsSpat$LON, LAT = CondsSpat$LAT, tmean_extr_df)


#adding the TRE_CN, PLT_CN to the climate dataframes
ppt_df <- merge(ppt_extr_df, conds_ba[, c("LON", "LAT", "PLT_CN")], by = c("LON", "LAT"))
tmean_df <- merge(tmean_extr_df, conds_ba[, c("LON", "LAT",  "PLT_CN")], by = c("LON", "LAT"))

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

ppt_df_filt <- ppt_df_long %>%
  filter(YEAR >= 2003) 

map_per_tree_year <- ppt_df_filt %>%
  group_by(PLT_CN, YEAR) %>%
  summarise(
    MAP = sum(PRECIP, na.rm = TRUE)
  ) %>%
  ungroup()


ppt_df_filt <- ppt_df_filt %>% 
  dplyr::select(c(-MONTH_YEAR, -PRECIP, -MONTH)) %>% 
  distinct(PLT_CN, YEAR, .keep_all = TRUE)

ppt_df_filt <- ppt_df_filt %>%
  left_join(map_per_tree_year, by = c("PLT_CN", "YEAR"))


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

tmean_df_filt <- tmean_df_long %>%
  filter(YEAR >= 2003) 

# tre <- tmean_df_filt %>% 
#   filter(TRE_CN == 251156435489998). #okay checking values for MAT and it works

mat_per_tree_year <- tmean_df_filt %>%
  group_by(PLT_CN, YEAR) %>%
  summarise(
    MAT = mean(TEMP, na.rm = TRUE)
  ) %>%
  ungroup()

tmean_df_filt <- tmean_df_long %>%
  dplyr::select(c(-MONTH_YEAR, -TEMP, -MONTH)) %>% 
  distinct(PLT_CN, YEAR, .keep_all = TRUE)

tmean_df_filt <- tmean_df_filt %>%
  left_join(mat_per_tree_year, by = c("PLT_CN", "YEAR"))


#okay aggregating data frame by MAT for each year

conds_ba <- conds_ba %>%
  mutate(
    START_YEAR = MEASYEAR - REMPER, 
    END_YEAR = MEASYEAR
  )

conds_ba$PLT_CN <- as.character(conds_ba$PLT_CN)

#okay adding MAP for the census interval
merged_data <- conds_ba %>%
  dplyr::select(PLT_CN, START_YEAR, END_YEAR, REMPER) %>%
  distinct() %>%
  left_join(ppt_df_filt, by = "PLT_CN")

filtered_data <- merged_data %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAP_data <- filtered_data %>%
  group_by(PLT_CN) %>%
  summarise(mean_MAP = mean(MAP, na.rm = TRUE))

final_data <- conds_ba %>%
  left_join(mean_MAP_data, by = "PLT_CN") %>%
  mutate(MAP = mean_MAP / REMPER)

##okay adding MAT
#okay adding MAP for the census interval
merged_data_tmean <- conds_ba %>%
  dplyr::select(PLT_CN, START_YEAR, END_YEAR, REMPER) %>%
  distinct() %>%
  left_join(tmean_df_filt, by = "PLT_CN")

filtered_data_tmean <- merged_data_tmean %>%
  rowwise() %>%
  filter(YEAR >= START_YEAR & YEAR <= END_YEAR) %>%
  ungroup()

mean_MAT_data <- filtered_data_tmean %>%
  group_by(PLT_CN) %>%
  summarise(mean_MAT = mean(MAT, na.rm = TRUE))

final_data <- final_data %>%
  left_join(mean_MAT_data, by = "PLT_CN") %>%
  mutate(MAT = mean_MAT / REMPER)




# Create output data frame
output <- final_data[, c("PLT_CN", "CONDID", "CONDPROP_UNADJ",
                       "LAT", "LON", "ELEV",
                       "BALIVE", "MAP", "MAT")]
#"PPT_c_norm", "T_c_norm", "VPD_c_norm",
#"PPT_wd_norm", "T_wd_norm", "VPD_wd_norm",
#"PPT_pf_norm", "T_pf_norm", "VPD_pf_norm",
#"PPT_fs_norm", "T_fs_norm", "VPD_fs_norm",
#"PPT_m_norm", "T_m_norm", "VPD_m_norm",
#"PPT_yr_norm", "T_yr_norm", "VPD_yr_norm")]
# write.csv(output, paste0(path, "BA/BALIVEdata.csv"), row.names = F)
write.csv(output,"data_processed//BALIVEdata2.csv", row.names = F)
