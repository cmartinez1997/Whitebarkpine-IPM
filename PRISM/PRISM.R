##PRISM Climate Data - WBP
##June 2023
##cecimartinez333@gmail.com



# load necessary packages -------------------------------------------------

library(prism) # for retrieving prism data
library(tidyverse)
library(maps)
library(terra)
library(raster)
library(sf)
library(sp)
library(dplyr)

### Getting PRISM climate data to create rasters of monthly climate variables in WBP study area ###

#set directory for climate files
prism_set_dl_dir("PRISM/data/")

#retrieve files from PRISM website from 1895 to 2022, this step takes a long time but only do it once

#get precip files 
get_prism_monthlys(type = 'ppt', 
                   years = 1895:2022,  
                   mon=1:12,
                   keepZip = TRUE)

#get temp max files
get_prism_monthlys(type = 'tmax', 
                   years = 1895:2022,  
                   mon=1:12, 
                   keepZip = TRUE)

#get temp min files 
get_prism_monthlys(type = 'tmin', 
                   years = 1895:2022,  
                   mon=1:12, 
                   keepZip = TRUE)

#get vpdmax files
get_prism_monthlys(type = 'vpdmax', 
                   years = 1895:2022,  
                   mon=1:12, 
                   keepZip = TRUE)

#looking at all the files in the prism archive
prism_archive_ls()

#Stack monthly data according to data type - part of prism package, this takes a little while
pptStack <- pd_stack(prism_archive_subset('ppt', "monthly")) 
tmaxStack <- pd_stack(prism_archive_subset('tmax', "monthly"))
tminStack <- pd_stack(prism_archive_subset('tmin', "monthly"))
vpdStack <- pd_stack(prism_archive_subset('vpdmax', "monthly"))

# crop climate to extent of WBP occurrence in FIA plots: Wyoming, Montana, Idaho ### not for

# crop climate to our study area and make spatial, we will do this for Utah

clim_map <- ggplot2::map_data('state', region = c("Utah"))
utah_spat <- vect(clim_map, geom = c("long", "lat"), crs = "+proj=longlat +datum=NAD83")
# print(utah_spat)

# do this in raster package vs terra package, one works and the other does notweird 

ut_meta_es <- read_csv("data_processed/ut_meta_ES.csv")

ut_spat <- SpatialPointsDataFrame(coords = cbind(clim_map$long, clim_map$lat), 
                                  data = clim_map, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))

es_spat <- SpatialPointsDataFrame(coords = cbind(ut_meta$LON, ut_meta$LAT), 
                                  data = ut_meta_es, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))

cropUT <- extent(ut_spat)

#cropping to extent of utah, this takes some time
# the crop function works for the raster packlage but not the terra package
pptStackCropped <- crop(pptStack, cropUT)
tminStackCropped <- crop(tminStack, cropUT)
tmaxStackCropped <- crop(tmaxStack, cropUT)
vpdStackCropped <- crop(vpdStack, cropUT)

# create an extent object to try to get terra crop function to work
#crop_es <- terra::ext(c(-114.0472, -109.0396, 36.99588, 42.00354))

# pptStackCropped <- terra::crop(pptStack, crop_es)
# tminStackCropped <- terra::crop(tminStack, crop_es)
# tmaxStackCropped <- terra::crop(tmaxStack, crop_es)
# vpdStackCropped <- terra::crop(vpdStack, crop_es)

### Want a single climate value instead of several values for each data point, over the entire geographic extent of my occurrence data


# Export rasters to PRISM data formatted folder
clim_path <-  "PRISM/data_formatted/"

writeRaster(pptStackCropped, paste0(clim_path, "utah_pptStack.tif"), overwrite = T)
writeRaster(tminStackCropped, paste0(clim_path, "utah_tminStack.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim_path, "utah_tmaxStack.tif"), overwrite = T)
writeRaster(vpdStackCropped, paste0(clim_path, "utah_vpdStack.tif"), overwrite = T)


#extract PRISM data into vectors 
ppt_extr <- terra::extract(pptStackCropped, es_spat[,c("LON", "LAT")]) 
tmin_extr <- terra::extract(tminStackCropped, es_spat[,c("LON", "LAT")])
tmax_extr <- terra::extract(tmaxStackCropped, es_spat[,c("LON", "LAT")])
vpd_extr <- terra::extract(vpdStackCropped,  es_spat[,c("LON", "LAT")])

# Add sensible column names for raster extracted climate data by referencing the original climate files
# Each row is an individual tree's data 
ppt_extr <- as.data.frame(ppt_extr)
tmin_extr <- as.data.frame(tmin_extr)
tmax_extr <- as.data.frame(tmax_extr)
vpd_extr <- as.data.frame(vpd_extr)

PRISM_path <-  "PRISM/data/"
ppt_files <- list.files(path = PRISM_path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)
#tmeanFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*_bil"), full.names = TRUE)
#tmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*_bil"), full.names = TRUE)
#tminFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmin*_bil"), full.names = TRUE)

colNames <- lapply(strsplit(ppt_files, "4kmM[23]_"), function(x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)

colnames(ppt_extr) <- paste0("ppt_", colNames)
colnames(tmin_extr) <- paste0("tmin_", colNames)
colnames(tmax_extr) <- paste0("tmax_", colNames)
colnames(vpd_extr) <- paste0("vpd_", colNames)



## Add tre_cn, plt_cn, core_cn columns to link to other dataframes

ppt_df <- data.frame(LON = es_spat$LON, LAT = es_spat$LAT, ppt_extr)
tmin_df <- data.frame(LON = es_spat$LON, LAT = es_spat$LAT, tmin_extr)
tmax_df <- data.frame(LON = es_spat$LON, LAT = es_spat$LAT, tmax_extr)
vpd_df <- data.frame(LON = es_spat$LON, LAT = es_spat$LAT, vpd_extr)


#adding the CN, TRE_CN, PLT_CN to the climate dataframes
ppt_df <- merge(ppt_df, ut_meta_es[, c("LON", "LAT", "CN", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))
tmin_df <- merge(tmin_df, ut_meta_es[, c("LON", "LAT", "CN", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))
tmax_df <- merge(tmax_df, ut_meta_es[, c("LON", "LAT", "CN", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))
vpd_df <- merge(vpd_df, ut_meta_es[, c("LON", "LAT", "CN", "TRE_CN", "PLT_CN")], by = c("LON", "LAT"))

ppt_df <- ppt_df %>%
  dplyr::select(TRE_CN, CN, PLT_CN, LON, LAT, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  # Renames CN to CORE_CN

tmin_df <- tmin_df %>%
  dplyr::select(TRE_CN, CN, PLT_CN, LON, LAT, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  # Renames CN to CORE_CN

tmax_df <- tmax_df %>%
  dplyr::select(TRE_CN, CN, PLT_CN, LON, LAT, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  # Renames CN to CORE_CN

vpd_df <- vpd_df %>%
  dplyr::select(TRE_CN, CN, PLT_CN, LON, LAT, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  # Renames CN to CORE_CN

#n.tmx.extr <- as.data.frame(n.tmx.extr)
#n.tmx.extr$TRE_CN <- val_trees$TRE_CN
# Export climate data

write_csv(ppt_df, "PRISM/data_formatted/utah_ppt_df.csv")
write_csv(tmin_df, "PRISM/data_formatted/utah_tmin_df.csv")
write_csv(tmax_df, "PRISM/data_formatted/utah_tmax_df.csv")
write_csv(vpd_df, "PRISM/data_formatted/utah_vpd_df.csv")


