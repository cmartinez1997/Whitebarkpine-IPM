##PRISM Climate Data - WBP
##June 2023
##cecimartinez333@gmail.com


library(prism)
library(raster)
library(rgdal)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maptools)
library(terra)
library(maps)



### Getting PRISM climate data to create rasters of monthly climate variables in WBP study area ###


#set directory for climate files
prism_set_dl_dir("PRISM.data/")
prism_check_dl_dir()

#get precip files from Jan 1895 to Dec 2018
get_prism_monthlys(type = 'ppt', 
                   years = 1895:2018,  
                   mon=1:12,
                   keepZip = TRUE)
#get temp max files from Jan 1895 to Dec 2018
get_prism_monthlys(type = 'tmax', 
                   years = 1895:2018,  
                   mon=1:12, 
                   keepZip = TRUE)

#get temp min files from Jan 1895 to Dec 2018
get_prism_monthlys(type = 'tmin', 
                   years = 1895:2018,  
                   mon=1:12, 
                   keepZip = TRUE)

#get mean temp files from Jan 1895 to Dec 2018
get_prism_monthlys(type = 'tmean', 
                   years = 1895:2018,  
                   mon=1:12, 
                   keepZip = TRUE)

#looking at all the files in the prism archive
prism_archive_ls()

#Stack monthly data according to data type - part of prism package
pptStack <- pd_stack(prism_archive_subset('ppt', "monthly")) #this takes some time 
tmaxStack <- pd_stack(prism_archive_subset('tmax', "monthly"))
tminStack <- pd_stack(prism_archive_subset('tmin', "monthly"))
tmeanStack <- pd_stack(prism_archive_subset('tmean', "monthly"))

###OR###

###if this does not work, use original script from DRM code: refer to Emily Schultz github
###this is alternate code that can be used and relies on the raster package

#PRISM.path <-  "PRISM.data/"
#pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
#pptFiles <- list.files(path = PRISM.path, pattern = ".*ppt.*\\.bil$", full.names = TRUE)

#tminFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmin*.bil"), full.names = TRUE)
#tmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*.bil"), full.names = TRUE)
#tmeanFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)

#pptStack <- stack()
#for (i in pptFiles) {
#  print(i)
#  pptStack <- stack(pptStack, raster(i))
#}

#tminStack <- stack()
#for (i in tminFiles) {
#  print(i)
#  tminStack <- stack(tminStack, raster(i))
#}

#tmaxStack <- stack()
#for (i in tmaxFiles) {
#  print(i)
#  tmaxStack <- stack(tmaxStack, raster(i))
#}

### Crop climate to extent of WBP occurrence in FIA plots: Wyoming, Montana, Idaho ###

# Crop climate to extent of Utah
library(maps)

clim_map <- ggplot2::map_data('state', region = c('Wyoming', 'Montana', 'Idaho'))
wbp_spat <- SpatialPointsDataFrame(coords = cbind(clim_map$lon, clim_map$lat), 
                                  data = clim_map, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))


##using terra package

  wbp_spats <- vect(clim_map, 
                    geom = c("LON", "LAT"), 
                    crs = "+proj=longlat +datum=NAD83")
  # Convert the SpatVector to a SpatVectorPointsDataFrame
  # to keep associated data from the original data.frame (conds_ba)
  wbp_spat_df <- as.points(wbp_spats)


crop_wbp <- extent(wbp_spat)
pptStackCropped <- crop(pptStack, crop_wbp)
tminStackCropped <- crop(tminStack, crop_wbp)
tmaxStackCropped <- crop(tmaxStack, crop_wbp)
tmeanStackCropped <- crop(tmeanStack, crop_wbp)

### Want a single climate value instead of several values for each data point, over the entire geographic extent of my occurrence data



# Export rasters to PRISM data formatted folder
clim.path <-  "Prism_formatted/"
writeRaster(pptStackCropped, paste0(clim.path, "pptStack.tif"), overwrite = T)
writeRaster(tminStackCropped, paste0(clim.path, "tminStack.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim.path, "tmaxStack.tif"), overwrite = T)
writeRaster(tmeanStackCropped, paste0(clim.path, "tmeanStack.tif"), overwrite = T)

#original code from Margaret Evans to make spatial data frame 
#margaret.ekevans@gmail.com
#updated by Courtney Giebink
#updated by Cecilia Martinez

#load trees
#covariate data - per_cov_final
load("per_cov_final.Rdata")

#time series data - per_cov_incr
load("per_cov_incr_unique.Rdata")

#wide format
#one tree per row
final_trees <- per_cov_final[per_cov_final$TRE_CN %in% per_cov_incr$TRE_CN,]

# Make lat, lon data spatial
wbp_tree_spat <- SpatialPointsDataFrame(coords = cbind(final_trees$LON, final_trees$LAT), 
                                       data = final_trees, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

### Generate extracted raster files into csv files ###

# Read in PRISM climate stacks, only do this if you need to reupload raster data

#clim.path <-  "Prism_formatted/"
#ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
#tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
#tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))
#tmean <- stack(paste(clim.path,"tmeanStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(pptStackCropped, wbp_tree_spat[,c("LON", "LAT")]) 
tmin.extr <- raster::extract(tminStackCropped, wbp_tree_spat[,c("LON", "LAT")])
tmax.extr <- raster::extract(tmaxStackCropped, wbp_tree_spat[,c("LON", "LAT")])
tmean.extr <- raster::extract(tmeanStackCropped, wbp_tree_spat[,c("LON", "LAT")])


#this is a different terra function
ppt.extr <- terra::extract(pptStackCropped, wbp_tree_spat[,c("LON", "LAT")]) 
tmin.extr <- terra::extract(tminStackCropped, wbp_tree_spat[,c("LON", "LAT")])
tmax.extr <- terra::extract(tmaxStackCropped, wbp_tree_spat[,c("LON", "LAT")])
tmean.extr <- terra::extract(tmeanStackCropped, wbp_tree_spat[,c("LON", "LAT")])

# Add sensible column names for raster::extracted climate data by referencing the original climate files
# Each row is an individual tree's data 
ppt.extr <- as.data.frame(ppt.extr)
tmin.extr <- as.data.frame(tmin.extr)
tmax.extr <- as.data.frame(tmax.extr)
tmean.extr <- as.data.frame(tmean.extr)

PRISM.path <-  "PRISM.data/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)
#tmeanFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*_bil"), full.names = TRUE)
#tmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*_bil"), full.names = TRUE)
#tminFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmin*_bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmin.extr) <- paste0("tmin_", colNames)
colnames(tmax.extr) <- paste0("tmax_", colNames)
colnames(tmean.extr) <- paste0("tmean_", colNames)


## Add tre_cn, plt_cn, core_cn columns to link to other dataframes

ppt.extr <- data.frame(CORE_CN = final_trees$CN, TRE_CN = final_trees$TRE_CN, PLT_CN = final_trees$PLT_CN, ppt.extr)

tmin.extr <- data.frame(CORE_CN = final_trees$CN, TRE_CN = final_trees$TRE_CN, PLT_CN = final_trees$PLT_CN, tmin.extr)

tmax.extr <- data.frame(CORE_CN = final_trees$CN, TRE_CN = final_trees$TRE_CN, PLT_CN = final_trees$PLT_CN, tmax.extr)

tmean.extr <- data.frame(CORE_CN = final_trees$CN, TRE_CN = final_trees$TRE_CN, PLT_CN = final_trees$PLT_CN, tmean.extr)

#n.tmx.extr <- as.data.frame(n.tmx.extr)
#n.tmx.extr$TRE_CN <- val_trees$TRE_CN
# Export climate data
processed.path <- "Prism_formatted/"

save(ppt.extr,file = "ppt_extr.Rdata")
save(tmin.extr,file = "tmin_extr.Rdata")
save(tmax.extr,file = "tmax_extr.Rdata")
save(tmean.extr,file = "tmean_extr.Rdata")

write.csv(ppt.extr, paste0(processed.path,"ppt_extr.csv"), row.names = F)
write.csv(tmin.extr, paste0(processed.path,"tmin_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_extr.csv"), row.names = F)
write.csv(tmean.extr, paste0(processed.path,"tmean_extr.csv"), row.names = F)


