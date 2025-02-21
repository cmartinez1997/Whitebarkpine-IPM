##Evaluate PIAL integral projection model

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(glmmTMB)
library(rasterVis)
library(RColorBrewer)
library(terra)
library(readr)
library(mgcv)

# Load vital rate and IPM functions

# define path
#path = "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/"
#path = "C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PIED_IPM/MEKEvans/"
path = "models/"
#PRISM.norm.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/PRISM/Normals/"
#PRISM.norm.path <-  "./ClimateData/PRISM/Normals/"

# load models and scaling --------------------------------------------------

load("models/Grow_Rescaling.Rdata")
load("models/Surv_Rescaling.Rdata")
load("models/RecruitRescaling.Rdata")
load("models/recrstats.Rdata")

# Load FIA survival, growth data
FIA <- read.csv("data_processed/model_surv_data.csv", header = T, stringsAsFactors = F)

# when running IPM without trees killed by fire, technically should filter out those trees
# prolly makes ~0 difference
#2719 trees
#FIA <- FIA[!(FIA$DSTRBCD1 %in% c(30, 31, 32, 80)), ]

xy = FIA[, c("LON", "LAT")]
spFIA = sp::SpatialPoints(xy)
spFIA = SpatialPointsDataFrame(spFIA, FIA) #why are you making it spatial here?

min_ba<-min(FIA$BALIVE, na.rm=T)
max_ba<-max(FIA$BALIVE, na.rm=T)
min_ppt<-min(FIA$MAP, na.rm=T)
max_ppt<-max(FIA$MAP, na.rm=T)
min_t<-min(FIA$MAT, na.rm=T)
max_t<-max(FIA$MAT, na.rm=T)


# data_test<-data.frame(MAP=seq(1, 1000, length.out = 100),BALIVE=110,MAT=8.9)
# test <- s.x(model = surv_model1, size.x = 8.4, data = data_test, interval = 10)
# plot(seq(1, 1000, length.out = 100), test)
# # look at survival as a function of Tann
# data_test<-data.frame(MAP=395,BALIVE=110,MAT=seq(-4, 30, length.out = 100))
# test = s.x(model = surv_model1, size.x = 8.4, data = data_test, interval = 10)
# plot(seq(-4, 30, length.out = 100), test)
# # look at survival as a function of balive, if balive is in mort model
# data_test<-data.frame(MAP=395,BALIVE=seq(0, 360, length.out = 100),MAT=8.9)
# test = s.x(model = surv_model1, size.x = 8.4, data = data_test, interval = 10)
# plot(seq(0, 360, length.out = 100), test, ylab = "10-yr survival probability", xlab = "basal area live trees")
# #abline(v = min(FIA$BALIVE)); abline(v = max(FIA$BALIVE)) # extrapolation lines
# abline(v = 239) # 239 is the maximum value in Michiel's interpolated BALIVE; 360 is the largest value observed at PIED plots
# 
# data_test<-data.frame(BALIVE=seq(0, 360, length.out = 100))
# test <- s.x(model = surv_model1, size.x = 8.4, data = data_test, interval = 10)
# plot(seq(0, 400, length.out = 100), test, ylab = "10-yr survival probability", xlab = "basal area live trees")
# abline(v = min(FIA$BALIVE)); abline(v = max(FIA$BALIVE)) # extrapolation lines


# try out g.yx
#d_growth <- g.yx(y, 10, balive = 110, PPTann = 348, Tann = 17) # time step = 1 year
dx<-0.1
y=seq(min(FIA$PREVDIA,na.rm=T),(max(FIA$PREVDIA,na.rm=T)+5),dx)
data_test<-data.frame(MAP=395,BALIVE=110,MAT=8.9)
d_growth <- g.yx(model = grow_model_02, growSD = grow_SD_02, size.x = 20, size.y = y, data = data_test, interval = 1, h = y[2]-y[1]) # time step = 1 year
plot(y, d_growth, type = "l", ylab = "density", xlim = c(30,40))
sum(d_growth)*dx
sum(d_growth)
# sum of d_growth should = 1.0 
# sum of d_growth*dx = 1 for small enough dx


# look at mean growth as a function of balive
data_test<-data.frame(MAP=395,BALIVE=110,MAT=8.9)
test = g.mean(model = grow_model_02, size.x = 8.4, data = data_test, interval = 10)
plot(seq(0, 240, length.out = 100), test, ylab = "10-yr growth incr (in)", xlab = "basal area live trees")
abline(v = min(FIA$BALIVE)); abline(v = max(FIA$BALIVE)) # extrapolation lines
abline(v = 190, col = "blue", lty = 2, lwd = 3) # clamping line

# try out fec (y's are the 500 mid-point sizes)
data_test<-data.frame(BALIVE=110,MAP=348,MAT=17)
d_recruit <- fec(model = rec_model1, y, 10, data = data_test) # time step = 1 year
data_test<-data.frame(BALIVE=88.6,PPT_yr=244,T_yr=7.5)
d_recruit <- fec(model = rec_model1, y, 1.0581, data = data_test) # smallest tree
d_recruit <- fec(model = rec_model1, y, 35, data = data_test) # largest tree
# the probability density looks the same, no matter what the size of the "parent" tree
# because our model of recruitment doesn't account for the influence of tree size
plot(y, d_recruit, type = "l", ylab = "density")

# Load climate layers
# these created using the script "current.R"
#ppt_yr_raster <- raster(paste0(PRISM.norm.path, "PPT_year.tif"))
#t_yr_raster <- raster(paste0(PRISM.norm.path, "T_year.tif"))
# stand-level basal area raster
ba_raster <- raster("scripts/BA/balive_RF.tif")

library(raster)
library(rgdal)


##extrap <- ppt_yr_raster
#for (i in 1:nrow(extrap)) {
#  for (j in 1:ncol(extrap)) {
#    extrap[i,j]<-ifelse(ppt_yr_raster[i,j]>max_ppt | ppt_yr_raster[i,j]<min_ppt |
#                          t_yr_raster[i,j]>max_t | t_yr_raster[i,j]<min_t |
#                          ba_raster[i,j]>max_ba | ba_raster[i,j]<min_ba,1,NA)
#  }
#}



# aggregate for now, just to make it faster
ppt_yr_raster <- aggregate(ppt_yr_raster, 4)
t_yr_raster <- aggregate(t_yr_raster, 4)
ba_raster <- aggregate(ba_raster, 4)


# Prepare empty rasters
lambda <- ba_raster
lambda <- setValues(lambda, NA)
growth <- ba_raster
growth <- setValues(growth, NA)
survival <- ba_raster
survival <- setValues(survival, NA)
reproduction <- ba_raster
reproduction <- setValues(reproduction, NA)

min.size <- 1*min(FIA$PREVDIA, na.rm = T) # minimum size is 1.0 inches diameter DBH
max.size <- 1.5*max(FIA$PREVDIA, na.rm = T) # maximum size is 59.2 inches DBH 
n_dim <- 500
# I think max.size should be smaller...59.1 inches DRC is totally unrealistic

dim_vec<-seq(100,1000,by=50)

dim_check <- data.frame(dim = dim_vec,
                        BALIVE = rep(median(FIA$BALIVE), length(dim_vec)), 
                        MAP = rep(median(FIA$MAP), length(dim_vec)), 
                        MAT = rep(median(FIA$MAT), length(dim_vec)),
                        lambda_c = rep(0, length(dim_vec)))
#lambda_ccl = rep(0, length(dim_vec)),
##lambda_cc = rep(0, length(dim_vec)),
##lambda_ccf = rep(0, length(dim_vec)),
#lambda_i = rep(0, length(dim_vec)))

## NOW checking the IPM Function - get P + R non-comformable arrays - explore this later
for (i in 1:length(dim_vec)) {
  dim_check[i, "lambda_c"] <- Re(eigen(ipm_fun(min=min.size, max=max.size, n=dim_vec[i], 
                                               gmodel=grow_model_02, 
                                               smodel=surv_model1, 
                                               rmodel=rec_model1, 
                                               gSD=grow_SD_02,
                                               data=dim_check[i,],
                                               s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F))$values[1])
  
  # dim_check[i, "lambda_ccl"] <- Re(eigen(ipm_fun(min=min.size, max=max.size, n=dim_vec[i], 
  #      gmodel=gmodel.DIA.q, 
  #     smodel=smodel.comp, 
  #    rmodel=rmodel_zip, 
  #   gSD=grow.SD.DIA.q,
  #    data=dim_check[i,],
  #   s.t.clamp=T, g.t.clamp=T, g.ba.clamp=T,r.ba.clamp=T))$values[1])
  #  dim_check[i, "lambda_cc"] <- Re(eigen(ipm_fun(min=min.size, max=max.size, n=dim_vec[i], 
  # gmodel=gmodel.clim.comp, 
  #smodel=smodel.clim.comp, 
  #rmodel=rmodel.clim.comp, 
  #gSD=growSD.clim.comp,
  #data=dim_check[i,],
  #s.t.clamp=T, g.t.clamp=F, g.ba.clamp=T,r.ba.clamp=T))$values[1])
  # dim_check[i, "lambda_ccf"] <- Re(eigen(ipm_fun(min=min.size, max=max.size, n=dim_vec[i], 
  #gmodel=gmodel.clim.comp, 
  #smodel=smodel.clim.comp.fire, 
  #rmodel=rmodel.clim.comp, 
  #gSD=growSD.clim.comp,
  #data=dim_check[i,],
  #s.t.clamp=T, g.t.clamp=F, g.ba.clamp=T,r.ba.clamp=T))$values[1])
  #dim_check[i, "lambda_i"] <- Re(eigen(ipm_fun(min=min.size, max=max.size, n=dim_vec[i], 
  #gmodel=gmodel.int, 
  #smodel=smodel.int, 
  #rmodel=rmodel.int, 
  #gSD=growSD.int,
  #data=dim_check[i,],
  #s.t.clamp=T, g.t.clamp=F, g.ba.clamp=T,r.ba.clamp=T))$values[1])
}

#i=49; j=79
nrow(ba_raster)
ncol(ba_raster)
# Build IPMs and calculate lambda, get error ##TAKES A WHILE
for (i in 1:nrow(ba_raster)) {
  for (j in 1:ncol(ba_raster)) {
    # Extract climate for cell
    pred_data <- data.frame(BALIVE=as.numeric(ba_raster[i,j]))
    #T_wd_norm=as.numeric(t_wd_raster[i,j]),
    #T_c_norm=as.numeric(t_c_raster[i,j]),
    #T_m_norm=as.numeric(t_m_raster[i,j]))
    # Check for missing value
    if (is.na(pred_data$BALIVE)) {
      lambda[i,j] <- NA
      print("Missing predictor... Skipping!")
      next
    }
    # Calculate lambda
    K<-ipm_fun(min=min.size, max=max.size, n=n_dim, gmodel=gmodel.DIA.q, smodel=smodel.comp, 
               rmodel=rmodel_zip, gSD=grow.SD.DIA.q,
               data=pred_data,
               s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F)
    lambda_val <- Re(eigen(K)$values[1])
    print(lambda_val)
    lambda[i,j] <- lambda_val
  }
  print(paste0("Finished row ", i, " of ", nrow(ba_raster)))
  plot(lambda, main = "Lambda"); points(LAT ~ LON, FIA, pch = 19, cex = 0.05)
}

# Fill growth, survival, reproduction rasters
for (i in 1:nrow(ba_raster)) {
  for (j in 1:ncol(ba_raster)) {
    # Extract climate for cell
    pred_data <- data.frame(BALIVE=as.numeric(ba_raster[i,j]))
    #T_wd_norm=as.numeric(t_wd_raster[i,j]),
    #T_c_norm=as.numeric(t_c_raster[i,j]),
    #T_m_norm=as.numeric(t_m_raster[i,j]))
    # Check for missing value
    if (is.na(pred_data$BALIVE)) {
      growth[i,j] <- NA
      survival[i,j] <- NA
      reproduction[i,j] <- NA
      print("Missing predictor... Skipping!")
      next
    }
    
    G <- g.mean(size.x = median(FIA$DIA, na.rm = T), model = gmodel.DIA.q, data=pred_data, t.clamp = T, ba.clamp = F) # interval = 1 in g.mean function
    # S <- s.x(size.x = median(FIA$DIA, na.rm = T), PPTann = ppt_yr_val, Tann = t_yr_val)
    S <- s.x(size.x = median(FIA$DIA, na.rm = T), model = smodel.comp, data=pred_data, t.clamp = F)
    R <- f.mean(model = rmodel_zip, data=pred_data, ba.clamp = F)
    growth[i,j] <- G
    survival[i,j] <- S
    reproduction[i,j] <- R
  }
  print(paste0("Finished row ", i, " of ", nrow(ba_raster)))
  plot(growth); points(LAT ~ LON, FIA, pch = 19, cex = 0.05)
}

# Export
writeRaster(lambda, "./Output/tifs/PIED.clim.int_lambda_gam.tif", overwrite = T)
writeRaster(growth, "./Output/tifs/PIED.int_growth_gam.tif", overwrite = T)
writeRaster(survival, "./Output/tifs/PIED.int_survival_gam.tif", overwrite = T)
writeRaster(reproduction, "./Output/tifs/PIED.int_reproduction_gam.tif", overwrite = T)

writeRaster(extrap,"./Output/tifs/extrap.tif", overwrite = T)

#Calculate lambdas for FIA plots
#Upload recruitment data, which has PIED presence/absence column, and summarize by plot
BAdata <- read.csv("data/BALIVEdata2.csv", header = T, stringsAsFactors = F)
BA.plot<-BAdata %>%
  group_by(PLT_CN) %>%
  summarise(lat=mean(LAT),lon=mean(LON),elev=mean(ELEV),BALIVE=mean(BALIVE)) 
BA.plot$lambda_c<-as.numeric(NA)
#BA.plot$lambda_ccl<-as.numeric(NA)
#BA.plot$lambda_cc<-as.numeric(NA)
#BA.plot$lambda_ccf<-as.numeric(NA)
#BA.plot$lambda_i<-as.numeric(NA)

FIA <- read.csv("data/WBP_data/WBPRecruitData.csv", header = T, stringsAsFactors = F)
FIA.plot<-FIA %>%
  group_by(plot) %>%
  summarise(lat=mean(lat),lon=mean(lon),elev=mean(elev),PAwbp=mean(PAwbp), BALIVE=mean(BALIVE)) 
FIA.plot$PAwbp<-as.factor(FIA.plot$PAwbp)
FIA.plot$lambda_c<-as.numeric(NA)
#FIA.plot$lambda_ci<-as.numeric(NA)
#FIA.plot$lambda_cc<-as.numeric(NA)
#FIA.plot$lambda_ccf<-as.numeric(NA)
#FIA.plot$lambda_i<-as.numeric(NA)

# Build IPMs and calculate lambda for FIA data/plots
## does not seem to be skipping lambda calculation for no predictor variable value 
for (i in 1:nrow(FIA.plot)) {
  # Extract climate for cell
  pred_data <- data.frame(#PPT_yr=(FIA.plot$PPT_yr_norm[i]),
    #T_yr=(FIA.plot$T_yr_norm[i]),
    BALIVE=(FIA.plot$BALIVE[i]))
  # Check for missing value
  if (is.na(pred_data$BALIVE)) {
    print("Missing predictor... Skipping!")
    next
  }
  # Calculate lambda and add to FIA plot dataframes, takes a while
  FIA.plot$lambda_c[i]<-Re(eigen(ipm_fun(min=min.size, max=max.size, n=n_dim, gmodel=gmodel.DIA.q, 
                                         smodel=smodel.comp,rmodel=rmodel_zip, gSD=grow.SD.DIA.q,
                                         data=pred_data,s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F))$values[1])
  #FIA.plot$lambda_ci[i]<-Re(eigen(ipm_fun(min=min.size, max=max.size, n=n_dim, gmodel=gmodel.clim.int.gam, 
  #                                       smodel=smodel.clim.int.gam,rmodel=rmodel.clim.int.gam, gSD=growSD.clim.gam,
  #                                      data=pred_data,s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F))$values[1])
  #FIA.plot$lambda_cc[i]<-Re(eigen(ipm_fun(min=min.size, max=max.size, n=n_dim, gmodel=gmodel.clim.comp.gam, 
  #                                       smodel=smodel.clim.comp.gam,rmodel=rmodel.clim.comp.gam, gSD=growSD.clim.comp.gam,
  #                                        data=pred_data,s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F))$values[1])
  #FIA.plot$lambda_i[i]<-Re(eigen(ipm_fun(min=min.size, max=max.size, n=n_dim, gmodel=gmodel.int.gam, 
  #                                     smodel=smodel.int.gam,rmodel=rmodel.int.gam, gSD=growSD.int.gam,
  #                                     data=pred_data,s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F))$values[1])
  print(FIA.plot$lambda_c[i])
  print(paste0("Finished row ", i, " of ", nrow(FIA.plot)))
}

write.csv(FIA.plot,"data/Outputs/FIA_lambdas.csv")

FIA_plot <- read.csv("data/Outputs/FIA_lambdas.csv")

wbp_fia_plot <- FIA_plot %>% 
  filter(PAwbp == 1) %>% 
  
  
  as.data.frame(wbp_fia_plot)

ggplot(wbp_fia_plot, aes(x = lambda_c)) +
  geom_histogram(bins = 50, fill = "goldenrod1", color = "black", alpha = 0.7)+ 
  labs(title = "Distribution of Lambda", x = "Lambda", y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  )

for (i in 1:nrow(FIA.plot)) {
  # Extract climate for cell
  pred_data <- data.frame(#PPT_yr=(FIA.plot$PPT_yr_norm[i]),
    #T_yr=(FIA.plot$T_yr_norm[i]),
    BALIVE=(FIA.plot$BALIVE[i]))
  # Check for missing value
  if (is.na(pred_data$BALIVE)) {
    print("Missing predictor... Skipping!")
    next
  }
}



FIA.plot_sf <- st_as_sf(FIA.plot, coords = c("lon", "lat"), crs = 4326)

states <- states(cb = TRUE) %>%
  filter(NAME %in% c("Wyoming", "Montana", "Idaho"))

states_sf <- st_as_sf(states)

heatmap_plot <- ggplot() +
  geom_sf(data = states_sf, fill = NA, color = "black") + # State borders
  geom_tile(data = FIA.plot, aes(x = lon, y = lat, fill = lambda_c)) +    # Plotting the points as tiles
  scale_fill_gradient(low = "blue", high = "red", name = "Lambda", limits = c(0.85, 1.25)) +
  labs(
    title = "Heatmap of Lambda Values",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


print(heatmap_plot)

# plotting lambdas



coordinates(FIA.plot) <- ~lon+lat
raster_template <- raster(extent(FIA.plot), res = 0.15) # Adjust resolution as needed

raster_lambda <- rasterize(FIA.plot, raster_template, field = "lambda_c", fun = mean)

# Convert raster to dataframe for ggplot2
raster_df <- as.data.frame(rasterToPoints(raster_lambda))
colnames(raster_df) <- c("lon", "lat", "lambda_c")

# Get state boundaries for Wyoming, Montana, and Idaho
states <- states(cb = TRUE) %>%
  filter(NAME %in% c("Wyoming", "Montana", "Idaho"))

# Convert states to sf object
states_sf <- st_as_sf(states)

# Create the tile heatmap plot
heatmap_plot <- ggplot() +
  geom_tile(data = raster_df, aes(x = lon, y = lat, fill = lambda_c)) +    # Plotting the raster data as tiles
  geom_sf(data = states_sf, fill = NA, color = "black") + # State borders
  scale_fill_gradient(low = "goldenrod3", high = "turquoise3", name = "λ", limits = c(0.85, 1.25)) +
  labs(
    title = "Heatmap of Lambda Values",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Display the plot
print(heatmap_plot)

# Save the plot to a file
ggsave("lambda_heatmap.png", heatmap_plot, width = 10, height = 6)
# Save the plot to a file
ggsave("lambda_heatmap.png", heatmap_plot, width = 10, height = 6)