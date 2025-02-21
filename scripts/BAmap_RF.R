###Original code from Emily Schultz DRM:https://github.com/emilylschultz/DemographicRangeModel/blob/master/Code
##Updated by Cecilia Martinez
##July 2023
##cecimartinez333@gmail.com


library(mlr)
library(missForest)
library(raster)
library(ranger)

set.seed(123)

# Making BA map using Random Forest!!

# Path structure ---------------------------------------------------------

# path = "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/"


# Read data ---------------------------------------------------------------


######## basal area
BAdata <- read.csv("data_processed/BALIVEdata2.csv", header = T, stringsAsFactors = F)

######## climate layers - don't need to add just yet
# these created using the script "current.R"
#ppt_yr_raster <- raster(paste0(climate.path, "/PRISM/Normals/PPT_year.tif"))
#t_yr_raster <- raster(paste0(climate.path, "/PRISM/Normals/T_year.tif"))


climate_path <- "PRISM/data_formatted/"
ppt_raster <- raster(paste0(climate_path, "new_wbp_pptStack.tif"))
tmean_raster <- raster(paste0(climate_path, "new_wbp_tmeanStack.tif"))


# Prepare random forest settings ------------------------------------------


data = na.omit(BAdata[, c("BALIVE", "LAT", "LON", "MAT", "MAP")])

task = makeRegrTask(data = data, target = "BALIVE")


######## setup learner = model
getParamSet("regr.ranger") # see possible parameter
learner = makeLearner("regr.ranger", importance = "impurity")


######## parameter tuning
params = makeParamSet(makeIntegerParam("mtry", 1, 2),
                      makeIntegerParam("min.node.size", 1, 100))


tune_Control = makeTuneControlRandom(maxit = 10) 




# RF tuning ---------------------------------------------------------------

######## tune RF parameters
tune_Result = tuneParams(learner, task, measures = rmse, par.set = params, control = tune_Control,resampling = cv3)
learner_updated = setHyperPars(learner, par.vals = tune_Result$x)




# RF training -------------------------------------------------------------


######## training: 80% train, 20% test
sel = sample(1:nrow(data), ceiling(0.8*nrow(data)))
model = train(learner = learner_updated, task = task, subset = sel)
pred = predict(model, newdata = task$env$data[-sel,])


# Feature importance and performance evaluation on the test set
importance_values <-getFeatureImportance(model)$res
importance_values$importance/1e6

performance(pred, rmse)
performance(pred, rsq)
# 52.9 % Rsquared in independent sample
#ELS update: 40.8% Rsquared


######## training: 100% train for predicting with rasters
model = train(learner = learner_updated, task = task)
pred = predict(model, task = task) 

importance_values_train <-getFeatureImportance(model)$res
importance_values_train$importance/1e6

performance(pred, rmse) #40.57
performance(pred, rsq)  #63.99
# note: these performance measures are based on the training data and therefore the model fits much better!


# Generate BA map ---------------------------------------------------------

rasterData = data.frame(MAP = raster::getValues(ppt_raster),
  MAT = raster::getValues(tmean_raster),
  LAT = yFromCell(ppt_raster, 1:ncell(ppt_raster)),
  LON = xFromCell(ppt_raster, 1:ncell(ppt_raster)))
subs = complete.cases(rasterData)

pred = predict(model, newdata = rasterData[subs, ]) 
rasterPred = rep(NA, nrow(rasterData))
rasterPred[subs] = pred$data$response

balive <- ppt_raster
balive <- setValues(balive, as.vector(rasterPred))

plot(balive)


##Do this with ggplot
#rasterData_df <- as.data.frame(balive, xy = TRUE)
#ggplot() +
#  geom_raster(data = rasterData_df, aes(x = x, y = y, fill = balive)) +
#  scale_fill_viridis_c() +  # You can choose any color palette you prefer
#  theme_minimal() +
#  labs(x = "LON", y = "LAT", fill = "BALIVE")



# Save map ----------------------------------------------------------------

#pdf("BA/BAmap2_RF.pdf")
#ggplot() +
#  geom_raster(data = rasterData_df, aes(x = X, y = Y, fill = balive)) +
#  geom_raster(data = rasterData_df, aes(x = LON, y = LAT, fill = balive)) +
#  scale_fill_viridis_c() +  # You can choose any color palette you prefer
#  theme_minimal() +
#  labs(x = "LON", y = "LAT", fill = "BALIVE")
# dev.off()


pdf("scripts/BA/BAmap_RF.pdf")
plot(balive, main = "BALIVE")
dev.off()

# as raster (for use in building IPM)
writeRaster(balive, "scripts/BA/balive_RF.tif", overwrite = T)
