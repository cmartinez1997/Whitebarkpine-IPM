## Creating growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)
library(performance) #to check model 


# Read in data used for model ---------------------------------------------

model_growth_data <- read_csv("data_processed/WBP_growth.csv")

model_growth_data <- model_growth_data %>% dplyr::select(TRE_CN, PREV_TRE_CN, PLT_CN, PREV_PLT_CN, LAT, LON, ELEV, 
                                                  DIA, PREVDIA, DIA_DIFF, DIA_INCR, MEASYEAR, PREV_MEASYEAR, CENSUS_INTERVAL, 
                                                  BALIVE, CONDID, STATUSCD, DSTRBCD1, DSTRBCD2, DSTRBCD3, AGENTCD)

#standardize numerical covariates, center and scale 

scaled_growth_data <- model_growth_data %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                      -MEASYEAR, -PREV_MEASYEAR,
                                                                      -CENSUS_INTERVAL, -DIA_INCR, -STATUSCD))


#growth model that includes plot level random effect

#this is the linear model 
grow_model_lin <- lmer(DIA_INCR ~ PREVDIA + BALIVE + (1|PLT_CN), data = scaled_growth_data)
class(grow_model_lin) <- "lmerMOD"

# check the model residuals in DHARMA: not looking so great 
plot(simulateResiduals(grow_model_lin, integerResponse = F), quantreg = T)
check_model(grow_model_lin)
#how to graph residuals with mixed models?? - ask for help

#check the residuals using DHARMA package
plot(grow_model_lin)
#quantile deviations detected 
#model prediction - rank transformed

#quadratic effects - check preference for model by using AICc
grow_model_quad <- lmer(DIA_INCR ~ PREVDIA + BALIVE  + 
                       I(PREVDIA^2) + I(BALIVE^2) +
                       (1|PLT_CN), data = scaled_growth_data)
qqnorm(residuals(grow_model_quad))

check_model(grow_model_quad)
summary(grow_model_quad)
plot(simulateResiduals(grow_model_quad, integerResponse = F), quantreg = T)
plot(grow_model_quad)

resid_pearson <- residuals(gmodel.DIA.q,type="pearson",scaled=TRUE)
predicted <- predict(gmodel.DIA.q)
plotLowess(resid_pearson~predicted, ylab="Residuals",xlab="Predicted", main="Gaussian")


#model selection step using AIC
mod_comp <- model.sel(grow_model_lin,grow_model_quad)
#better model is the quadratic, but linear has the lower AIC

grow_model_size <- lm(DIA ~ PREVDIA  + 
                        I(PREVDIA^2) , data = scaled_growth_data)


# growSD is used for building IPM (see BuildIPM.R)
grow_SD_quad <- sd(resid(grow_model_quad))

#make a model to check the IPM with only size as predictor
gmodel_check <- lmer(DIA_INCR ~ PREVDIA   + 
                       I(PREVDIA^2) +
                       (1|PLT_CN), data = scaled_growth_data)
grow_SD_check <- sd(resid(gmodel_check))

# specify the predictors in the exported models
grow_model_predictors <- c("PREVDIA", "BALIVE") 

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
for (i in gr.predictors) {
  attributes(scaled_grow_data[, i]) = NULL
}

plot(effect("BALIVE", gmodel.DIA.q))
plot(effect("BALIVE", gmodel.DIA_lin))
plot(effect("PREVDIA", gmodel.DIA.q))
plot(effect("PREVDIA", gmodel.DIA_lin))


# export model for coefficients and scaling information -------------------
#save(gmodel.7, gr.scaling, growSD, file = "./Code/IPM/GrRescaling.Rdata")
save(grow_model_quad, grow_SD_quad,grow_model_size, grow_SD_check,
     growth_scaling, scaled_growth_data, file = "models/Grow_Rescaling.Rdata")

