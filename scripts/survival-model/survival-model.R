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
  filter(BALIVE > 0) # goes from 7434 to 7136 observations

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


# now run models ----------------------------------------------------------

# run first survival model
surv_model1 <- glmer(mort ~ PREVDIA + I(PREVDIA^2) + BALIVE + (1|PLT_CN) + offset(log(REMEAS_PERIOD)), 
                   family = binomial(link = "cloglog"), data = wbp_surv_dat3_scaled,
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
surv_model2 <- glmer(mort ~ PREVDIA + BALIVE + (1|PLT_CN) + offset(log(REMEAS_PERIOD)), 
                    family = binomial(link = "cloglog"), data = wbp_surv_dat3_scaled,
                    control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
surv_model3 <- glmer(mort ~ PREVDIA + BALIVE + 
                    I(PREVDIA^2) + I(BALIVE^2) + (1|PLT_CN) + offset(log(REMEAS_PERIOD)), 
                  family = binomial(link = "cloglog"), data = wbp_surv_dat3_scaled,
                  control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
#model selection

mod_comp<-model.sel(surv_model1,surv_model2, surv_model3)
mod_comp
# strong preference for survival model1

# specify the predictors in the "best" model (or candidate best)
surv_predictors <- c("PREVDIA","BALIVE")

get_scale <- function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

surv_scaling <- get_scale(wbp_surv_dat3_scaled, surv_predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in surv_predictors) {
  attributes(wbp_surv_dat_scaled[, i]) = NULL
}

# export model for coefficients and scaling information -------------------
#save(smodel4.q, surv.scaling, file = "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/Code/IPM/SurvRescaling.Rdata")
#save(smodel4, surv.scaling, file = "./Code/IPM/SurvRescalingNoFire.Rdata")
#save(smodel3, surv.scaling, file = "./Code/IPM/SurvRescalingBA.Rdata")
save(surv_model1,
     surv_scaling, file = "models/Surv_Rescaling.Rdata")

