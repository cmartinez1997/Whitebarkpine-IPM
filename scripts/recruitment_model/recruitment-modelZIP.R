## Creating growth model
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
    select(-1)

# standardize covariates --------------------------------------------------
rec_dat_scaled <- recruitment %>% mutate_at(scale, .vars = vars(-plot, -lat, -lon, -elev, -PAwbp,
                                                        -state, -county, -plotID, -CONDID, 
                                                        -measyear, -plotprev, -PREV_MEASYEAR,
                                                        -CENSUS_INTERVAL, -recruits1, -recruits12,
                                                        -WBPadults1,
                                                        -WBPadults4, -WBPadults8, -WBPadults148, -cumDIA.WBP, -cumDIA.others))



# model ZIP ---------------------------------------------------------------

# choosing wbp >1 as adult
rec_model1 <- glmmTMB(recruits1 ~ 1
                      + BALIVE + I(BALIVE^2)
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(WBPadults1)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2),
                      data = rec_dat_scaled, 
                      family = "poisson")

rec_model2 <- glmmTMB(recruits1 ~ 1
                      + BALIVE + I(BALIVE^2)
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(WBPadults4)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2),
                      data = rec_dat_scaled, 
                      family = "poisson")

rec_model3 <- glmmTMB(recruits1 ~ 1
                      + BALIVE + I(BALIVE^2)
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(WBPadults8)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2),
                      data = rec_dat_scaled, 
                      family = "poisson")


# with offset = WBPadults4, same problem
# with offset = WBPadults8 can;t ber un without adding arbitrary non-zero value since lots of plots have zero WBp trees > 8in dbh
rec_model4 <- glmmTMB(recruits1 ~ 1
                      + BALIVE + I(BALIVE^2)
                      + offset(log(CENSUS_INTERVAL))
                      + offset(log(cumDIA.WBP)), # various alternatives for this offset
                      ziformula = ~ 1 
                      + BALIVE + I(BALIVE^2),
                      data = rec_dat_scaled, 
                      family = "poisson")


# model selection ---------------------------------------------------------

mod_comp<-model.sel(rec_model1,rec_model2, rec_model4)
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


### dealing with standardized covariates

# specify the predictors in the "best" model (or candidate best)
r_predictors <- c("BALIVE") # rec_model1

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

