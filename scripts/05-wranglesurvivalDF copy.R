## Making WBP data frame for survival model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(wesanderson)
library(tidyverse)

# Read in and process data ------------------------------------------------

# read tree data and filter to only apply to whitebark pine trees (PIAL); SPCD = 101 in FIA
tree_data <- read_csv("data_processed/TREE_MT-ID-WY.csv") #773058 total trees
tree_data_WBP <- filter(tree_data, SPCD == 101) #36,403 wbp trees
tree_data_WBP <- tree_data_WBP %>% 
  rename(TRE_CN = CN) #rename TRE_CN for CN in tree table

# Only keep remeasured trees coded as dead or alive
### STATUSCD = 0 = no status (not in sample)
### STATUSCD = 1 = live tree
### STATUSCD = 2 = dead tree
### STATUSCD = 3 = harvested tree
wbp_remeasured <- tree_data_WBP %>%
  filter(!is.na(PREVDIA), STATUSCD %in% c(1,2)) #7817 trees 

wbp_survive <- wbp_remeasured

# look up previous PLT_CN and CONDID and add columns to data frame
wbp_survive_df <- wbp_survive %>% 
  mutate(PREV_PLT_CN = tree_data_WBP$PLT_CN[match(PREV_TRE_CN, tree_data_WBP$TRE_CN)]) %>% 
  mutate(DIA_DIFF = wbp_survive$DIA - wbp_survive$PREVDIA) %>%  #diameter is dbh in inches
  mutate(PREV_CONDID = tree_data_WBP$CONDID[match(wbp_survive$PREV_TRE_CN, tree_data_WBP$TRE_CN)])

wbp_survive_df <- wbp_survive_df %>% dplyr::select(TRE_CN, PLT_CN, PREV_TRE_CN, PREV_PLT_CN, DIA_DIFF, SPCD, INVYR, STATECD, 
                                                       UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, PREVCOND, STATUSCD, DIA, 
                                                       STOCKING, PREVDIA, PREV_CONDID, AGENTCD)

# Read in plot data and get coordinates and previous measurement year
plot_iw <- read_csv("data_processed/PLOT_MT-ID-WY.csv")

# add columns, LAT/LON/ELEV/MEASYEAR/PREV_MEASYEAR/CENSUS_INTERVAL to wbp growth data frame from plot table, add census interval column
wbp_survive_df <- wbp_survive_df %>%  
  mutate(LAT = plot_iw$LAT[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(LON = plot_iw$LON[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(ELEV = plot_iw$ELEV[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(MEASYEAR = plot_iw$MEASYEAR[match(wbp_survive_df$PLT_CN, plot_iw$CN)]) %>% 
  mutate(PREV_MEASYEAR = plot_iw$MEASYEAR[match(wbp_survive_df$PREV_PLT_CN, plot_iw$CN)]) %>% 
  mutate(CENSUS_INTERVAL = MEASYEAR - PREV_MEASYEAR)

options(scipen = 999) #gets rid of scientific notation

# look up previous (tree-specific) condition-level BALIVE(Basal area in square feet per acre of all live trees) 
cond_iw <- read_csv("data_processed/COND_MT-ID-WY.csv")

wbp_survive_df$BALIVE <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                    MARGIN = 1, # applies function to each row in wbp_survive_df
                                    FUN = function(x, conds.df) {
                                      conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                        conds.df$CONDID %in% x["PREV_CONDID"]]
                                    },
                                    conds.df = cond_iw)
# also look up disturbance codes and add them to the dataframe
wbp_survive_df$DSTRBCD1 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in wbp_survive_df
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD1[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

wbp_survive_df$DSTRBCD2 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in wbp_survive_df
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD2[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

wbp_survive_df$DSTRBCD3 <- apply(X = wbp_survive_df[, c("PREV_PLT_CN", "PREV_CONDID")], 
                                      MARGIN = 1, # applies function to each row in grData_remeas
                                      FUN = function(x, conds.df) {
                                        conds.df$DSTRBCD3[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                            conds.df$CONDID %in% x["PREV_CONDID"]]
                                      },
                                      conds.df = cond_iw)

#make BALIVE numeric instead of list so that NAs pop up and then remove the NA values 
wbp_survive_df$BALIVE <- as.numeric(wbp_survive_df$BALIVE)
wbp_survive_df <- wbp_survive_df %>% filter(!is.na(BALIVE)) #now we have 7434 trees with BALIVE predictor variable
  #hmm some values are 0 for BALIVE, this is the initial BALIVE condition, what does this mean? were they on a non-forested condition
  #explore what the condition was of these trees, are they in krummholz and <10% stocked, in which case they are not forested

#Search for CONDIDs
### CONDID = 1 = no status (not in sample)
### CONDID = 2 = Accessible nonforested land (<10% tally trees) 
### CONDID = 3 = Noncensused water? why the heck are these in there, when they have clearly been censused

# Create increment columns - maybe not needed for analysis; but we need to sprinkle death in there
# not needed for survival/mort analysis as of now
wbp_survive_df$DIA_INCR <- wbp_survive_df$DIA_DIFF / wbp_survive_df$CENSUS_INTERVAL

# Recode status for mortality and for survival instead of just status code
wbp_survive_df$survival <- ifelse(wbp_survive_df$STATUSCD == 2, 0, 1)
wbp_survive_df$mortality <- ifelse(wbp_survive_df$STATUSCD == 1, 0, 1)

# remove cases where BALIVE at time 1 = zero (this should be impossible)
# survData <- subset(survData, log.BALIVE > 0) 
wbp_survive_df_BA0 <- wbp_survive_df %>%
  filter(BALIVE > 0)
  # this makes sample go from 7424 to 7126

# remove conditions where fire or harvest occurred
wbp_survive_df_nofire <- wbp_survive_df_BA0 %>%
  filter(!(DSTRBCD1 %in% c(30, 31, 32, 80)))


# Look at response variable - survived vs died
ggthemr::ggthemr("fresh", layout = "clean")

# Make new column to give meaningful levels to these statuscodes and then make a barplot
wbp_survive_df$STATUSCODE <- factor(wbp_survive_df$STATUSCD, levels = c(1, 2), labels = c("Lived", "Died"))
ggthemr::ggthemr("fresh", layout = "clean")
ggplot(data = wbp_survive_df, aes(x = STATUSCODE, fill = STATECD)) +
  geom_bar() +
  labs(title = "Total Survival and Mortality") +
  xlab("Status") 

# plot by alive and died by state
ggplot(data = wbp_survive_df, aes(x = as.factor(STATECD), fill = STATUSCODE)) +
  geom_bar(position = "dodge") +
  labs(title = "Total Survival and Mortality") +
  xlab("State")

# Look at some predictor variables

# Look at distribution of size - PREVDIA
ggplot(wbp_survive_df, aes(x = PREVDIA)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  xlim(0, 40) +
  labs(title = "Histogram of PREVDIA",
       x = "PREVDIA",
       y = "Frequency")

# Look at size distribution when log tranforming previous size
ggplot(wbp_survive_df, aes(x = log(PREVDIA))) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue") +
  xlim(log(0.5), log(40)) +
  labs(title = "Histogram of log(PREVDIA)",
       x = "log(PREVDIA)",
       y = "Frequency")
# size is lognormal-ish, with a wonky bit at the small end of the scale 
# (due to min size threshold?) only measure certain DBH and also long tail

# Create a ggplot for the original BALIVE values
ggplot(wbp_survive_df, aes(x = BALIVE)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "black") +
  labs(title = "Histogram of BALIVE") +
  xlab("BALIVE")

# Create a ggplot for the log-transformed "BALIVE" values
ggplot(wbp_survive_df, aes(x = log(BALIVE))) +
  geom_histogram(binwidth = 0.3, fill = "lightblue", color = "black") +
  labs(title = "Histogram of log(BALIVE)") +
  xlab("log(BALIVE)")


# standardize covariates
#ELS update: no AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3 in dataframe, so I removed them from the following code
survData.scaled <- survData %>% mutate_at(scale, .vars = vars(-CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                              -CD, -MEASYEAR, -PREV_MEASYEAR, 
                                                              -CENSUS_INTERVAL,
                                                              AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                              -DIA_INCR,
                                                              -surv, -mort))

survData2.scaled <- survData.2 %>% mutate_at(scale, .vars = vars(-CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                 -STATUSCD, -MEASYEAR, -PREV_MEASYEAR, 
                                                                 -CENSUS_INTERVAL,
                                                                 AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                                 -DIA_INCR, 
                                                                 -surv, -mort))

survData3.scaled <- survData.3 %>% mutate_at(scale, .vars = vars(-CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                 -STATUSCD, -MEASYEAR, -PREV_MEASYEAR, 
                                                                 -CENSUS_INTERVAL,
                                                                 AGENTCD, DSTRBCD1, DSTRBCD2, DSTRBCD3,
                                                                 -DIA_INCR, 
                                                                 -surv, -mort))


library(lme4)
library(easystats)
library(patchwork)
#library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)

#Compare different size predictors
smodel.0b <- glmer(mort ~ PREVDIA + I(PREVDIA^2) + BALIVE + (1|PLT_CN) + offset(log(CENSUS_INTERVAL)), 
                   family = binomial(link = "cloglog"), data = survData3.scaled,
                   control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
summary(smodel.0b)

plot(allEffects(smodel.0b))
plot(effect("BALIVE",smodel.0b)) #the results of BALIVE (ie: competition) is negative
#interpret these model results

plot(simulateResiduals(smodel.0b))
#what do these residual plots mean
check_model(smodel.0b)

plotResiduals(survData.scaled$PREVDIA, res$scaledResiduals, quantreg = T, main = "PREVDIA") #resid plot doesn't look good

#Demonstrate the effect of quadratics
smodel.lin <- glmer(mort ~ PREVDIA + BALIVE + (1|PLT_CN) + offset(log(CENSUS_INTERVAL)), 
                    family = binomial(link = "cloglog"), data = survData3.scaled,
                    control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
smodel.q <- glmer(mort ~ PREVDIA + BALIVE + 
                    I(PREVDIA^2) + I(BALIVE^2) + (1|PLT_CN) + offset(log(CENSUS_INTERVAL)), 
                  family = binomial(link = "cloglog"), data = survData3.scaled,
                  control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
#model selection

mod.comp0<-model.sel(smodel.lin,smodel.q, smodel.0b)
mod.comp0
# strong preference for quadratics (delta AIC = 69.31)

# compare VPD and temperature

# Models to export:

smodel.comp<-glmer(mort ~ PREVDIA + BALIVE + 
                     I(PREVDIA^2) + I(BALIVE^2) + 
                     (1|PLT_CN) + offset(log(CENSUS_INTERVAL)), 
                   family = binomial(link = "cloglog"), data = survData3.scaled,
                   control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=10000)))
res1 = simulateResiduals(smodel.comp)
plot(res1) #p=0.25375


# specify the predictors in the "best" model (or candidate best)
#surv.predictors <- c("PREVDIA", "T_yr_norm", "PPT_yr_norm")
surv.predictors <- c("PREVDIA","BALIVE")
# eventually rewrite this so that it can handle alternative "best" models

get_scale = function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

surv.scaling = get_scale(survData3.scaled, surv.predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in surv.predictors) {
  attributes(survData.scaled[, i]) = NULL
}

# export model for coefficients and scaling information -------------------
#save(smodel4.q, surv.scaling, file = "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/Code/IPM/SurvRescaling.Rdata")
#save(smodel4, surv.scaling, file = "./Code/IPM/SurvRescalingNoFire.Rdata")
#save(smodel3, surv.scaling, file = "./Code/IPM/SurvRescalingBA.Rdata")
save(smodel.comp,
     surv.scaling, file = "data/WBP_data/SurvRescaling.Rdata")




# Create output data frame
write_csv(wbp_survive_df, "data_processed/WBP_survival.csv")


