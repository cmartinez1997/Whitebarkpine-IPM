## Exploring WBP growth data for growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com



# Load necessary packages -------------------------------------------------

library(ggplot2)
library(wesanderson)
library(tidyverse)


# Read in and process surviva data ------------------------------------------------
wbp_survival <- read_csv("data_processed/WBP_surv.csv") 

# explore data ------------------------------------------------------------

# look at mortality, by PREV_MEASYEAR and MEASYEAR
surv_table1 <- table(wbp_survival[, c("PREV_MEASYEAR", "STATUSCD")])
wbp_survival1 <- surv_table1[,1]/(surv_table1[,1]+surv_table1[,2]) #calculates survival probabilities from the 1st census within the population at that time, for each year
barplot(wbp_survival1, 
        names.arg = names(wbp_survival1), 
        col = "skyblue", 
        main = "Survival Probability Over Years",
        xlab = "Year",
        ylab = "Survival Probability")

# survival probability ranges from 0.2307 in 2010, to 0.5652 in 2011, but note that sample size is much lower in 2011 than in previous years
surv_table2 <- table(wbp_survive_df[, c("MEASYEAR", "STATUSCD")])
wbp_survival2 <- surv_table2[,1]/(surv_table2[,1]+surv_table2[,2]) # calculates survival probability from the 2nd census within population 
barplot(wbp_survival2, 
        names.arg = names(wbp_survival2), 
        col = "skyblue", 
        main = "Survival Probability Over Years",
        xlab = "Year",
        ylab = "Survival Probability")
# survival ranges from 0.2498 to 0.4756 (lower overall survival in later years, trending downwards)

# Making plots of survied vs died by states 
# Make new column to give meaningful levels to these statuscodes and then make a barplot
wbp_survival$STATUSCODE <- factor(wbp_survival$STATUSCD, levels = c(1, 2), labels = c("Lived", "Died"))
# plot of total died vs total suyrvived
ggplot(wbp_survival, aes(x = STATUSCODE, fill = STATUSCODE)) +
  geom_bar() +
  labs(title = "Total Survival and Mortality") +
  xlab("Status") 

# how many trees died and how many trees lived
# Count the number of individuals in each category
status_counts <- table(wbp_survival$STATUSCODE)

# Count of individuals who lived
lived_count <- status_counts["Lived"]

# Count of individuals who died
died_count <- status_counts["Died"]

# Print the counts
print(paste("Lived:", lived_count))
print(paste("Died:", died_count)) #4715 died from 2003-2019

range(wbp_survival$INVYR) #2013-2019
range(wbp_survival$PREV_MEASYEAR) #2003-2011
# plot by alive and died by state
ggplot(data = wbp_survival, aes(x = as.factor(STATECD), fill = STATUSCODE)) +
  geom_bar(position = "dodge") +
  labs(title = "Total Survival and Mortality") +
  xlab("State")

# make plot of survival rate as a function of cumulative PPT during drought (and anomaly)...like Clifford et al
# make plot of survival rate as a function of cumulative T anomaly during drought...like Clifford et al
# thanks to Jeff Oliver for help

surv_table <- wbp_survival %>%
  group_by(PLT_CN, CONDID) %>%
  summarise(survival_rate = sum(STATUSCD ==1)/n())
cols_to_include <- c("PLT_CN", "CONDID", "BALIVE", "ELEV", "LAT")
surv_table <- merge(x = surv_table,
                    y = wbp_survival[!duplicated(wbp_survival[c("PLT_CN", "CONDID")], ), cols_to_include],
                    by = c("PLT_CN", "CONDID"))

surv_competition <- ggplot(data = surv_table, aes(y = survival_rate, x = BALIVE)) + 
  geom_point(aes(col = ELEV)) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial")) + 
  theme_classic()
surv_competition + scale_color_gradient(low="brown", high="chartreuse1")

surv_elevation <- ggplot(data = surv_table, aes(y = survival_rate, x = ELEV)) + 
  geom_point(aes(col = BALIVE)) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial")) + 
  theme_c
surv_elevation + scale_color_gradient(low="brown", high="chartreuse1")

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



