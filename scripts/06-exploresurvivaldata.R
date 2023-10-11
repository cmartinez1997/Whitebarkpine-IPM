## Exploring WBP growth data for growth model
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




### look at mortality, by PREV_MEASYEAR and MEASYEAR
surv_table1 <- table(wbp_survive_df[, c("PREV_MEASYEAR", "STATUSCD")])
wbp_survival1 <- surv_table1[,1]/(surv_table1[,1]+surv_table1[,2]) #calculates survival probabilities from the 1st census within the population at that time, for each year
# survival probability ranges from 0.2307 in 2010, to 0.5652 in 2011

surv_table2 <- table(wbp_survive_df[, c("MEASYEAR", "STATUSCD")])
wbp_survival2 <- surv_table2[,1]/(surv_table2[,1]+surv_table2[,2])
# survival ranges from 0.2498 to 0.4756 (lower overall survival in later years, trending downwards)

# make plot of survival rate as a function of cumulative PPT during drought (and anomaly)...like Clifford et al
# make plot of survival rate as a function of cumulative T anomaly during drought...like Clifford et al
# thanks to Jeff Oliver for help
library(tidyverse)
library(dplyr)
surv.table <- grow_data_surv %>%
  group_by(PLT_CN, CONDID) %>%
  summarise(survival.rate = sum(STATUSCD ==1)/n())
cols.to.include <- c("PLT_CN", "CONDID", "BALIVE", "ELEV", "LAT")
surv.table <- merge(x = surv.table,
                    y = grow_data_surv[!duplicated(grow_data_surv[c("PLT_CN", "CONDID")], ), cols.to.include],
                    by = c("PLT_CN", "CONDID"))

surv_competition <- ggplot(data = surv.table, aes(y = survival.rate, x = BALIVE)) + geom_point(aes(col = ELEV)) + stat_smooth(method = "glm", method.args = list(family = "binomial"))
surv_competition + scale_color_gradient(low="brown", high="chartreuse1")

surv_elevation <- ggplot(data = surv.table, aes(y = survival.rate, x = ELEV)) + geom_point(aes(col = BALIVE)) + stat_smooth(method = "glm", method.args = list(family = "binomial"))
surv_elevation + scale_color_gradient(low="brown", high="chartreuse1")

grow_data_surv$DIA_DIFF <- grow_data_surv$DIA - grow_data_surv$PREVDIA #getting the difference in diameters
