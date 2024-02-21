## Exploring WBP recruitment data 
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com



# load packages -----------------------------------------------------------


library(tidyverse)



# read in data ------------------------------------------------------------

regen_dat <- read_csv("data_processed/WBP_recruit.csv")


# vizualize data ----------------------------------------------------------

# looking at the data
recruit_table1 <- table(regen_dat[, c("WBPadults1", "recruits1")])
colSums(recruit_table1)

recruit_table4 <- table(regen_dat[, c("WBPadults4", "recruits1")])
colSums(recruit_table4)

# no difference between adults > 1 in and > 4 inches

plot_info <- c("plot", "lat", "lon", "elev", "state", "county", "plotID", "WBPadults1", "recruits1")
colonize_plots <- regen_dat[regen_dat$WBPadults1 == 0 & regen_dat$recruits12 > 0, plot_info]

#want to make new column that combines all of WBP adults, ask question about this
regen_dat$WBPadults148 <- rowSums(regen_dat[, c("WBPadults1", "WBPadults4", "WBPadults8")])

# Subset data: only plots that contained PIAL trees at time 1 census
regen_dat <- subset(regen_dat, WBPadults148 > 0) #504 observations

# look at distribution of response variable
hist(regen_dat$recruits1) #lots of zeroes, poisson dist (ZIP)
hist(regen_dat$WBPadults1)

hist(regen_dat$WBPadults1, breaks = c(seq(0,110, by=1)), xlim = c(0, 110))
plot(regen_dat$WBPadults148, regen_dat$recruits1)
plot(regen_dat$WBPadults8, regen_dat$recruits1)
plot(regen_dat$WBPadults4, regen_dat$recruits1)
plot(regen_dat$WBPadults1, regen_dat$recruits1)

plot(regen_dat$BALIVE, regen_dat$recruits1)


# look at distribution of (some) covariates
hist(regen_dat$BALIVE, breaks = c(seq(0, 400, by = 10)), xlim = c(0, 400))
# looks like there's one outlier with very high BALIVE, could be eliminated, two outliers
regen_dat2 <- subset(regen_dat, BALIVE < 300) # 502
hist(regen_dat2$BALIVE, breaks = c(seq(0, 300, by = 10)), xlim = c(0, 300)) # get rid of outliers




