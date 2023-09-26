## Exploring WBP growth data for growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com

# Load necessary packages -------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(wesanderson)
library(ggthemr)

ggthemr("fresh", layout = "clean")


# Read in processed growth data -------------------------------------------
wbp_grow_df <- read_csv("data_processed/WBP_growth.csv") #2719 trees, 28 columns/fields

# Data exploration --------------------------------------------------------

# Looking at growth increment distribution,these units are in inches - this is response variable
ggplot(wbp_grow_df, aes(DIA_INCR)) + geom_histogram() #normal distributionish
# size distribution
ggplot(wbp_grow_df, aes(DIA)) + geom_histogram() #right skewed, slightly bimodal ish? outlier bigboy
ggplot(wbp_grow_df, aes(PREVDIA)) + geom_histogram() #right skewed slightly bimodal ish too? outlier at 29 ish inches
wbp_grow_df %>% ggplot() +
  geom_histogram(aes(x = PREVDIA, fill = "PREVDIA"), alpha = 0.6) +
  geom_histogram(aes(x = DIA, fill = "DIA"), alpha = 0.6) +
  labs(title = "Diameter of WBP", x = "Diameter (in.)", y = "Frequency")

# Look at if the trees, grew, shrunk or stayed the same
ggplot(wbp_grow_df, aes(x = PREVDIA, y = DIA)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5, size = 2.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Growth/Shrinkage/Stasis",
       x = "PREVDIA",
       y = "DIA")
# if they are above the line, they are gorwing, on the line, stay the same, below the line, they are shrinking
ggplot(wbp_grow_df, aes(x = PREVDIA, y = DIA_INCR_NEG)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.5, size = 2.5) +
  labs(title = "Growth/Shrinkage/Stasis",
       x = "PREVDIA",
       y = "DIA_INCR_NEG")


# look at some summary statistics 
wbp_grow_df %>%
  reframe(summary(DIA)) #largest tree = 29.9 in #smallest tree = 1.0 in
wbp_grow_df %>% mean(DIA)
mean(wbp_grow_df$DIA)

wbp_grow_df %>% count(DIA == PREVDIA) #390 stayed the same, #2329 changed, #2233 grew


# Distribution of predictor variable
# Original BALIVE histogram
ggplot(wbp_grow_df, aes(x = BALIVE)) +
  geom_histogram(binwidth = 15, alpha = 0.8) +
  labs(title = "Histogram of BALIVE", x = "BALIVE", y = "Frequency") #normalish

# Log-transformed BALIVE histogram
ggplot(wbp_grow_df, aes(x = log(BALIVE))) +
  geom_histogram(binwidth = 0.2, alpha = 0.8) +
  labs(title = "Histogram of log(BALIVE)", x = "log(BALIVE)", y = "Frequency")

# diameter increment growth - difference and incr vs BALIVE
ggplot(wbp_grow_df, aes(x = BALIVE, y = DIA_DIFF)) +
  geom_point() +
  labs(title = "Scatterplot of Basal Area vs. DIA_DIFF",
       x = "BALIVE",
       y = "(DIA_DIFF)") + 
  ylim(0, max(wbp_grow_df$DIA_DIFF)) #96 rows missing 

ggplot(wbp_grow_df, aes(x = BALIVE, y = DIA_INCR)) +
  geom_point() +
  labs(title = "Scatterplot of Basal Area vs. DIA_INCR",
       x = "BALIVE",
       y = "(DIA_INCR)") +
  ylim(0.29, max(wbp_grow_df$DIA_INCR))

