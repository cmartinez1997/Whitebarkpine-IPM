## Exploring WBP Aerial Detection Survey Data for data merge with FIA plot inventory data - for bayesian model
## Cecilia Martinez
## January 22, 2024
## cecimartinez333@gmail.com



# Load necessary packages -------------------------------------------------

library(tidyverse)
library(readr)


# load data ---------------------------------------------------------------

ADS_data <- read_csv("data_raw/ADS_wbp_outbreak.csv") 
head(ADS_data)
str(ADS_data)
summary(ADS_data)
# survey years from 1999-2013
# Measurement year: from 22010-2019


# plot data ---------------------------------------------------------------

nrow(ADS_data) #213 total plots/rows

#ADS survey years
ggplot(ADS_data, aes(x = SURVEY_YEAR, fill = factor(STATECD))) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "stack") +
  labs(title = "ADS Survey Years",
       x = "Survey Year",
       y = "# plots with whitebark pine outbreak") +
  scale_x_continuous(breaks = unique(ADS_data$SURVEY_YEAR)) +
  scale_fill_discrete(name = "State") +
  theme_classic()

#FIA Survey Years
ggplot(ADS_data, aes(x = MEASYEAR, fill = factor(STATECD))) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "stack") +
  labs(title = "FIA Measurement Years",
       x = "Survey Year",
       y = "# plots with whitebark pien outbreak") +
  scale_x_continuous(breaks = unique(ADS_data$MEASYEAR)) +
  scale_fill_discrete(name = "State") +
  theme_classic()

#FIA and ADS overlain
ggplot(ADS_data) +
  geom_histogram(aes(x = MEASYEAR, fill = "MEASYEAR"), binwidth = 1, color = "black", alpha = 0.7, position = "stack") +
  geom_histogram(aes(x = SURVEY_YEAR, fill = "SURVEY_YEAR"), binwidth = 1, color = "black", alpha = 0.4, position = "stack") +
  labs(title = "ADS and FIA data",
       x = "Year",
       y = "Frequency") +
  scale_fill_manual(values = c("MEASYEAR" = "black", "SURVEY_YEAR" = "cadetblue")) +
  scale_x_continuous(breaks = unique(c(ADS_data$MEASYEAR, ADS_data$SURVEY_YEAR))) +
  theme_classic()

# summary stats -----------------------------------------------------------

wbp_tree <- read_csv("data_processed/WBP_survival.csv")
ads_merge <- merge(ADS_data, wbp_tree, "PLT_CN", all.x = T, all.y = F)
nrow(ads_merge)


#see number of distinct PLT_CN
ads_merge %>% 
  select(PLT_CN) %>% 
  distinct() %>% 
  n_distinct() #205 distinct plots, we lost 13 somehow, maybe the plots havent been updated in FIAdb

ads_sorted <- ads_merge %>% arrange(PLT_CN)

#plot how many wbp lived or died within the plot
ggplot(ads_merge, aes(x = factor(STATECD.x), fill = factor(STATUSCD), group = factor(STATUSCD))) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Number of Trees within States by Status",
       x = "State Code",
       y = "Number of Trees") +
  theme_classic()


# plot on maps which trees lived or died ------------------------------------------------------------

library(maps)
m = map_data('state', region = c('Wyoming', 'Montana', 'Idaho'))

spatial_df <- ads_merge %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,STATUSCD) %>%
  distinct()

length(unique(spatial_df$TRE_CN)) #1811 trees

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATUSCD), alpha = .50))+
  ggtitle("Whitebark pine trees that died or lived during outbreak")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() +
  scale_colour_manual(values = c("1" = "darkgreen", "2" = "darkred"))


###february 21 2024
##explore with new data frame from Erin

ads_dat <- read_csv("ADS-dataexploration_files/bark_beetle_repeat.csv")

nrow(ads_dat) #41827 rows

#filter by species code, wbp SPCD=101, HOST_CODE = 101
head(ads_dat)

ads_wbp <- ads_dat %>% 
  filter(HOST_CODE ==101)
nrow(ads_wbp) #1670 entries with repeats




