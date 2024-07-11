## Exploring WBP Aerial Detection Survey Data for data merge with FIA plot inventory data - for bayesian model
## Cecilia Martinez
## January 22, 2024
## cecimartinez333@gmail.com



# Load necessary packages -------------------------------------------------

library(tidyverse)
library(readr)
library(kableExtra)

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

wbp_tree <- read_csv("data_processed/WBP_surv.csv")
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
m = map_data('state', region = c('Montana', 'Idaho'))

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

ads_wbp$PLT_CN <- as.character(ads_wbp$PLT_CN)

#ADS survey years
ggplot(ads_wbp, aes(x = SURVEY_YEAR)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "stack") +
  labs(title = "ADS Survey Years",
       x = "Survey Year",
       y = "# plots with whitebark pine mortality") +
  scale_x_continuous(breaks = unique(ADS_data$SURVEY_YEAR)) +
  theme_classic()

range(ads_wbp$SURVEY_YEAR) #survey years fo from 1999-2016

#filter by damage causing code, which is 11006 for bark beetle
ads_wbp <- ads_wbp %>% 
  filter(DCA_CODE == 11006)
nrow(ads_wbp) #1670, only damage causing agent for whitebark pine is bark beetle, this is good the number stays the same

##match plt_CN from this dataframe to the tree table in FIA
ads_wbp_tree <- read_csv("data_processed/WBP_surv.csv") #7434 trees
head(ads_wbp_tree)

#make plt_cn a character not numerical
ads_wbp_tree$PLT_CN <- as.character(ads_wbp_tree$PLT_CN)
ads_wbp <- as.character(ads_wbp$PLT_CN)
                        

#see if there are any duplicate PLT_CN values
unique_ads <- unique(ads_wbp$PLT_CN)
length(unique_ads) #811 unique ads PLT_CN so the rest are duplicated

#now merge
ads_comb <- left_join(ads_wbp, ads_wbp_tree, by = "PLT_CN", relationship = "many-to-many")
nrow(ads_merge) # weird now there are 1913 trees


# getting warning with merge, multiple duplicate CNs which is weird
# group by PLT_CN and combine survey_years
ads_wbp_comb <- ads_wbp %>%
  group_by(PLT_CN) %>%
  summarise(combined_years = toString(unique(SURVEY_YEAR)))

# Perform left join on the combined data frame and ads_wbp_tree
ads_wbp <- left_join(ads_wbp, ads_wbp_comb, by = "PLT_CN")
ads_wbp <- rename(ads_wbp, SURV_YEARS_ALL = combined_years)

ads_wbp_unique <- ads_wbp %>% 
  select(-SURVEY_YEAR) %>% 
  distinct()

#now merge with Tree table
ads_merge_final <- merge(ads_wbp_unique, ads_wbp_tree, "PLT_CN", all.x = T, all.y = F)
head(ads_merge_final) 
#filter to keep only trees that STATUSCD = 1 or NA

ads_merge_final <- ads_merge_final %>% 
  filter(STATUSCD == 2 )
nrow(ads_merge_final) #1418 trees

ads_merge_final %>%
  summarise(n_distinct(PLT_CN)) #116 distinct unique plots

ads_merge_final <- ads_merge_final %>% 
  select(-DIA_INCR_NEG) 



#### April 20 2024 ######

ads_montana_q3 <- ads_merge_final %>% 
  filter(STATECD == 30 & COUNTYCD %in% c(1, 7, 9, 23, 31, 39, 43, 57, 59, 67, 81, 93, 95, 97))
unique(ads_montana_q3$COUNTYCD) # so counties in ads are 1, 31, 57, 59, 67, 81, 93, 97

county_counts_ads <- ads_montana_q3 %>%
  count(COUNTYCD)

county_counts_ads %>%
  kable(caption = "Counties (FIPS codes) in MONTANA with ADS identified mortality (occurrences = number of trees that died") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>%
  row_spec(0, bold = TRUE, color = "black") # Darken the title

wbp_tree_montana_q3 <- wbp_tree %>% 
  filter(STATECD == 30 & COUNTYCD %in% c(1, 7, 9, 23, 31, 39, 43, 57, 59, 67, 81, 93, 95, 97)) %>% 
  filter(STATUSCD == 2 ) 
unique(wbp_tree_montana_q3$COUNTYCD) # counties with tree mortality are: 1, 7, 9, 23, 31, 39, 43, 57, 59, 67, 81, 93, 95, 97  

county_counts_fia <- wbp_tree_montana_q3 %>%
  count(COUNTYCD)

county_counts_fia %>%
  kable(caption = "Counties (FIPS codes) in MONTANA represented with mortality from all FIA plots") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>%
  row_spec(0, bold = TRUE, color = "black") # Darken the title

#write csv file
write_csv(ads_merge_final, "ADS-dataexploration_files/ads_merge_final.csv") #116 unique plots but we dont have wyoming? 



