





# libraries ---------------------------------------------------------------

library(dplR)
library(tidyverse) # for data wrangling
library(here)
library(dplR)
library(shiny)
library(maps)
library(sf)
library(ggplot2)
library(viridis)
library(kableExtra)



# read in data ------------------------------------------------------------



plot_43 <- read.rwl("treeringdata_counties/30_1/43-3-3.rwl")


# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("data_raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("data_raw", "T_iwfia_whitebark.txt"))

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe

wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))

length(unique(wbp_rw$TRE_CN)) #219 unique trees with CN numbers - 219 samples of wbp that can be linked to trees - 219 trees

wbp_rw$TRE_CN <- as.character(wbp_rw$TRE_CN)
wbp_rw$PLT_CN <- as.character(wbp_rw$PLT_CN)

##merge the inventory data from FIAdb with tree ring 

tree_dat_wbp <- read_csv("data_processed/tree_dat_wbp_mt.csv")
# filter only necessary columns 

wbp_fia_beaverhead <- tree_dat_wbp %>% 
  select(TRE_CN, PLT_CN, PREV_TRE_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, TREE, CONDID, STATUSCD, LAT, LON, ELEV, MEASYEAR)

#need to filter trees from specific counties for our analysis - only include beaverhead county

mt_meta_beaverhead <- wbp_meta %>% 
  filter(!is.na(TRE_CN) & !is.na(PLT_CN)) %>% 
  filter(STATECD == 30) %>% 
  filter(COUNTYCD == 1) #only 16 samples have a tree cn associated with them

mt_fia_beaverhead <- left_join(mt_meta_beaverhead, wbp_fia_beaverhead, by = "TRE_CN")
mt_fia_beaverhead <- mt_fia_beaverhead %>% select(!ends_with(".x"))


# okay this adds the state codes so we filter out hte ring widths thaat dont have state codes or county codes associated withthem, flathead county specifically
mt_fia_beaverhead$TRE_CN <- as.character(mt_fia_beaverhead$TRE_CN)
mt_fia_beaverhead$PLT_CN <- as.character(mt_fia_beaverhead$PLT_CN.y)

wbp_rw <- left_join(wbp_rw, wbp_meta, by = "TRE_CN")
wbp_rw <- wbp_rw %>% select(TRE_CN, Year, RW, STATECD, COUNTYCD)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN))

mt_rw_beaverhead <- wbp_rw %>% 
  filter(!is.na(TRE_CN)) %>% 
  filter(STATECD == 30) %>% 
  filter(COUNTYCD == 1) #79 cores from these counties for our analysis

mt_rw_beaverhead <- left_join(mt_rw_beaverhead, mt_fia_beaverhead)

mt_fia_beaverhead$TRE_CN <- as.character(mt_fia_beaverhead$TRE_CN)



#write data to csv so don't have to mess with this table ever again
write_csv(mt_rw_flathead, "data_processed/mt_rw_flathead.csv")
write_csv(mt_meta_flathead_fiadb, "data_processed/mt_meta_flathead_fiadb.csv")



# reread in the data
mt_rw_beaverhead <- read_csv("data_processed/mt_rw_flathead.csv")
mt_fia_beaverhead <- read_csv("data_processed/mt_meta_flathead_fiadb.csv")

mt_rw_beaverhead_df <- mt_rw_beaverhead %>% 
  select(CN, TRE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT.y, SUBP.y, TREE.y, SPCD, DIA, MEASYEAR.y, PITH, RINGCOUNT, CONDID, 
         LAT, LON, ELEV)

mt_rw_beaverhead_df$CN <- as.character(mt_rw_beaverhead_df$CN)

unique_cn <- mt_rw_beaverhead_df %>% distinct(CN)


mt_rw_beaverhead_df <- mt_rw_beaverhead %>% 
  select(CN, TRE_CN, PLT_CN, Year, RW, STATECD, COUNTYCD, PLOT.y, SUBP.y, TREE.y, SPCD, DIA, MEASYEAR.y, PITH, RINGCOUNT, CONDID, 
         LAT, LON, ELEV)




mt_43_usu <- mt_rw_beaverhead_df %>% 
  filter(CN == 289859185489998) %>% 
  select(Year, RW) 

## plot them together 
ggplot(mt_44_df, aes(x = Year, y = RW, color = "mt_44_ltrr", fill = "black")) +
  geom_line() +
  geom_line(data = mt_44_usu, aes(x = Year, y = RW, color = "mt_44_usu")) +
  labs(title = "MT Sample 44 Comparison",
       x = "Year",
       y = "Ring Width") +
  scale_x_continuous(limits = c(1900, 2000)) +
  theme_bw()
#plot utah data


## okay now do this for 58
mt_58 <- read.tucson("data_processed/montana_fia_rwl/county29_flathead/058-5-5.rwl")
head(mt_58)

mt_58 <- mt_58 %>%
  rename(RW = "058-5-5")


mt_58_df <- data.frame(Year = as.numeric(row.names(mt_58)), RW = mt_58$RW)
str(mt_58_df)

mt_58_usu <- mt_rw_flathead_df %>% 
  filter(CN == 289859808489998) %>% 
  select(Year, RW) 

## plot them together 
ggplot(mt_58_df, aes(x = Year, y = RW, color = "mt_58_ltrr")) +
  geom_line() +
  geom_line(data = mt_58_usu, aes(x = Year, y = RW, color = "mt_58_usu")) +
  labs(title = "MT Sample 58 Comparison",
       x = "Year",
       y = "Ring Width") +
  scale_x_continuous(limits = c(1900, 2000)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw()


#county 77_sample_15

mt_15_usu <- wbp_rw %>% 
  filter(CN == 289859808489998) %>% 
  select(Year, RW) d

ggplot(mt_15_usu, aes(x = Year, y = RW, color = "mt_15_usu")) +
  geom_line() +
  labs(title = "MT Sample 15 RW",
       x = "Year",
       y = "Ring Width") +
  scale_x_continuous(limits = c(1900, 2000)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw()

# get the starting and ending years for each core
core_years <- mt_rw_flathead %>%
  group_by(CN) %>%
  summarise(start_year = min(Year),
            end_year = max(Year))

# Calculate the length of each core (difference between end year and start year)
core_years <- core_years %>%
  mutate(core_length = end_year - start_year + 1) %>%
  arrange(desc(core_length))# Adding 1 to include both start and end years

# Print the result
print(core_years_table)

core_years_table <- core_years %>%
  kable(caption = "Core Years") %>%
  kable_styling(full_width = FALSE)

#for better viszdo this
split(core_years, ceiling(seq_len(nrow(core_years)) / (nrow(core_years) / 2))) %>%
  lapply(function(df) {
    df %>%
      kable(caption = "Core Years") %>%
      kable_styling(full_width = FALSE)
  }) %>%
  print()


# Print the table
print(core_years_table)
##Map out where our ring width data are: 

# MAPPING

spatial_df_flathead <- mt_meta_flathead_fiadb %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON) %>%
  distinct() %>%  
  na.omit(spatial_df_flathead)

# make table from dataframe for lat longs in flathead county
kable(spatial_df_flathead, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray") %>%
  column_spec(1:3, bold = TRUE)


m = map_data('state', region = ('Montana'))
montana_counties <- subset(counties, region == "montana")

# Create a data frame for the reference MT172 chronology points
reference_point_mt172 <- data.frame(LAT = 48.8333, LON = -111.5) #MT 172
reference_point_mt158 <- data.frame(LAT = 45.1667, LON = -109.5167) #MT 158

ggplot() + 
  geom_polygon(data = m, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + 
  geom_point(data = spatial_df_flathead, aes(x = LON, y = LAT), color = "turquoise4", alpha = 0.70) +
  geom_point(data = reference_point_mt172, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_point(data = reference_point_mt158, aes(x = LON, y = LAT), color = "darkred", size = 3.5) +  
  geom_polygon(data = montana_counties, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  xlab('Longitude') +
  ylab('Latitude') +
  coord_fixed() +
  theme_bw()


rw_data_flathead <- mt_rw_flathead %>%
  ungroup() %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data_flathead <- spread(rw_data_flathead,key = "TRE_CN",value = RW,fill = NA,drop = FALSE) 

# Get column names of the dataframe
col_names_flathead <- colnames(rw_data_flathead)

# Remove the last 5 digits from each column name bc tucson decadal format can't handle more than 8 digits
new_col_names <- sub("\\d{5}$", "", col_names_flathead)


#Load in chronology from itrdb from northern montana - daviud sauchwyn 

itrdb_mt <- read.rwl("data_raw/itrdb_mt172.txt")
spag.plot(itrdb_mt)
ids <- read.ids(itrdb_mt, c(3,1,1))
itrdb_mt_tree <- treeMean(itrdb_mt, ids, na.rm = T)
mt_172_chron <- chron(itrdb_mt_tree, biweight = T, prewhiten = F)
as.data.frame(mt_172_chron)

mt_172_std <- mt_172_chron %>% select("std")
#write chronology
write.rwl(mt_172_std, "data_processed/mt_172")


# make dataframe to plot in ggplot
mt_172_chron$year <- as.numeric(row.names(mt_172_chron))

# Truncate the data to start from 1800 onwards
mt_172_chron <- mt_172_chron[mt_172_chron$year >= 1700, ]

# Manually specify peaks to label
peaks_to_label <- c(1812, 1819, 1828, 1838, 1840, 1844, 1855, 1862, 1875, 1901, 1902, 1915, 1929, 1930, 1976, 1985, 1989, 1994, 1997, 1999)

peaks_data <- mt_172_chron[mt_172_chron$year %in% peaks_to_label, ]
peaks_data$peak_y <- peaks_data$std

ggplot(mt_172_chron, aes(x = year, y = std)) +
  geom_line() +
  geom_text(data = peaks_data, aes(label = year, y = peak_y), 
            position = position_jitter(height = 0.03),  
            hjust = -0.08, size = 3.5, color = "blue", fontface = "bold") +
  labs(title = "ITRDB Tree Ring Chronology WBP MT172",
       x = "Year",
       y = "Ring Width") + 
  scale_x_continuous(breaks = seq(min(mt_172_chron$year), max(mt_172_chron$year), by = 5),
                     minor_breaks = seq(min(mt_172_chron$year), max(mt_172_chron$year), by = 1),
                     limits = c(min(mt_172_chron$year), max(mt_172_chron$year)),
                     expand = c(0, 0)) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  theme_bw()

# Assign new column names to the dataframe
colnames(rw_data_flathead) <- new_col_names


rw_data_flathead <- rw_data_flathead %>% 
  remove_rownames %>% 
  column_to_rownames(var="Year")

colnames(mt_rw_flathead)
#explore dataframe in dplR
rwl.report(rw_data_flathead) #descriptive statistics Mean (Std dev) series intercorrelation: 0.04579581 (0.1202018)

spag.plot(rw_data_flathead)

rw_stats_wbp <- rwl.stats(rw_data_flathead) # wow this is bad, 0.0344208 series intercorrelation
head(rw_data_flathead)

#Plot specific series - 29748810
sample_name <- "29748810"

# Create a time series plot
ggplot(rw_data_flathead, aes_string(x = "Year", y = sample_name)) +
  geom_line() +
  labs(x = "Year", y = sample_name, title = paste("Time Series of", sample_name))


# okay cut off at years 1850 onwards
rw_data_flathead <- subset(rw_data_flathead, rownames(rw_data_flathead) > 1895) 
max(rownames(rw_data_flathead)) #last year will subset climate data; 1994

spag.plot(rw_data_flathead)

colnames_rw <- colnames(rw_data_flathead)

# detrend with 30 year spline 
rwi_flathead <- detrend(rwl = rw_data_flathead, 
                        method = "ModNegExp")

rw_data_flathead

MT_29745840_rwi <- detrend.series(y = rw_data_flathead[, "29745840"],verbose=TRUE, method = "ModNegExp") #1951-1993
MT_29746320_rwi <- detrend.series(y = rw_data_flathead[, "29746320"],verbose=TRUE, method = "ModNegExp") #1518-1994
MT_29748810_rwi <- detrend.series(y = rw_data_flathead[, "29748810"],verbose=TRUE, method = "ModNegExp") #1609-1993
MT_29751610_rwi <- detrend.series(y = rw_data_flathead[, "29751610"],verbose=TRUE, method = "ModNegExp") #1954-1988
MT_29753370_rwi <- detrend.series(y = rw_data_flathead[, "29753370"],verbose=TRUE, method = "ModNegExp") #1869-1990
MT_29760310_rwi <- detrend.series(y = rw_data_flathead[, "29760310"],verbose=TRUE, method = "ModNegExp") #1935-1993
MT_29760390_rwi <- detrend.series(y = rw_data_flathead[, "29760390"],verbose=TRUE, method = "ModNegExp") #1950-1994
MT_29770160_rwi <- detrend.series(y = rw_data_flathead[, "29770160"],verbose=TRUE, method = "ModNegExp") #1562-1993
MT_29817250_rwi <- detrend.series(y = rw_data_flathead[, "29817250"],verbose=TRUE, method = "ModNegExp") #1909-1993
MT_29833020_rwi <- detrend.series(y = rw_data_flathead[, "29833020"],verbose=TRUE, method = "ModNegExp") #1611-1994
MT_29861520_rwi <- detrend.series(y = rw_data_flathead[, "29861520"],verbose=TRUE, method = "ModNegExp") #1645-1994
MT_29875630_rwi <- detrend.series(y = rw_data_flathead[, "29875630"],verbose=TRUE, method = "ModNegExp") #1599-1994
MT_29877620_rwi <- detrend.series(y = rw_data_flathead[, "29877620"],verbose=TRUE, method = "ModNegExp") #1762-1994
MT_29887690_rwi <- detrend.series(y = rw_data_flathead[, "29887690"],verbose=TRUE, method = "ModNegExp") #1782-1994
MT_29890670_rwi <- detrend.series(y = rw_data_flathead[, "29890670"],verbose=TRUE, method = "ModNegExp") #1719-1993
MT_29895550_rwi <- detrend.series(y = rw_data_flathead[, "29895550"],verbose=TRUE, method = "ModNegExp") #1947-1993

## write to text file tucson format

write.tucson(rw_data_wbp, "data_raw/rw_data_wbp_swmontana.txt", prec = 0.01, long.names = TRUE)


