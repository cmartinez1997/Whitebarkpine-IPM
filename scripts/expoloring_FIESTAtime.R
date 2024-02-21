##Cecilia Martinez
## Eplorign FIESTAtime output data given to me by Renata 
## January 29 2024



# load packages -----------------------------------------------------------

library(tidyverse)



# read in data ------------------------------------------------------------


wbp_time <- read_csv("data_raw/Fiesta_whitebark_pine.csv")
wbp_three <- unique(wbp_time$duplicated(wbp_time$TREE_UNIQUE_ID) & !duplicated(wbp_time$TREE_UNIQUE_ID, fromLast = TRUE))
