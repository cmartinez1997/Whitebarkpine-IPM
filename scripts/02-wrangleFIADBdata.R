## This script is for wrangling the FIA csv files and making FIA dataframe
## Cecilia Martinez 
## March 15 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------
library(tidyverse) #for data manipulation

# Read data in ------------------------------------------------------------

# read in Montana data 
MT_tree <- read_csv("data_raw/MT_TREE.csv")
MT_plot <- read_csv("data_raw/MT_PLOT.csv")
MT_cond <- read_csv("data_raw/MT_COND.csv")

# read in Idaho data
ID_tree <- read_csv("data_raw/ID_TREE.csv")
ID_plot <- read_csv("data_raw/ID_PLOT.csv")
ID_cond <- read_csv("data_raw/ID_COND.csv")

# read in Wyoming data
WY_tree <- read_csv("data_raw/WY_TREE.csv")
WY_plot <- read_csv("data_raw/WY_PLOT.csv")
WY_cond <- read_csv("data_raw/WY_COND.csv")


# Wrangle data ------------------------------------------------------------

# Join tables from different states to create dataframe for interior west

# combining plot data tables from MT, ID, WY and save 
plot_MT_ID_WY <- bind_rows(MT_plot, ID_plot, WY_plot)
str(plot_MT_ID_WY)
write_csv(plot_MT_ID_WY, "data_processed/PLOT_MT-ID-WY.csv")

# combining tree data tables from MT, ID, WY and save 
tree_MT_ID_WY <- bind_rows(MT_tree, ID_tree, WY_tree)
str(tree_MT_ID_WY)
write_csv(tree_MT_ID_WY, "data_processed/TREE_MT-ID-WY.csv")

# combining condition data tables from MT, ID, WY and save 
cond_MT_ID_WY <- bind_rows(MT_cond, ID_cond, WY_cond)
str(cond_MT_ID_WY)
write_csv(cond_MT_ID_WY, "data_processed/COND_MT-ID-WY.csv")
