## This script gets tables from FIADB and turns them into csvs
## Cecilia Martinez modified by Ellen Bledsoe
## March 15 2023
## cecimartinez333@gmail.com


# Load Packages -----------------------------------------------------------

library(DBI) #required to access FIADB
library(tidyverse) #data processing

# Getting data from SQL ---------------------------------------------------

# Before we can get csv files, we have to download SQL db for our states of
# interest to our working directory by going to FIA datamart
# https://experience.arcgis.com/experience/3641cea45d614ab88791aef54f3a1849/

# GET IDAHO DATA ----------------------------------------------------------
# Set path to where you want the CSVs to appear
path_out_ID <- "data_raw/"

# reference the database and checking the connection and table names
con_ID <-  dbConnect(RSQLite::SQLite(), "SQLite_FIADB_ID.db")
dbGetQuery(conn = con_ID, "SELECT name FROM sqlite_master WHERE type='table';")

# subset tables that you want
db_file_names_ID <-
  c("PLOT",
    "TREE",
    "COND",
    "SUBPLOT_REGEN",
    "SUBPLOT",
    "PLOT_REGEN",
    "PLOTGEOM")

#loop through specified tables and write them at csvs
for (i in db_file_names_ID) {
  write_csv(dbReadTable(conn = con_ID, name = i),
            file = paste0(path_out_ID, "ID_", i, ".csv"))
}

# GET MONTANA DATA --------------------------------------------------------
# Set path to where you want the CSVs to appear
path_out_MT <- "data_raw/"

# reference the database and checking the connection and table names
con_MT <-  dbConnect(RSQLite::SQLite(), "SQLite_FIADB_MT.db")
dbGetQuery(conn = con_MT, "SELECT name FROM sqlite_master WHERE type='table';")

# subset tables that you want
db_file_names_MT <-
  c("PLOT",
    "TREE",
    "COND",
    "SUBPLOT_REGEN",
    "SUBPLOT",
    "PLOT_REGEN",
    "PLOTGEOM")

# loop through specified tables and write them at csvs
for (i in db_file_names_MT) {
  write_csv(dbReadTable(conn = con_MT, name = i),
            file = paste0(path_out_MT, "MT_", i, ".csv"))
}

# GET WYOMING DATA --------------------------------------------------------
# Set path to where you want the CSVs to appear
path_out_WY <- "data_raw/"

# reference the database and checking the connection and table names
con_WY <-  dbConnect(RSQLite::SQLite(), "SQLite_FIADB_WY.db")
dbGetQuery(conn = con_WY, "SELECT name FROM sqlite_master WHERE type='table';")

# subset tables that you want
db_file_names_WY <-
  c("PLOT",
    "TREE",
    "COND",
    "SUBPLOT_REGEN",
    "SUBPLOT",
    "PLOT_REGEN",
    "PLOTGEOM")

# loop through specified tables and write them at csvs
for (i in db_file_names_WY) {
  write_csv(dbReadTable(conn = con_WY, name = i),
            file = paste0(path_out_WY, "WY_", i, ".csv"))
}

# close connection
dbDisconnect(con_ID, con_MT, con_WY)
