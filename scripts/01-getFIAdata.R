## This script gets tables from FIADB and turns them into csvs
## Cecilia Martinez modified by Ellen Bledsoe
## 2023-03-15
## cecimartinez333@gmail.com


# Load Packages -----------------------------------------------------------

library(DBI)#required to access FIADB
library(tidyverse) 

# Getting data from SQL ---------------------------------------------------

# Before we can get csv files, we have to download SQL db for our states of 
# interest to our working directory by going to FIA datamart
# https://experience.arcgis.com/experience/3641cea45d614ab88791aef54f3a1849/

# get data for Idaho

# Set path to where you want the CSVs to appear
path_out_ID <- "data_raw/"

# reference the database and checking the connection and table names
con_ID <-  dbConnect(RSQLite::SQLite(), "SQLite_FIADB_ID.db")
dbGetQuery(conn = con_ID, "SELECT name FROM sqlite_master WHERE type='table';")

# subset tables that you want
db_table_names <-
  c("PLOT",
    "TREE",
    "COND",
    "SUBPLOT_REGEN",
    "SUBPLOT",
    "PLOT_REGEN",
    "PLOTGEOM")

#loop through and write them at csvs for each specified table
for (i in db_table_names) {
  write.csv(dbReadTable(conn = con, name = i),
            file = paste0(path_out, "ID_", i, ".csv"))
}

#close connection
dbDisconnect(con)
