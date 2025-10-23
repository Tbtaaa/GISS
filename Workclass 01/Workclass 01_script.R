library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)
#read in the shapefile

shape <- st_read(
  "C:/Users/Andy/OneDrive - University College London/Teaching/CASA0005/2020_2021/CASA0005repo/prac1_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
# read in the csv
mycsv <- read_csv("C:/Users/Andy/OneDrive - University College London/Teaching/CASA0005/2020_2021/CASA0005repo/prac1_data/fly_tipping_borough_edit.csv")  
# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
# set tmap to plot
tmap_mode("plot")
# have a look at the map
qtm(shape, fill = "2011_12")
# write to a .gpkg
shape %>%
  st_write(.,"C:/Users/Andy/OneDrive - University College London/Teaching/CASA0005/2020_2021/CASA0005repo/prac1_data/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)
# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="C:/Users/Andy/OneDrive - University College London/Teaching/CASA0005/2020_2021/CASA0005repo/prac1_data/Rwk1.gpkg")
# list what is in it
con %>%
  dbListTables()
# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)
# disconnect from it
con %>% 
  dbDisconnect()