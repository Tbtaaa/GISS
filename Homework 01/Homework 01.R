library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

#read in the shapefile
shape <- st_read(
  "C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Homework 01//statsnz-territorial-authority-2018-generalised-SHP//territorial-authority-2018-generalised.shp")

# read in the csv
mycsv <- read_csv("C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Homework 01//Statistical Area 1 dataset for Census 2018-total-New Zealand_updated_4-11-21//2018_Paid_employees.csv")  

# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="Territorial authority code (2018 areas)")

# set tmap to plot
tmap_mode("plot")

# have a look at the map
qtm(shape, fill = "Paid employee")

# write to a .gpkg
shape %>%
  st_write(.,"C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Homework 01//NZ.gpkg",
           "NZ_2018_paid employee",
           delete_layer=TRUE)

# connect to the .gpkg
con <- dbConnect(SQLite(),dbname="C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Homework 01//NZ.gpkg")

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

