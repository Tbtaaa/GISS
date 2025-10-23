install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), 
                     repos = "https://www.stats.bris.ac.uk/R/")
Codelibrary(sf)
# change this to your file path!!!
shape <- st_read("C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Workclass 01//statistical-gis-boundaries-london//statistical-gis-boundaries-london//ESRI")
summary(shape)

#Plot the entire shape
plot(shape)

#Just plot boudaries
library(sf)
shape %>% 
  st_geometry() %>%
  plot()

#Import csv
library(tidyverse)
#this needs to be your file path again
mycsv <-  read_csv("C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Workclass 01//fly-tipping-borough_fixed_.csv")
mycsv

#Joint csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Etiquetas de fila")
#check
shape%>%
  head(., n=10)

#fix
install.packages("tmap")
library(dplyr)

#thematic map
library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2011-12.x")

#export
shape %>%
  st_write(.,"C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Workclass 01//London_Geopackage_R.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

#export csv
library(readr)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),dbname="C://Users//Tabata//Documents//CASA//CASA0005_GISS//CASA0005_01//Workclass 01//London_Geopackage_R.gpkg")
con %>%
  dbListTables()
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()

