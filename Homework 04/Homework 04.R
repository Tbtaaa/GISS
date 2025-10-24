library(sf)
library(janitor)
library(tidyverse)

#READ FILES---------------------------------------------------------------------

w_countries <- st_read("Homework 04//World_Countries//World_Countries_Generalized.shp")
plot(w_countries)
head(w_countries)
clean_names(w_countries)

data_complete <- read_csv("Homework 04//HDR25_Composite_indices_complete_time_series.csv",
                          na = "NA")
head(data_complete)
clean_names(data_complete)
