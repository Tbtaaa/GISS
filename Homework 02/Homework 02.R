library(sf)
library(tidyverse)

#IMPORT SHAPEFILE
washington_counties <- st_read("Homework 02//Washington_Counties_with_Natural_Shoreline___washsh_area//Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
plot(washington_counties)

#IMPORT CSV
assesment_data <- read_csv("Homework 02//Report_Card_Assessment_Data_2018-19_School_Year_20251009.csv",
                           locale = locale(encoding = "latin1"),
                           na = "NULL")

#TRY TO USE FILTER--------------------------------------------------------------

#SELECT COLUMNS OF ASSESMENT DATA(3,20,22)
assesment_data_short <- assesment_data[,c(3,20,22)]

#CLEAN NAMES
library(janitor)
assesment_data_short <- assesment_data_short %>%
  clean_names()

summary(assesment_data_short)

#SUMMARIZE BY COUNTY
library(dplyr)

data_pcounty <- assesment_data_short %>%
  group_by(county) %>%
  summarise(
    students = sum(count_of_students_expected_to_test, na.rm = TRUE),
    count_met_standar = sum(count_met_standard,na.rm = TRUE),
    .groups = "drop"
  )

#PERCENTAGE OF STUDENTS PER COUNTY 
data_pcounty <- data_pcounty %>%
  mutate(Perct = count_met_standar * 100 / students)

#CAPITALS FOR COUNTIES BEFORE JOIN
library(dplyr)
library(stringr)

data_pcounty <- data_pcounty %>%
  mutate(county = str_to_upper((county)))

#FINALLY, JOIN LEFT
wcounties_data <- washington_counties %>%
  left_join(
    data_pcounty %>%
      select(county, students, count_met_standar, Perct),
    by = c("COUNTY" = "county")
      )

#PLOT MAP
library(tmap)
library(tmaptools)

qtm(wcounties_data,
    fill = "Perct")

#ALL WASHINGTON
wtotal <- wcounties_data %>%
  summarise(
    students_total = sum(students, na.rm = TRUE),
    count_met_total      = sum(count_met_standar, na.rm = TRUE),
    pct_weighted   = 100 * count_met_total / students_total
  )

#ARE THEY ABOVE O BELOW AVERAGE?
wcounties_data <- wcounties_data %>%
  mutate(Wcompare = case_when(Perct > 51.58 ~ "above Washington average",
                               TRUE ~ "below Washington average"))

#PLOT MAP
qtm(wcounties_data,
    fill = "Wcompare")

#TRY TO ADD OPEN STREET MAP-----------------------------------------------------
