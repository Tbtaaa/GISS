library(tidyverse)
library(here)
library(sf)
library(janitor)
library(tmap)
library(tmaptools)

#IMPORT SHAPEFILE
w_counties <- st_read("Homework 02//Washington_Counties_with_Natural_Shoreline___washsh_area//Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
plot(w_counties)
head(w_counties)

#IMPORT CSV
report <- read_csv("Homework 02//Report_Card_Assessment_Data_2018-19_School_Year_20251009.csv",
                           locale = locale(encoding = "latin1"),
                           na = "NULL")
head(report)

#COLOUMNS TO WORK WITH
#  TestSubject
#  Count of Students Expected to Test
#  CountMetStandar

#FILTER
report_short <- report %>%
  select(County, TestSubject, `Count of Students Expected to Test`, CountMetStandard) %>%
  clean_names(.)

report_short_sum <- report_short %>%
  group_by(county) %>%
  summarise(Total_CountMetStandar=sum(count_met_standard, na.rm = TRUE),
            Total_Students=sum(count_of_students_expected_to_test, na.rm = TRUE)) %>%
  mutate(Perct = Total_CountMetStandar * 100 / Total_Students)

#JOIN_LEFT
w_data <- w_counties %>%
  left_join(report_short_sum %>%
              select(Total_CountMetStandar, Total_Students, Perct, county),
            by = c("COUNTYLABE"="county"))

#WASHINGTON AVERAGE
w_average <- w_data %>%
  summarise(students_w = sum(Total_Students, na.rm = TRUE),
            count_met_w = sum(Total_CountMetStandar, na.rm = TRUE),
            Perct_w = 100 * count_met_w / students_w)

w_data_ <- w_data %>%
  mutate(w_total = case_when(
    Perct>51.58 ~ "above Wahington average",
    TRUE ~ "below Washington average"))

#MAKE PLOT
tmap_mode("plot")

tmapw <- w_data_ %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tm_shape(tmapw)+
  tm_rgb()+
  tm_shape(w_data_) + 
  tm_polygons(fill="w_total",
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "County averages", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Washinton counties", 
           size = 2,
           position = c("center", "top"))
