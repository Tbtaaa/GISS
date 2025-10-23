#CREATE SOME DATASET, FIRST A VECTOR OF 1-100 AND THEN 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)

#PLOT THE DATA
plot(Data1, Data2, col="red")

#ANOTHER ONE JUST FOR FUN JEJE
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)

#NOW PLOT THIS ONE
plot(Data3, Data4, col="blue")

#CREAT A NEW DATA FRAM
df <- data.frame(Data1, Data2)

#NOW PLOT IT
plot(df, col="green")

#WHAT
library(tidyverse)
#SHOW THE FIRST TEN ROWS AND THEN THE LAST TEN IN DF
df %>%
  head()
df %>%
  tail()

#ELEMENTS OF A DATA FRAME, FIRS ROW, THEN COLUMN
df[1:10, 1]
df[1:15,]
df[c(2,3,6), 2]
df[,1]

#RENAME COLUMNS
library(dplyr)
df <- df %>%
  dplyr::rename(column1=Data1, column2=Data2)

#SELECT COLUMNS, THREE WAYS OF DOING THIS
df %>%
  dplyr::select(column1)

df$column1

df[["column1"]]

#--------------------------FILE FROM DIRECTORY----------------------------------

#IMPORT FILES
LondonDataOSK <- read.csv("LondonData.csv",
                          header = TRUE,
                          sep = ",",
                          fileEncoding = "Windows-1252",
                          check.names = FALSE)

#INSTALL PACKAGES HERE
install.packages("here")
library(here)

here::here()

#-----------------------------FILE FROM WEB-------------------------------------

#READING A CSV FILE FROM WEB WITH READ_CSV
LondonData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

#EXAMINING THIS NEW DATA
#LONDONDATA IS A TIBLE
#LONDONDATAOSK IS A DATAFRAME
class(LondonData)
class(LondonDataOSK)

Datatypelist <- LondonData %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")
Datatypelist

summary(df)

LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()

#-----------------------SELECT JUST SOME ROWS_NUMERIC---------------------------

LondonBoroughs<-LondonData[626:658,]
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)

#SELECT JUST SOME ROWS_STRING
install.packages("stringr")
library(stringr)

LondonBoroughs <- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

LondonBoroughs$`Ward name`

LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

#HIDE REPITITIVE ROWS
LondonBoroughs <- LondonBoroughs %>%
  distinct()

#------------------SELECT JUST SOME COLUMNS: 1, 19, 20 AND 21-------------------

LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]

LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name"))

#RENAME COLUMNS
install.packages("janitor")

library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()

#-----------------------------MORE DPLYR VERBS----------------------------------

Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#top of data
slice_head(Life_expectancy, n=5)

#bottom of data
slice_tail(Life_expectancy,n=5)

#COMPARE TO THE AVERAGE LIFE EXPECTANCY IN UK
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

#WHAT IS THE RANGE OF THIS VALUES?
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

#ANOTHER WAY...
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3

Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))

#-------------------------------NOW LETS PLOT-----------------------------------

plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

#MAKE IT PRETTY
install.packages("plotly")

library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

#===========================YES, ANOTHER SECTION================================

install.packages(c("tmap"))

# might also need these ones
install.packages(c("tmaptools", "sf"))

#Load Packages (ignore any error messages about being built under a 
#different R version):
library(tmap)
library(tmaptools)
library(sf)

#INSERT SHAPEFILE
EW <- st_read(here::here("London_Boundaries",
                         "LAD_Dec_2015_FCB_GB.shp"))

#JUST SELECT LONDON
LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

#PLOT IT
qtm(LondonMap)

#CLEAN NAMES WITH JANITOR
LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)

#OTHER WAY, FALSE
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))

#CREATE COLORED MAP
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

#DONWLOAD OPEN STREET MAP
install.packages("OpenStreetMap")
library(OpenStreetMap)

#SET THE DATA FIRST, THEN PLOT MAP----------------------------------------------

tmaplondon <- BoroughDataMap %>%
  # st_bbox gives the bounding x and y coordinates 
  st_bbox(.) %>% 
  #note type="osm" gives error atm - issue raised on github: https://github.com/r-tmap/tmaptools/issues/41
  tmaptools::read_osm(., type = "esri", zoom = NULL)

#PLOT MAP
tmap_mode("plot")

tm_shape(tmaplondon)+
  # add basemap as Red Green Blue raster
  tm_rgb()+
  # add sf data
  tm_shape(BoroughDataMap) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "rate of claimants 2015", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

#MORE OPTIONS
BoroughDataMap84 <- BoroughDataMap %>%
  st_transform(.,4326)

tmap_mode("view")

#AND MOOOORE OPTIONS
tm_shape(BoroughDataMap84) + 
  # add polygon layer
  tm_polygons(fill="rate_of_job_seekers_allowance_jsa_claimants_2015",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Job seekers' Allowance Claimants", 
                                      size = 0.8))+
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Job seekers' Allowance Claimants", 
           size = 2,
           position = c("center", "top"))

#MERGE LIFE EXPECTANCY WITH EW--------------------------------------------------

#TRY TO CHANGE INNER JOIN WITH LEFT JOIN
Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "new_code"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

#NOW PLOT
tmap_mode("plot")

tm_shape(tmaplondon)+
  # add basemap as Red Green Blue raster
  tm_rgb()+
  # add sf data
  tm_shape(Life_expectancy4map) + 
  # add polygon layer
  tm_polygons(fill="UKdiff",
              fill.scale= tm_scale_intervals(values="brewer.bu_pu",
                                             style="jenks"),
              fill_alpha=0.5,
              fill.legend = tm_legend(title = "Number of years", 
                                      size = 0.8))+
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Difference in life expectancy", 
           size = 2,
           position = c("center", "top"))



