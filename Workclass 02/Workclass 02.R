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





