library(sf)
library(tidyverse)
library(here)
library(raster)
library(terra)
library(plotly)
library(dplyr)
library(janitor)

#OPEN SHAPE PERU----------------------------------------------------------------

PeruOutline <- st_read("Homework 03//GEOP_Peru//gadm41_PER.gpkg", layer = 'ADM_ADM_0')
st_crs(PeruOutline, proj4string())
st_simplify(PeruOutline, dTolerance=1000)

#OPEN RASTERS-------------------------------------------------------------------

list_files<-dir_info("Homework 03//Rasters") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

Proy_W <- list_files %>%
  terra::rast()

st_crs(Proy_W)

#JUST TO LOOK ONE BY ONE
Proy_W_P1 <- terra::rast("Homework 03//Rasters//wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif")
Proy_W_P5 <- terra::rast("Homework 03//Rasters//wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif")

#OPEN WORLD POINTS--------------------------------------------------------------

WPoints <- st_read("Homework 03//World_Cities//World_Cities.shp")
st_crs(WPoints, proj4string()) #3857

PeruPoint <- WPoints %>%
  filter(., CNTRY_NAME == 'Peru') %>%
  group_by(CNTRY_NAME)

PeruPoint_4326 <- PeruPoint %>%
  st_transform(.,4326)

#RASTER FOR PERU POINTS---------------------------------------------------------
#ADD CITY_NAME AND BIND TO INCLUDE THIS

Proy_P <- Proy_W %>%
  terra::extract(PeruPoint_4326["CITY_NAME"], bind=TRUE)

#RENAME FOR BETTER LECTURE
#SOURCE: INTERGOVERNMENTAL PANEL ON CLIMATE CHANGE
#SSPS: SHARED SOCIOECONOMIC PATHWAYS 
#SSP1: LOTS OF MITIGATION AND ADAPTATION
#SSP5: FOSSIL FUEL DEVELOPMENT

months <- c("CITY_NAME",
            "SSP1_jan", "SSP1_feb", "SSP1_mar", "SSP1_apr", "SSP1_may", "SSP1_jun",
            "SSP1_jul", "SSP1_agu", "SSP1_set", "SSP1_oct", "SSP1_nov", "SSP1_dic", 
            "SSP5_jan", "SSP5_feb", "SSP5_mar", "SSP5_apr", "SSP5_may", "SSP5_jun",
            "SSP5_jul", "SSP5_agu", "SSP5_set", "SSP5_oct", "SSP5_nov", "SSP5_dic")

names(Proy_P) <- months

Proy_P2 <- Proy_P %>% 
  as_tibble()

class(Proy_P2)

#HISTOGRAM1---------------------------------------------------------------------

userbreak <- c(10,12,14,16,18,20,22,24,26,28,30,32,36,38,40,42,44)

Proy_Lima <- Proy_P2 %>%
  filter(CITY_NAME=="Lima")

p <- Proy_P2 %>%
  dplyr::select(SSP1_jan:SSP5_dic)

#UNLIST() NECESSARY TO FUNCTION

hist(as.numeric(unlist(p)),
     breaks=userbreak,
     col="#00688B",
     main="Temperature per SSPS of Lima",
     xlab="Temperature",
     ylab="Frequency")

#HISTOGRAM2---------------------------------------------------------------------

Proy_P3 <- Proy_P2 %>%
  select(c(2:25))

#USE PIVOT LONGER
squishP <- Proy_P3 %>%
  pivot_longer(
    cols = 1:24,
    names_to = "SSPS",
    values_to = "temp")

#JUST SELECT TWO MONTHS FOR THIS
twoSSPS <- squishP %>%
  # | = OR
  filter(., SSPS=="SSP1_dic" | SSPS=="SSP5_dic")

#GET THE MEAN FOR EACH MONTH
meantwoSSPS <- twoSSPS %>%
  group_by(SSPS) %>%
  summarise(mean=mean(temp, na.rm=TRUE))

#SET HISTOGRAM
ggplot(twoSSPS, aes(x=temp, color=SSPS, fill=SSPS)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwoSSPS, 
             aes(xintercept=mean, 
                 color=SSPS),
             linetype="dashed")+
  labs(title="Ggplot2 Histogram of proyectecd temperatures in Lim",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#FACETED HISTOGRAM--------------------------------------------------------------

Proy_P4 <- Proy_P3 %>%
  select(c(1:12))

squishSSP1 <- Proy_P4 %>%
  pivot_longer(
    cols = 1:12,
    names_to = "SSPS",
    values_to = "temp")

#Plot faceted histogram
ggplot(squishSSP1, aes(x=temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 Faceted Histogram of proyected temperatures in Lima", 
       x="Temperature",
       y="Frequency")+
  facet_grid(SSPS ~ .)+
  theme(plot.title = element_text(hjust = 0.5),  # tamaño del título
        axis.text = element_text(size = 4),      # números de los ejes
        axis.title = element_text(size = 10),    # títulos de ejes
        strip.text = element_text(size = 7)      # etiquetas de los paneles (facet)
  )

#TRY TO DO IT WITH THE DIFFERENCE BETWEEN THE SSPS
#TRY TO PLOT THINGS
