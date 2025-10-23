#CHANGING PROJECTIONS===========================================================

#CHECK THE FILE-----------------------------------------------------------------
library(sf)
library(here)
st_layers(here("Workclass 03", "gadm41_AUS.gpkg"))

library(sf)
Ausoutline <- st_read(here("Workclass 03", "gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0')
print(Ausoutline)

#WGS84--------------------------------------------------------------------------
library(sf)
st_crs(Ausoutline)$proj4string

#PROJ4--------------------------------------------------------------------------
library(sf)
st_crs(Ausoutline)$proj4string

#EPSG---------------------------------------------------------------------------
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

#only use when there is no CRS
Ausoutline <- st_read(here("Workclass 03", "gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0') %>% 
  st_set_crs(4326)

#REPROJECTING YOUR SPATIAL DATA-------------------------------------------------
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

#WORLDCLIM DATA=================================================================

library(raster)
library(terra)

#LOAD THE RASTER----------------------------------------------------------------
jan<-terra::rast(here("Workclass 03", "wc2.1_5m_srad_01.tif"))

# have a look at the raster layer jan
jan

plot(jan)

#TO MOLLWEIDE PROJECTION
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)

#BACK TO WGS84
pr1 <- pr1 %>%
  terra::project(., "+proj=longlat +datum=WGS84 +no_defs +type=crs")
plot(pr1)

#DATA LOADING===================================================================

#LOOK FOR TIF FILES IN OUR FOLDER
library(fs)
dir_info("Workclass 03//wc2.1_5m_srad/")

#SELECT FILES
library(tidyverse)
listfiles<-dir_info("Workclass 03//wc2.1_5m_srad/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

#set a collection of rasters
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp

# access the january layer
worldclimtemp[[1]]

#rename layers
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month

#GET THE DATA FOR JUST JANUARY
worldclimtemp$Jan

#RASTER LOCATION================================================================

#EXTRACT DATA MAKING A DATAFRAME
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)

#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")

#Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

#Add column sites
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

#DESCRIPTIVE STATISTICS=========================================================

#DATA PREPARATION TAKING PERTH AS AN EXAMPLE
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

Perthtemp <- Aucitytemp2[3,]

#HISTOGRAM----------------------------------------------------------------------
hist(as.numeric(Perthtemp))

#MAKE IT PRETTIER

library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(7500,10000,12500,15000,17500,20000,22500,25000,27500,30000)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist_T <- hist((as.numeric(t)), 
     breaks=userbreak, 
     col="#53868B", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

#CHECK THE HISTOGRAM INFORMATION
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo

#USING MORE DATA================================================================
plot(Ausoutline$geom)

#SIMPLIFY DATA
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

print(Ausoutline)

#CHECK THE COORDINATE SYSTEM OF THE DATA
#this works nicely for rasters
crs(worldclimtemp)

Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)

exactAus<-terra::mask(Austemp, Ausoutline)
plot(exactAus)

#subset using the known location of the raster
hist(exactAus[[3]], col="#8B2323", main ="March temperature")

#HISTOGRAM WITH GGPLOT==========================================================

#MAKE IT INTO A DATAFRAME
exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)

# set up the basic histogram----------------------------------------------------
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")

# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

#PLOTTING MULTIPLE MONTHSOF TEMPERATURE DATA------------------------------------

#USE PIVOT LONGER
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

#JUST SELECT TWO MONTHS FOR THIS
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

#GET THE MEAN FOR EACH MONTH
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#FACETED HISTOGRAM--------------------------------------------------------------
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

#Plot faceted histogram
ggplot(squishdata, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

#OTHER TYPE OF PLOT-------------------------------------------------------------

library(plotly)

# split the data for plotly based on month
jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=7500, end=30000, size = 2500)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

#ANOTHER ONE--------------------------------------------------------------------

# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)

# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))







