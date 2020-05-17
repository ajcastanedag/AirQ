######################## LOAD LIBRARIES 
library(ggplot2)
library(gganimate)
library(ggmap)
library(sf)
require(pals)
library(ggdark)
require(reshape2)
library(RColorBrewer)

######################## PREPARE THE DATA 
getwd()
setwd("Data")
file <- "TEST.TXT"
Data <- read.table(file, header = FALSE, sep = ",", dec = ".")
names(Data) <- c("ID","LAT","LON","ALT","TIME","DATE","SAT","SPEED")

######################## STORE AND DELETE OUTLINERS 
LongOut <- boxplot(Data$LON)$out
LatgOut <- boxplot(Data$LAT)$out

Data <- Data[!Data$LON %in% LongOut,]
Data <- Data[!Data$LAT %in% LatgOut,]

Data$ID <- seq(1:length(Data$ID))
######################## PLOT DATA
Plot <- ggplot(Data, aes(x=LON, y=LAT, color=SPEED))+
  geom_point() +
  geom_path(lineend = "round") +
  coord_fixed(ratio = 1) +
  scale_color_gradientn(colours = jet(100)) +
  dark_theme_gray() +
  labs(title = "GPS TEST",
       subtitle = "Date: 17/05/20",
       x = "Longitude",
       y = "Latitude",
       color = "Speed m/s")

######################## MAKE GIF
Gif <- Plot + transition_reveal(along = ID)
animate(base, fps = 10, width = 694, height = 403)
anim_save("Test.gif")

######################## PLOT DENSITY TO CHECK SLOW POINTS
ggplot(Data, aes(x=LON, y=LAT) ) +
  geom_bin2d(bins = 200) +
  dark_theme_gray() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "GPS DENSITY",
       subtitle = "Date: 17/05/20",
       x = "Longitude",
       y = "Latitude",
       color = "Bins")

######################## EXPORT GEOPACKAGE
p.sf <- st_as_sf(Data, coords = c("LAT", "LON"), crs = 4326) 
st_write(p.sf, "Points_test.gpkg", driver="GPKG")  # Create a geopackage file


#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html