library(ggplot2)
library(gganimate)
library(ggmap)
library(sf)
require(pals)
library(ggdark)

getwd()
setwd("Data")
file <- "TEST.TXT"
Data <- read.table(file, header = FALSE, sep = ",", dec = ".")
names(Data) <- c("ID","LON","LAT","ALT","TIME","DATE","SAT","SPEED")

Data <- Data[(Data$LON > 49.77 & Data$LON < 49.805),]
Data <- Data[(Data$LAT > 9.889 & Data$LAT < 9.95),]

base <- ggplot(Data, aes(x=LAT, y=LON, color=SPEED))+
  geom_point() +
  geom_path(lineend = "round") +
  coord_fixed(ratio = 1) +
  scale_color_gradientn(colours = jet(100)) +
  dark_theme_gray() +
  labs(title = "GPS TEST",
       subtitle = "Date: 17/05/20",
       x = "Longitude",
       y = "Latitude",
       color = "Speed m/s")+
  transition_reveal(along = TIME)

animate(base, fps = 10, width = 694, height = 403)

anim_save("Test.gif")


p.sf <- st_as_sf(Data, coords = c("LAT", "LON"), crs = 4326) 
st_write(p.sf, "Points_test.gpkg", driver="GPKG")  # Create a geopackage file


#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html