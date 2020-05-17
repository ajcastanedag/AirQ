library(ggmap)
library(sf)

setwd("C:\\Users\\Cowboybebop\\Documents\\EAGLE\\0_Other\\Additional_Projects\\AirQ\\R")
file <- "TEST.TXT"
Data <- read.table(file, header = FALSE, sep = ",", dec = ".")
names(Data) <- c("ID","LON","LAT","ALT","TIME","DATE","SAT","SPEED")

Data <- Data[(Data$LON > 49.77 & Data$LON < 49.805),]
Data <- Data[(Data$LAT > 9.889 & Data$LAT < 9.95),]

ggplot(Data)+
  geom_point(aes(x=LON, y=LAT, color=ALT)) +
  coord_fixed(ratio = 1)

p.sf <- st_as_sf(Data, coords = c("LAT", "LON"), crs = 4326) 

st_write(p.sf, "Points_test.gpkg", driver="GPKG")  # Create a geopackage file

map <- get_googlemap('houston', markers = df, path = df, scale = 2)

ggmap(map, extent ='device')
