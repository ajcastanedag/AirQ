# List libraries
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","leaflet","ggplot2","gganimate",
              "ggmap","pals","ggdark","reshape2","RColorBrewer")

# import functions file 
source("Functions.R")

# Load libraries
ipak(packages)

# Change path
setwd(paste0(getwd(),"/Data"))

# Check datalog files
list.files(getwd())

# Import file            <- this has to be updated to read all files inside the "LOG" folder
file <- "TEST.TXT"

# Create dataframe
Data <- read.table(file, header = FALSE, sep = ",", dec = ".")
names(Data) <- c("ID","LAT","LON","ALT","TIME","DATE","SAT","SPEED","Temperature","ALT_B","HUM","PPM")

######################## DELETE LAT LON OUTLINERS 
LongOut <- boxplot(Data$LON)$out
LatgOut <- boxplot(Data$LAT)$out
Data <- Data[!Data$LON %in% LongOut,]
Data <- Data[!Data$LAT %in% LatgOut,]

######################## DELETE LESS THAN 3 SAT data 
#Data <- Data[Data$SAT > 3,]

######################## RESET ID
Data$ID <- seq(1:length(Data$ID))

######################## Transform Date to %d%m%y 
Data <- transform(Data, DATE = as.Date(as.character(DATE), "%d%m%y"))

######################## Transform Time to %h%m%s 


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
animate(Gif, fps = 10, width = 694, height = 403)
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

######################## PLOT DENSITY 2 TO CHECK SLOW POINTS
ggplot(Data, aes(x=LON, y=LAT) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "RdYlBu") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  dark_theme_gray() +
  geom_path(lineend = "round", color="Black") +
  labs(title = "GPS DENSITY",
       subtitle = "Date: 17/05/20",
       x = "Longitude",
       y = "Latitude",
       color = "Bins") 


############################### map it ###########################################

leaflet(Data) %>% addTiles() %>% addPolylines(~LON, ~LAT)

######################## EXPORT GEOPACKAGE##########################
p.sf <- st_as_sf(Data, coords = c("LON", "LAT"), crs = 4326) 
st_write(p.sf, "Points_test.gpkg", driver="GPKG")  # Create a geopackage file


###########################################################################################
####################################### IDEAS  ############################################
###########################################################################################

# use and mod the https://movevis.org/ package to plot raster characteristics from the GPS TRACKS

# transform Fusion2020 into package to aquire S1&S2$S5 images from the GPS TRACK automatically 







#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html