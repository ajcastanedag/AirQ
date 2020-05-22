# List libraries
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","leaflet","ggplot2","gganimate",
              "ggmap","pals","ggdark","reshape2","RColorBrewer","plyr", "hms","stringi")

# import functions file 
source("Functions.R")

# Load libraries
ipak(packages)

# Load data
LoadData()

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






###########################################################################################
#################################### GUI PLOTS  ###########################################
###########################################################################################
# ALTITUDE
Graph_Y_Min <- round_any(min(Data$ALT_B),10)
Graph_Y_Max <- round_any(max(Data$ALT_B),5, f = ceiling)
Graph_X_Min <- 0
Graph_X_Max <- round_any(max(Data$ID),50, f = ceiling)

Alt_Area <- ggplot(Data, aes(x=ID,y=ALT_B)) +
  geom_ribbon(aes(xmin=Graph_X_Min, xmax=Graph_X_Max,
                  ymin=Graph_Y_Min, ymax=pmax(ALT_B)),
              fill="white", alpha=0.2) +
  geom_line() +
  coord_cartesian(xlim = c(min(Data$ID),Graph_X_Max),
                  ylim = c(Graph_Y_Min,Graph_Y_Max),
                  expand = FALSE) +
  dark_theme_gray() +
  scale_x_continuous(breaks = seq(0, max(Data$ID), by = 50)) +
  scale_y_continuous(breaks = seq(Graph_Y_Min, Graph_Y_Max, by=1)) +
  theme(plot.margin = unit(c(0.5,2,0.5,0.5), "cm")) +
  geom_rect(xmin = max(Data$ID), xmax = Graph_X_Max, ymin = Graph_Y_Min, ymax = Graph_Y_Max,   fill = "black") +
  labs(title = "Trip altitude variations",
       x = "Point ID",
       y = "Recorded Atitude") 

Alt_Area

Alt_anim <- Alt_Area +
  geom_point() +
  geom_segment(aes(xend = length(ID), yend = ALT_B), linetype = 2, colour = 'grey') + 
  geom_text(aes(x = max(ID), label = ALT_B), hjust = -0.1) +           
  transition_reveal(ID)

animate(Alt_anim, height= 400, width= 600)
anim_save("Alture.gif")
######################## 
# Temperature
data <- data.frame(Specie = rep("out",100), Val = seq(1,100))

# Stacked
ggplot(data, aes(fill=Val, y=1, x=Specie)) + 
  geom_bar(position="stack", stat="identity", width = 0.1) + 
  scale_fill_distiller(palette= "RdYlBu") +
  dark_theme_gray() +
  coord_polar(theta = "y", start=-pi/2) +
  ylim(c(0,max(data$Val)*2)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



