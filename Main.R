# List libraries
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","leaflet","ggplot2","gganimate",
              "ggmap","pals","ggdark","reshape2","RColorBrewer","plyr", "hms","stringi",
              "shiny","shinyWidgets","shinythemes")

# import functions file 
source("Functions.R")

# Load libraries
ipak(packages)

# Load data
LoadData(4)

###########################################################################################
#################################### GUI PLOTS  ###########################################
###########################################################################################
###########################################################################################
#Difine some constant values for the Alture plot
Graph_Y_Min <- round_any(min(Data$ALT_B),10)
Graph_Y_Max <- round_any(max(Data$ALT_B),5, f = ceiling)
Graph_X_Min <- 0
Graph_X_Max <- round_any(max(Data$ID),50, f = ceiling)

Base_Alture_Plot <- ggplot(Data, aes(x=ID,y=ALT_B)) +
  geom_ribbon(aes(xmin=Graph_X_Min, xmax=Graph_X_Max,
                  ymin=Graph_Y_Min, ymax=pmax(ALT_B)),
              fill="#606437ff", alpha=0.2) +
  geom_line(color="#a8b063ff", alpha=0.6) +
  coord_cartesian(xlim = c(min(Data$ID),Graph_X_Max),
                  ylim = c(Graph_Y_Min,Graph_Y_Max),
                  expand = FALSE) +
  scale_x_continuous(breaks = seq(0, max(Data$ID), by = 50)) +
  scale_y_continuous(breaks = seq(Graph_Y_Min, Graph_Y_Max, by=1)) +
  geom_rect(xmin = max(Data$ID), xmax = Graph_X_Max, ymin = Graph_Y_Min, ymax = Graph_Y_Max,   fill = "#282d32ff") +
  geom_segment(aes(x=ID[ALT_B == max(ALT_B)],y= max(ALT_B), xend = length(Data$ID), yend = max(ALT_B)), linetype = 2, colour = 'grey') + 
  geom_text(aes(x = max(Data$ID), y = ALT_B[ALT_B == max(ALT_B)] , label = max(ALT_B)), hjust = -0.1) +
  dark_theme_classic() +
  theme(plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
        plot.background = element_rect(fill = "#282d32ff"),
        panel.background = element_rect(fill = "#282d32ff")) +
  labs(title = "Altitude variations",
      subtitle = "",
       x = "Point ID",
       y = "Recorded Atitude")


Base_Alture_Plot
###########################################################################################
Max_Y_Polar <- 1.168
Data$SPEED_p <- Data$SPEED * Max_Y_Polar / max(Data$SPEED)

Velocity_Plot <- ggplot(Data, aes(ymax = 2, ymin = 0, xmax = 2, xmin = 1, group = ID)) +
  # Assign theme
  dark_theme_classic() +
  # Add background semicircle
  annotate("rect", xmin=1, xmax=1.1, ymin=0, ymax=Max_Y_Polar, alpha=0.5, fill="#52524fff") +
  # Add max vel semicircle
  annotate("rect", xmin=1, xmax=1.1, ymin=1, ymax=Max_Y_Polar, alpha=0.8, fill="#5b1d1cff") +
  # Add data semicircle
  annotate("rect", xmin=1, xmax=1.1, ymin=0, ymax=Data$SPEED_p[100], alpha=0.9, fill="#c8c8c8ff") +
  # Polar Y coordinate starting from -pi/2
  coord_polar(theta = "y", start=-7*pi/12) + xlim(c(0, 2)) + ylim(c(0,2)) +
  annotate(geom="text", x=0, y=1, label=Data$SPEED[100], color="#c8c8c8ff", size = 8) +
  # Put text indicators
  annotate(geom="text", x=0.6, y=0.59, label="SPEED", color="#c8c8c8ff", size = 6, alpha=0.8) +
  # Put max min indicators
  annotate(geom="text", x=1.3, y=0, label="0", color="#c8c8c8ff", size = 5, alpha=0.8) +
  annotate(geom="text", x=1.35, y=Max_Y_Polar, label=round(max(Data$SPEED)), color="#c8c8c8ff", size = 5, alpha=0.8) +
  # Polar Y coordinate starting from -pi/2
  theme(line = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Speed", subtitle = "") 

Velocity_Plot

#Gif <- Velocity_Plot + transition_states(Data$ID) +
#  ease_aes('linear') +
#  enter_fade() +
#  exit_fade()

animate(Gif, fps = 10, width = 1200, height = 1200)

###########################################################################################








###########################################################################################
####################################### IDEAS  ############################################
###########################################################################################

# use and mod the https://movevis.org/ package to plot raster characteristics from the GPS TRACKS

# transform Fusion2020 into package to aquire S1&S2$S5 images from the GPS TRACK automatically 

#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html

# ######################## PLOT DATA
# Plot <- ggplot(Data, aes(x=LON, y=LAT, color=ALT_B))+
#   geom_point() +
#   geom_path(lineend = "round") +
#   coord_fixed(ratio = 1) +
#   scale_color_gradientn(colours = jet(100)) +
#   dark_theme_gray() +
#   labs(title = "GPS TEST",
#        subtitle = paste0("Date:", Data$DATE[1]),
#        x = "Longitude",
#        y = "Latitude",
#        color = "Height m")
# 
# Plot
# ######################## MAKE GIF
# Gif <- Plot + transition_reveal(along = ID)
# animate(Gif, fps = 10, width = 694, height = 403)
# anim_save("Test.gif")
# 
# ######################## PLOT DENSITY TO CHECK SLOW POINTS
# ggplot(Data, aes(x=LON, y=LAT) ) +
#   geom_bin2d(bins = 200) +
#   dark_theme_gray() +
#   scale_fill_continuous(type = "viridis") +
#   labs(title = "GPS DENSITY",
#        subtitle = "Date: 17/05/20",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Bins")
# 
# ######################## PLOT DENSITY 2 TO CHECK SLOW POINTS
# ggplot(Data, aes(x=LON, y=LAT) ) +
#   stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#   scale_fill_distiller(palette= "RdYlBu") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   dark_theme_gray() +
#   geom_path(lineend = "round", color="Black") +
#   labs(title = "GPS DENSITY",
#        subtitle = "Date: 17/05/20",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Bins") 
# 
# 
# ############################### map it ###########################################
# leaflet(Data) %>% addTiles() %>% addPolylines(~LON, ~LAT)
# ######################## EXPORT GEOPACKAGE##########################
# p.sf <- st_as_sf(Data, coords = c("LON", "LAT"), crs = 4326) 
# st_write(p.sf, "Points_test.gpkg", driver="GPKG")  # Create a geopackage file

