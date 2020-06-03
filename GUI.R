##################################################################################################
# Deine variables that will be used as static in the GUI

#For alture Plot
Graph_Y_Min <- round_any(min(Data$ALT_B),10)
Graph_Y_Max <- round_any(max(Data$ALT_B),5, f = ceiling)
Graph_X_Min <- 0
Graph_X_Max <- round_any(max(Data$ID),50, f = ceiling)

Base_Alture_Plot <- ggplot(Data[114:150,], aes(x=ID,y=ALT_B)) +
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
  #geom_segment(aes(xend = length(Data$ID), yend = ALT_B[1]), linetype = 2, colour = 'grey') + 
  geom_segment(aes(x=ID[ALT_B == max(ALT_B)],y= max(ALT_B), xend = length(Data$ID), yend = max(ALT_B)), linetype = 2, colour = 'grey') + 
  #geom_text(aes(x = max(Data$ID), y= ALT_B[ID == max(ID)] , label = ALT_B[ID == max(ID)]), hjust = -0.1) +
  geom_text(aes(x = max(Data$ID), y = ALT_B[ALT_B == max(ALT_B)] , label = max(ALT_B)), hjust = -0.1) +
  dark_theme_classic() +
  theme(plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
        plot.background = element_rect(fill = "#282d32ff"),
        panel.background = element_rect(fill = "#282d32ff")) +
  labs(title = "Trip altitude variations",
       x = "Point ID",
       y = "Recorded Atitude")


Base_Alture_Plot
##################################################################################################
ui <- fluidPage( theme = shinytheme("slate"),
  chooseSliderSkin("Flat"),
  titlePanel(title=h2("AirQ Bike LOG", align="center")),
  sidebarPanel( 
    style = "background-color: #282d32ff;",
    setSliderColor("DimGray ",1),
    sliderInput("num", "Ride time:",
                min = min(Data$ID),
                max = max((Data$ID)),
                step=1,
                value=c(min(Data$ID),
                        max(Data$ID)))),
  mainPanel(plotOutput("Alture", width = "100%")))

##################################################################################################
server <- function(input,output){
  
  #################### locate data in interval (modify Dataframe)
  dat <- reactive({
    #test <- Data[Data$ID %in% seq(from=min(input$num),to=max(input$num)),]
    Data2 <<- Data[min(input$num):max(input$num),]
  })
  
  #################### Alture data
  output$Alture<-renderPlot({
    ggplot(dat(), aes(x=ID,y=ALT_B)) +
      geom_ribbon(aes(xmin=Graph_X_Min, xmax=Graph_X_Max,
                      ymin=Graph_Y_Min, ymax=pmax(ALT_B)),
                  fill="#606437ff", alpha=0.2) +
      geom_line(color="#a8b063ff", alpha=0.6) +
      coord_cartesian(xlim = c(min(Data$ID),Graph_X_Max),
                      ylim = c(Graph_Y_Min,Graph_Y_Max),
                      expand = FALSE) +
      dark_theme_classic() +
      scale_x_continuous(breaks = seq(0, max(Data$ID), by = 50)) +
      scale_y_continuous(breaks = seq(Graph_Y_Min, Graph_Y_Max, by=1)) +
      theme(plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
            plot.background = element_rect(fill = "#282d32ff"),
            panel.background = element_rect(fill = "#282d32ff")) +
      geom_rect(xmin = max(Data$ID), xmax = Graph_X_Max, ymin = Graph_Y_Min, ymax = Graph_Y_Max,   fill = "#282d32ff") +
      #geom_segment(aes(xend = length(Data$ID), yend = ALT_B[1]), linetype = 2, colour = 'grey') + 
      geom_segment(aes(x=ID[ALT_B == max(ALT_B)],y= max(ALT_B), xend = length(Data$ID), yend = max(ALT_B)), linetype = 2, colour = 'grey') + 
      #geom_text(aes(x = max(Data$ID), y= ALT_B[ID == max(ID)] , label = ALT_B[ID == max(ID)]), hjust = -0.1) +
      geom_text(aes(x = max(Data$ID), y = ALT_B[ALT_B == max(ALT_B)] , label = max(ALT_B)), hjust = -0.1) +
      labs(title = "Trip altitude variations",
           x = "Point ID",
           y = "Recorded Atitude") 
  }, height = 500, width = 900)
}

# 114 TO 124 ERROR <- FIX
##################################################################################################
shinyApp(ui, server)
