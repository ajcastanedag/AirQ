library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)

########################################################################################
########################################################################################
ui <- bootstrapPage(theme = shinytheme("slate"),
  tags$style(type = "text/css", "html, body {width:91%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("dataset", "Dataset",
                            Av_Data
                ),
                selectInput("variable", "Variable",
                            colnames(Data)
                ),
                tags$style(".irs-line {background: #282828ff} .irs-bar {background: #8cf9b1ff}"),
                sliderInput("range", "Range", min(Data$ID), max(Data$ID),
                            value = range(Data$ID), step = 1
                ),
                selectInput("colors", "Color Pallete",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                #checkboxInput("legend", "Show Legend", TRUE)
                prettyCheckbox( inputId = "legend", label = "Show Legend", icon = icon("check"))
  )
)
########################################################################################
########################################################################################
server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    Data[Data$SPEED >= input$range[1] & Data$SPEED <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, Data$SPEED)
  })
  
  output$map <- renderLeaflet({                             
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(Data) %>% addTiles() %>%                                          
      fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~1*SPEED, weight = 1, color = "#777777",
                 fillColor = ~pal(SPEED),
                 fillOpacity = 1,
                 popup = ~paste(SPEED)
      )
  })
  
  # Use a separate observer 
  observe({
    proxy <- leafletProxy("map", data = Data)
    
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~SPEED
      )
    }
  })
}
########################################################################################
########################################################################################

shinyApp(ui, server)

