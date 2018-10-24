library(dplyr)
library(ggmap)
library(ggplot2)
library(tmap)
library(rgdal)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(maps)
library(mapproj)
library(shinydashboard)
library(lubridate)
library(tidyr)
library(htmltools)
library(leaflet.extras)
library(shinyWidgets)


colorList = c("red", "blue", "green", "black", "purple")

data = read.csv("zipcodes.csv", stringsAsFactors = FALSE)

census = read.csv("census data.csv", stringsAsFactors = FALSE)

table = read.csv("parking data.csv", stringsAsFactors = FALSE)

table$zip = as.character(table$zip)

map = table

names = read.csv("businesses2.csv", stringsAsFactors = FALSE)

names$zip = as.character(names$zip)


map = full_join(map, names, by ="zip")

#################################
ui = fluidPage( theme = shinytheme("simplex"),
                titlePanel("SmartCity Parking"),
                
                # Create a new Row in the UI for selectInputs
                
                fluidRow(
                  
                  
                  
                  
                  
                  column(4,
                         selectInput("Name",
                                     "Destination",
                                     
                                     c("Where are you traveling?",unique(as.character(map$names),
                                                    selected = "Where are you traveling?")),
                                     
                                     checkboxInput("checkbox", label = "Credit", value = TRUE)
                         
                         )), 
                         
                  
                  column(4, offset=-1, sliderInput("slider1", label = "Cost Range", min = 0, 
                                        max = max(table$cost), value = 10)
                         
                  ),
                  
                  column(4, checkboxInput("checkbox", label = "Accepts Credit", value = TRUE)
                                     
                         ), 
                 
                  
                  
                  
                  # Create a new row for the map.
                  fluidPage( 
                    leafletOutput("mymap", height=800)
                    
                    
                  )
                )
)





# Server logic ----

server <- function(input, output, session) {
  
  observe({
    
    
    ### 
    
    table = table %>% filter(cost <= input$slider1)
    
    if (input$checkbox == "Accepts Credit") {
      table = table %>% filter(credit == "Yes" | credit == "yes")
    }
    
    
    
      output$mymap = renderLeaflet({
        
        leaflet(map) %>% setView(-118.24, 33.97, zoom = 11) %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
          addCircles(lng = ~map$long, lat = ~map$lat, weight = 10,
                     radius = 20, opacity =  .6,
                     color="green", 
                     highlightOptions = highlightOptions(
                       color='yellow', opacity = .6, weight = 5.2, fillOpacity = .5,
                       bringToFront = TRUE, sendToBack = TRUE),
                     label=paste(map$vacancy),
                     labelOptions= labelOptions(direction = 'auto')   ) 
        
        
       
        
        
        if (input$Name != "Where are you traveling?") {
          
        
          
          map = map[map$names == input$Name,]
   
   
        
          
       
          
        
             
        
        
        leaflet(map) %>% setView(map$long, map$lat, zoom = 12) %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addCircles(lng = ~map$long, lat = ~map$lat, weight = 10,
                     radius = 30, opacity =  .6,
                     color="green", 
                     highlightOptions = highlightOptions(
                       color='yellow', opacity = .6, weight = 5.2, fillOpacity = .5,
                       bringToFront = TRUE, sendToBack = TRUE),
                     label=paste(map$vacancy),
                     labelOptions= labelOptions(direction = 'auto')   ) %>%
          addCircles(lng = ~table$long, lat = ~table$lat, weight = 10,
                     radius = 20, opacity =  .6,
                     color="blue", 
                     highlightOptions = highlightOptions(
                       color='yellow', opacity = .6, weight = 5.2, fillOpacity = .5,
                       bringToFront = TRUE, sendToBack = TRUE),
                     label=paste("Vacant spots: ", table$vacancy, "; ", "Cost: $", table$cost),
                     labelOptions= labelOptions(direction = 'auto')   ) %>%
          addPulseMarkers(
                       lng = map$long, lat = map$lat, 
                       label = paste( "Vacant spots: ", map$vacancy, "; ", "Cost: $", map$cost),
                       icon = makePulseIcon(heartbeat = 7, color="green"))
         
        
         
        
     
        }
          
        
        })
      
   
   
    
   
   
    
    
    
    
    
    ####
    
    
    
  })
  
}
shinyApp(ui, server)




