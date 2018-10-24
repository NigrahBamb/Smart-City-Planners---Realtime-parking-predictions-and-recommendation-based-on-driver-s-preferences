ui <- bootstrapPage(
  plotOutput('plot')
)


# Define the server code
server <- function(input, output) {
  vacancy <- sample(70:100,7, replace=T)
  x3<-sample(50:75,3,replace=T)
  vacancy<-append(vacancy,x3,after = length(vacancy))
  x3<-sample(0:40,9,replace=T)
  vacancy<-append(vacancy,x3,after = length(vacancy))
  x3<-sample(20:60,3,replace=T)
  vacancy<-append(vacancy,x3,after = length(vacancy))
  vacancy
  Vacancies<-  ts(vacancy,frequency = 1,start = 0,end=24)
  output$plot <- renderPlot({
    plot.ts(Vacancies)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)