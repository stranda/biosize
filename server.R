
library(shiny)
library(ggplot2)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  datin <- reactive({
    source("read-data.R")
    if (input$roster) 
      dat[dat$classification=="R",] 
    else 
      dat
  })
  
  output$coursePlot <- renderPlot({
    course = as.character(input$course)
    datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
      group_by(Crse,year,season)%>%
      summarize(enrolled=sum(enrolled,na.rm=T))%>%
        ggplot(aes(x=year,y=enrolled,color=season)) +
          geom_line() + facet_wrap(~Crse)
  })
    
  output$totalPlot <-renderPlot({
      
  })
  
  output$CourseInstructorTable <- renderDataTable(
    {
      course = as.character(input$course)
      datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
        select(Crse,Instructor,year,semester,enrolled)%>%
        distinct()
    }
  ) 
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
