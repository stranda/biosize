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
      geom_line()+geom_point() + facet_wrap(~Crse,scales=ifelse(input$fixed,"fixed","free"))
  })
    
  output$totalPlot <-renderPlot({
    course = as.character(input$course)
    datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
      group_by(year,season)%>%
      summarize(enrolled=sum(enrolled,na.rm=T))%>%
      ggplot(aes(x=year,y=enrolled,color=season)) +
      geom_point()+geom_smooth(se=F) + ggtitle("Sum of all selected below")
  })
  
  output$CourseInstructorTable <- renderDataTable(
    {
      course = as.character(input$course)
      datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
        select(Crse,Instructor,year,semester,enrolled)%>%
        distinct()
    }
  ) 
})
