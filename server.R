library(shiny)
library(ggplot2)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    datin <- reactive({
    print("sourcing read-data")
    source("read-data.R")
    print("finished read-data")
    switch(input$cls,
            all=dat,
            roster=dat[dat$classification=="R",],
            nonroster=dat[dat$classification!="R",])
  })
  
  output$coursePlot <- renderPlot({
  print("in courseplot")
      course = as.character(input$course)
      datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
      group_by(Crse,year,season)%>%
      summarize(enrolled=sum(enrolled,na.rm=T))%>%
      ggplot(aes(x=year,y=enrolled,color=season)) +
      geom_line()+geom_point() + facet_wrap(~Crse,scales=ifelse(input$fixed,"fixed","free"))
  })
    
    output$totalPlot <-renderPlot({
        print("in totalplot")
    course = as.character(input$course)
    pdat <- datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
      select(Crse,year,season,enrolled)%>%
      group_by(year,season) %>%
      summarize(enrolled=sum(enrolled,na.rm=T))
#    write.csv(file="ecology.csv",row.names=F,pdat)
    ggplot(pdat,aes(x=year,y=enrolled,color=season)) +
        geom_point()+geom_line()+geom_smooth(se=F,color="black") +
        ggtitle("Sum of all selected below")
  })

  output$sectionDist <- renderPlot({
  print("in secdist")
  course = as.character(input$course)
  pltdat <- datin()%>%filter(Subj=="BIOL",Crse%in%course) #%>%
                                        #      group_by(Crse,year,season)%>%
                                        #      summarize(enrolled=sum(enrolled,na.rm=T))
  print(head(pltdat))
  ggplot(pltdat,aes(x=enrolled)) +
      geom_histogram(bins=10) + facet_wrap(~year)+
      ggtitle("Change in section sizes across calendar years")
  })
    
    
  output$sectionsizeTime <- renderPlot({
  print("in timesize")
  course = as.character(input$course)
  pltdat <-   datin()%>%filter(Subj=="BIOL",Crse%in%course) #%>%
#      group_by(year,Crse)%>%
#      summarize(enrolled=mean(enrolled,na.rm=T))
  print(head(pltdat))
  ggplot(pltdat,aes(x=year,y=enrolled)) +
      geom_point() + geom_smooth() +
    ggtitle("Change in mean section size through time (dots are sections)")
  })
    
    
  output$CourseInstructorTable <- renderDataTable(
    {
      course = as.character(input$course)
      datin()%>%filter(Subj=="BIOL",Crse%in%course)%>%
        select(Crse,Instructor,year,semester,enrolled)%>%
        distinct()
    }
  ) 
  
  
  
  observe({
    if ((input$threehundred==TRUE) &(input$grad==TRUE))
    {
      crses=(datin()%>%filter(Subj=="BIOL")%>%select(Crse))[,1]
  #   print(crses)

      crses=crses[-grep("305",crses)]
      crses=crses[-grep("^2",crses)]
      crses=crses[-grep("^1",crses)]     
#      crses=crses[-grep("^6",crses)]
#      crses=crses[-grep("^7",crses)]
#      crses=crses[-grep("^9",crses)]
      crses=sort(crses[!crses%in%c("397","399","499","452","450","453","451","452","448")])
      if (input$labs) { crses=crses[-grep("L|D",crses)]}
       updateSelectizeInput(session,'course',selected =crses,server=F)
    }
    else if ((input$threehundred==TRUE) &(input$grad!=TRUE))
    {
      crses=(datin()%>%filter(Subj=="BIOL")%>%select(Crse))[,1]
      #   print(crses)
      crses=crses[-grep("305",crses)]
      crses=crses[-grep("^2",crses)]
      crses=crses[-grep("^1",crses)]     
      crses=crses[-grep("^5",crses)]
      crses=crses[-grep("^6",crses)]
      crses=crses[-grep("^7",crses)]
      crses=crses[-grep("^9",crses)]
      crses=sort(crses[!crses%in%c("397","399","499","452","450","453","451","452","448")])
      if (input$labs) { crses=crses[-grep("L|D",crses)]}
      updateSelectizeInput(session,'course',selected =crses,server=F)
    }
    else if ((input$threehundred!=TRUE) &(input$grad==TRUE))
    {
      crses=(datin()%>%filter(Subj=="BIOL")%>%select(Crse))[,1]
      #   print(crses)
      
      crses=crses[-grep("305",crses)]
      crses=crses[-grep("^2",crses)]
      crses=crses[-grep("^1",crses)]     
      crses=crses[-grep("^3",crses)]     
      crses=crses[-grep("^4",crses)]
      crses=crses[-grep("^5",crses)]     

      #      crses=crses[-grep("^6",crses)]
      #      crses=crses[-grep("^7",crses)]
      #      crses=crses[-grep("^9",crses)]
      crses=sort(crses[!crses%in%c("397","399","499","452","450","453","451","452","448")])
      if (input$labs) { crses=crses[-grep("L|D",crses)]}
      updateSelectizeInput(session,'course',selected =crses,server=F)
    }
    else 
    {
      updateSelectizeInput(session,'course',selected = "111",server=F)
    }
  })
  
})
