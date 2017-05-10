#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("read-data.R")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Enrollment data for BIOL"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectizeInput("course","Choose course(s)",
                      sort(unique(as.character(dat$Crse[dat$Subj=="BIOL"]))),
                      selected="300",multiple=T),
       checkboxInput("roster","Include only roster faculty",
                     value=F),
       checkboxInput("fixed","Constrain Y-axes to be the same for each course",
                     value=F)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("totalPlot"),plotOutput("coursePlot"),
       dataTableOutput("CourseInstructorTable")
    )
  )
))
