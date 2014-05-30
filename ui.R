
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(scales)
library(reshape)
library(reshape2)
library(foreach)
library(quantreg)
library(ggplot2)
library(TTR)
library(gridExtra)
library(RColorBrewer)



shinyUI(
  navbarPage(theme = "bootstrap2.css",
  
  # Application title
  title="Euromonitor C&C v0.01",
  tabPanel("Graphs",
  # Sidebar with a slider input for number of observations
  fluidRow(

     wellPanel(
      # This outputs the dynamic UI component
      fileInput('First', 'Choose First CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      uiOutput("ui")
    ),
    
    column(9,
           plotOutput("distPlot")
    )
  )),
  tabPanel("Data Table",fileInput('dsa', 'Choose dsaFirst CSV File',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))
 

  
  # Show a plot of the generated distribution
  
    
  
))

