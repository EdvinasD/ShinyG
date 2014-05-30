
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



shinyUI(fluidPage(
  
  # Application title
  headerPanel("Euromonitor C&C v0.01"),
  
  # Sidebar with a slider input for number of observations
  fluidRow(

    column(3, wellPanel(
      # This outputs the dynamic UI component
      fileInput('First', 'Choose First CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      uiOutput("ui")
    )),
    
    column(9,
           plotOutput("distPlot")
    )
  )
 

  
  # Show a plot of the generated distribution
  
    
  
))

