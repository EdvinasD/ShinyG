
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
  navbarPage(theme = "bootstrap2.css",fluid=FALSE,
  
  # Application title
  title="Euromonitor C&C v0.01",
  tabPanel("Graphs",
  # Sidebar with a slider input for number of observations
  
  
  fluidRow(column(3, wellPanel(
      # This outputs the dynamic UI component
    radioButtons("typ", "Graph type:",
                 c("Single" = "sing",
                   "Multiple" = "mult",
                   "Region" = "regn")),  
    fileInput('First', 'Choose First CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      uiOutput("ui")
    )),
    
  column(9,
           plotOutput("distPlot",width = "100%", height = "600px")
    ))
  ),
  tabPanel("Data Table",
           radioButtons("tableOpts", "Table type:",
                                     c("Simple" = "simp",
                                       "Complex" = "comp")),
           uiOutput("table"))
 

  
  # Show a plot of the generated distribution
  
    
  
))

