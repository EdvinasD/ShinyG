
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
library(xlsx)
library(quantreg)
library(gtools)
library(XLConnect)
load("EMI base.Rdata")

shinyUI(
  navbarPage(theme = "bootstrap2.css",fluid=FALSE,
  
  # Application title
  title="Euromonitor C&C v0.01",
  tabPanel("Graphs",
  # Sidebar with a slider input for number of observations
  
  
  fluidRow(column(3, wellPanel(
      # This outputs the dynamic UI component
  
    uiOutput("ui"),
    radioButtons("typ", "Graph type:",
                 c("Single" = "sing",
                   "Multiple" = "mult",
                   "Region" = "regn")),  
    fileInput('First', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    radioButtons("downloadP", "Download:",
                 c("Single" = "sing",
                   "Multiple" = "mult")),
    downloadButton("downloadPDF", "Download the PDF Graph")
    )),
    
  column(9,
           plotOutput("distPlot",width = "100%", height = "600px")
    ))
  ),
  
  
  tabPanel("Data Table",
           radioButtons("tableOpts", "Table type:",
                                     c("Simple" = "simp",
                                       "Complex" = "comp")),
           uiOutput("table")),
  
################################################################## 
#         Tab For Comparison of two Data sets 
##################################################################  
  
  tabPanel("Old-New",
           fluidRow(column(3, wellPanel(
           fileInput('Older', 'Choose Old CSV File',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
           fileInput('Newer', 'Choose New CSV File',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
           sliderInput("percent", "Percent %:",
                       min = 1, max = 20, value = 5, step = 0.5),
           sliderInput("range", "Years:",
                       1977,  2030, value = c(1977,2030)),
           uiOutput("check"),
           radioButtons("downloadCheck", "Download:",
                        c("Single" = "sing",
                          "Multiple" = "mult")),
           downloadButton("downloadPDFCheck", "Download PDF Graph"),
           downloadButton("downloadPDFXlsx", "Download Full Summary"))),
           column(9,
                  plotOutput("OldNewPlot",width = "100%", height = "600px"),
                  plotOutput("OldNewPlot2",width = "100%", height = "250px")
                  
           ))
           ),
tabPanel("Interpolation",
         fluidRow(column(3, wellPanel(
           fileInput('First', 'Choose CSV File',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))
                  
           ))
)




  
  # Show a plot of the generated distribution
  
    
  
))

