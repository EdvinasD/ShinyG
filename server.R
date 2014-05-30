
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
# source("functions.R")


shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    inFile <- input$First
    
    if (is.null(inFile)){df <- data.frame()
                         print(ggplot(df)+ xlim(-5, 5) + ylim(-5, 5)+
                                 annotate("text", label = "Feed me some data", x = 0, y = 0, size = 8, colour = "red")
                         )
                        
    }else{
      
      Duomenys <- read.csv(inFile$datapath, stringsAsFactors=FALSE)
      DataSlidersFor <- c("CountryName","ProductName")
      DSli <- colnames(Duomenys)[colnames(Duomenys)%in%DataSlidersFor]
      textas <- ""
      for(i in DSli){
        prods <- unique(Duomenys[[i]])
        prods <- gsub("\\W"," ",prods)
        txt <-paste(i," == input$",i, sep="")
        if(textas==""){
          textas <- paste(textas,txt,sep="DataPlot=subset(Duomenys,")
        }else{
          textas <- paste(textas,txt,sep="&") 
        }
      }
      textas <- paste(textas,")")
      eval(parse(text=textas))
      toplot <- DataPlot[,grep("Y",colnames(DataPlot))]
      toplot <- toplot[,!is.na(toplot[])]
      g <- plot(as.numeric(gsub("Y","",colnames(toplot))),as.numeric(toplot),type="l")
      print(toplot)
      print(DataPlot)
      print(input$ProductName)
      print(input$CountryName)
      
    }
    # generate and plot an rnorm distribution with the requested
    # number of observations
  })
  output$ui <- renderUI({
    inFile <- input$First
    if (is.null(inFile))
      return()
    Duomenys <- read.csv(inFile$datapath, stringsAsFactors=FALSE)
    DataSlidersFor <- c("CountryName","ProductName")
    DSli <- colnames(Duomenys)[colnames(Duomenys)%in%DataSlidersFor]
    
    
    textas <- "fluidRow(fileInput('Second', 'Choose Second CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))"
    for(i in DSli){
      prods <- unique(Duomenys[[i]])
      prods <- gsub("\\W"," ",prods)
      txt <-paste("selectInput('",i,"','",i,":',c(",paste("'",prods,"'",sep="",collapse=","),"))", sep="")
      textas <- paste(textas,txt,sep=",")
      }
    textas <- paste(textas,")")
    eval(parse(text=textas))
    
    #Duomenys <- read.csv("new.csv", stringsAsFactors=FALSE)
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
   }) 
  })
  






                                                                                                                