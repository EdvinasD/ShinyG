
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)



shinyServer(function(input, output) {
  
  
  
  
  output$table <- renderUI({
    inFile <- input$First
    if (is.null(inFile))
      return()
    Duomenys <- read.csv(inFile$datapath, stringsAsFactors=FALSE)
    if(input$tableOpts=="comp"){
      output$ex1 <- renderDataTable(Duomenys, options = list(iDisplayLength = 10))
      dataTableOutput("ex1")
    }else{
      output$ex2 <- renderTable(Duomenys)
      tableOutput("ex2")
    }})
  
  
  output$distPlot <- renderPlot({
    
    inFile <- input$First
    
    if(is.null(inFile)){df <- data.frame()
                        print(ggplot(df)+ xlim(-5, 5) + ylim(-5, 5)+theme_bw()+xlab('')+ylab("")+
                                annotate("text", label = "", x = 0, y = 0, size = 8, colour = "Black")+
                                theme(axis.ticks = element_blank(), axis.text = element_blank())
                        )
                        
    }else{
      
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
      dd<- data.frame(x=as.numeric(gsub("Y","",colnames(toplot))),
                      y = as.numeric(toplot) )
      
      g <- ggplot(data=dd,aes(x=x,y=y))+geom_line()
      print(g)
      
      
      
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
    
    
    textas <- "fluidRow(theme='bootstrap.css'"
    for(i in DSli){
      prods <- unique(Duomenys[[i]])
      prods <- gsub("\\W"," ",prods)
      txt=switch(input$typ,  
                 sing = paste("selectInput('",i,"','",i,":',c(",paste("'",prods,"'",sep="",collapse=","),"))", sep=""),
                 mult=paste("selectInput('",i,"','",i,":',multiple = TRUE,c(",paste("'",prods,"'",sep="",collapse=","),"))", sep=""),
                 regn= paste("selectInput('",i,"','",i,":',c(",paste("'",prods,"'",sep="",collapse=","),"))", sep=""))     
      
      textas <- paste(textas,txt,sep=",")
    }
    textas <- paste(textas,",fileInput('Second', 'Choose Second CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')))")
    eval(parse(text=textas))
    
    #Duomenys <- read.csv("new.csv", stringsAsFactors=FALSE)
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
  }) 
  
  
  output$downloadPDF <- downloadHandler(filename = paste("graphs-",Sys.time(),".pdf",sep=""),
                                        content = function(FILE=NULL){
                                          # generate PDF
                                          pdffile <- createPdf()
                                          on.exit(unlink(pdffile))
                                          bytes <- readBin(pdffile, "raw", file.info(pdffile)$size)
                                          writeBin(bytes, FILE)
                                          
                                        })
  
  createPdf <- function() {
    temp <- tempfile(fileext=".pdf")
    pdf(temp)
    dd<- data.frame(x=1:10,
                    y = 1:10 )
    g <- ggplot(data=dd,aes(x=x,y=y))+geom_line()
    print(g)
    if(input$downloadP=="mult")
      print(g)
    dev.off()
    
    return(temp)
    
  }
  
  
  
  output$check <- renderUI({
    inOld <- input$Older
    inNew <- input$Newer
    if (is.null(inOld) | is.null(inNew)){
      return()
    }else{
      DuomOld <- read.csv(inOld$datapath, stringsAsFactors=FALSE)
      DuomNew <- read.csv(inNew$datapath, stringsAsFactors=FALSE)
      DataSlidersFor <- c("CountryName","ProductName")
      DSlOld <- colnames(DuomOld)[colnames(DuomOld)%in%DataSlidersFor]
      DSlNew <- colnames(DuomNew)[colnames(DuomNew)%in%DataSlidersFor]
      DSli <- intersect(DSlOld,DSlNew)
      
      textas <- "fluidRow(theme='bootstrap.css'"
      for(i in DSli){
        prods <- intersect(unique(DuomOld[[i]]),unique(DuomNew[[i]]))
        
        prods <- gsub("\\W"," ",prods)
        txt=paste("selectInput('",paste0(i,"C"),"','",i,":',c(",paste("'",prods,"'",sep="",collapse=","),"))", sep="")    
        
        textas <- paste(textas,txt,sep=",")
      }
      textas <- paste(textas,")")
      eval(parse(text=textas))
      
      #Duomenys <- read.csv("new.csv", stringsAsFactors=FALSE)
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
    }
  })
  
  
  output$downloadPDFCheck <- downloadHandler(filename = paste("graphs-",Sys.time(),".pdf",sep=""),
                                             content = function(FILE=NULL){
                                               # generate PDF
                                               pdffile <- createPdfCheck()
                                               on.exit(unlink(pdffile))
                                               bytes <- readBin(pdffile, "raw", file.info(pdffile)$size)
                                               writeBin(bytes, FILE)
                                               
                                             })
  
  
  
  output$downloadPDFXlsx <- downloadHandler(
    filename = function() { "output.xlsx" },
    content = function(file){
      inOld <- input$Older
      inNew <- input$Newer
      
      old <- read.csv(inOld$datapath, stringsAsFactors=FALSE)
      new <- read.csv(inNew$datapath, stringsAsFactors=FALSE)
      
      proc=input$percent/100
      bbbb <- grep("Y",intersect(names(new),names(old))) #[,!sapply(new,function(x) all(is.na(x)))]))
      bounds=bbbb[c(1,length(bbbb))]
      miy=input$range[1]
      may=input$range[2]
      
      new[bounds[1]:bounds[2]]=apply(new[bounds[1]:bounds[2]],2,as.numeric)
      old[bounds[1]:bounds[2]]=apply(old[bounds[1]:bounds[2]],2,as.numeric)
      d=rbind.fill(cbind(Version="new",new),cbind(Version="old",old))
      
      
      bounds2=bounds+1
      
      difmat=ddply(d,~ProductName+CountryName,function(x){
        
        x<<-x
        cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
        nx=x[x$Version=="new",]
        ox=x[x$Version=="old",]
        
        dx=nx
        dx$Version="diff ratio"
        dx[bounds2[1]:bounds2[2]]=NA  
        
        dx[bounds2[1]:bounds2[2]]=nx[bounds2[1]:bounds2[2]]/ox[bounds2[1]:bounds2[2]]
        
        return(dx)
      })
      logmat=ddply(difmat,~ProductName+CountryName,function(x){
        
        x<<-x
        cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
        out=c()
        if(any(x[bounds2[1]:bounds2[2]]> (1 + proc) | 
                 x[bounds2[1]:bounds2[2]]< (1 - proc),na.rm=T)){
          nm=which.max(abs(range(x[bounds2[1]:bounds2[2]],na.rm=T)-1))
          out=c(1,range(x[bounds2[1]:bounds2[2]],na.rm=T)[nm])
        }
        return(out)
        
      })
      
      dmat=cbind(d,Change=NA,MaxChangeRatio=NA)
      
      if(ncol(logmat)!=0){
        
        um=unique(logmat[,c("ProductName","CountryName")])
        
        for(i in 1:dim(um)[1]){
          
          cat(paste(um[i,"CountryName"],um[i,"ProductName"],sep=" >>> "),"\n")
          dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"Change"]=1
          dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"MaxChangeRatio"]=
            logmat[logmat$ProductName==um[i,"ProductName"] & logmat$CountryName==um[i,"CountryName"],"V2"]
          
        }
        
      }
      dmat=arrange(dmat,ProductName,CountryName)
      
      fname <- paste(file,"xlsx",sep=".")
      outas <- loadWorkbook(fname, create = TRUE)
      
      #      outas <- createWorkbook()
      createSheet(outas, name="Both with Change Column")
      createSheet(outas, name="New")
      createSheet(outas, name="Old")
      createSheet(outas, name="Ratios")
      createSheet(outas, name="Log Information")
      
      
      writeWorksheet(outas, dmat, sheet="Both with Change Column",rownames=FALSE)
      writeWorksheet(outas ,new, sheet="New",rownames=FALSE)
      writeWorksheet(outas, old, sheet="Old",rownames=FALSE)
      writeWorksheet(outas, difmat, sheet="Ratios",rownames=FALSE)
      writeWorksheet(outas, logmat, sheet="Log Information",rownames=FALSE)
      
      options(java.parameters = "-Xmx100000m")
      
      #     saveWorkbook(outas, paste(file,as.character(proc*100)," % precision.xlsx",sep=""))
      saveWorkbook(outas)
      file.rename(fname,file)
      
    })
  
  
  
  output$OldNewPlot <- renderPlot({
    
    inOld <- input$Older
    inNew <- input$Newer
    if (is.null(inOld) | is.null(inNew)){
      df <- data.frame()
      print(ggplot(df)+ xlim(-5, 5) + ylim(-5, 5)+theme_bw()+xlab('')+ylab("")+
              annotate("text", label = "", x = 0, y = 0, size = 8, colour = "Black")+
              theme(axis.ticks = element_blank(), axis.text = element_blank())
      )
    }else{
      old <- read.csv(inOld$datapath, stringsAsFactors=FALSE)
      new <- read.csv(inNew$datapath, stringsAsFactors=FALSE)
      
      proc=input$percent/100
      bbbb <- grep("Y",intersect(names(new),names(old))) #[,!sapply(new,function(x) all(is.na(x)))]))
      bounds=bbbb[c(1,length(bbbb))]
      miy=input$range[1]
      may=input$range[2]
      
      new[bounds[1]:bounds[2]]=apply(new[bounds[1]:bounds[2]],2,as.numeric)
      old[bounds[1]:bounds[2]]=apply(old[bounds[1]:bounds[2]],2,as.numeric)
      d=rbind.fill(cbind(Version="new",new),cbind(Version="old",old))
      
      
      DataSlidersFor <- c("CountryName","ProductName")
      DSlOld <- colnames(old)[colnames(old)%in%DataSlidersFor]
      DSlNew <- colnames(new)[colnames(new)%in%DataSlidersFor]
      DSli <- intersect(DSlOld,DSlNew)
      
      textas <- ""
      for(i in DSli){
        prods <- unique(d[[i]])
        prods <- gsub("\\W"," ",prods)
        txt <-paste(i," == input$",i,"C", sep="")
        if(textas==""){
          textas <- paste(textas,txt,sep="d=subset(d,")
        }else{
          textas <- paste(textas,txt,sep="&") 
        }
      }
      textas <- paste(textas,")")
      cat(textas)
      cat(input$CountryNameC)
      cat(input$ProductNameC)
      eval(parse(text=textas))
      
      
      
      bounds2=bounds+1
      
      difmat=ddply(d,~ProductName+CountryName,function(x){
        
        x<<-x
        cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
        nx=x[x$Version=="new",]
        ox=x[x$Version=="old",]
        
        dx=nx
        dx$Version="diff ratio"
        dx[bounds2[1]:bounds2[2]]=NA  
        
        dx[bounds2[1]:bounds2[2]]=nx[bounds2[1]:bounds2[2]]/ox[bounds2[1]:bounds2[2]]
        
        return(dx)
      })
      
      
      
      
      logmat=ddply(difmat,~ProductName+CountryName,function(x){
        
        x<<-x
        cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
        out=c()
        if(any(x[bounds2[1]:bounds2[2]]> (1 + proc) | 
                 x[bounds2[1]:bounds2[2]]< (1 - proc),na.rm=T)){
          nm=which.max(abs(range(x[bounds2[1]:bounds2[2]],na.rm=T)-1))
          out=c(1,range(x[bounds2[1]:bounds2[2]],na.rm=T)[nm])
        }
        return(out)
        
      })
      
      dmat=cbind(d,Change=NA,MaxChangeRatio=NA)
      
      if(ncol(logmat)!=0){
        
        um=unique(logmat[,c("ProductName","CountryName")])
        
        for(i in 1:dim(um)[1]){
          
          cat(paste(um[i,"CountryName"],um[i,"ProductName"],sep=" >>> "),"\n")
          dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"Change"]=1
          dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"MaxChangeRatio"]=
            logmat[logmat$ProductName==um[i,"ProductName"] & logmat$CountryName==um[i,"CountryName"],"V2"]
          
        }
        
      }
      dmat=arrange(dmat,ProductName,CountryName)
      
      # grpahs ------------------------------------------------------------------
      cola=c("#FAA537","#5F88A1")
      names(cola)=c("new","old")
      
      
      i=1
      
      miny=miy
      maxy=may
      
      cat(paste(um[i,"CountryName"],um[i,"ProductName"],sep=" >>> "),"\n")
      x=dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],]
      names(x)[bounds2[1]:bounds2[2]]=substr(names(x)[bounds2[1]:bounds2[2]],2,5)
      unit=x$Unit[1]
      
      lx=difmat[difmat$ProductName==um[i,"ProductName"] & difmat$CountryName==um[i,"CountryName"],]
      lx=lx[bounds2[1]:bounds2[2]]
      reds1=lx[which(lx>(1+proc) | lx<(1-proc))]
      
      if(range(as.numeric(substr(names(reds1),2,5)))[1]<miny)miny=range(as.numeric(substr(names(reds1),2,5)))[1]
      if(range(as.numeric(substr(names(reds1),2,5)))[2]>maxy)maxy=range(as.numeric(substr(names(reds1),2,5)))[2]
      
      
      x=cbind(x[,c("Version","ProductName","CountryName")],x[bounds2[1]:bounds2[2]])
      x=x[,c(1:3,which(names(x)==as.character(miny)):which(names(x)==as.character(maxy)))]
      
      mx=melt(x,id.vars=c("Version","ProductName","CountryName"))
      mx$variable=as.numeric(as.character(mx$variable))
      
      dx=mx[mx$Version=="new",]
      dx$value=mx[mx$Version=="new","value"]/mx[mx$Version=="old","value"]
      
      ddx=dx[!is.na(dx$value),]
      reds=ddx$variable[which(ddx$value>(1+proc) | ddx$value<(1-proc))]
      
      
      
      g2=ggplot(dx,aes(x=variable,y=value))+
        annotate("rect",ymax=Inf,ymin=-Inf,
                 xmin=reds-0.5,xmax=reds+0.5,fill="blue",alpha=0.05,colour="white")+
        geom_vline(xintercept=c(reds+0.5,reds-0.5),colour="blue",alpha=0.05)+
        theme_bw()+xlab("Year")+ylab("Change Ratio")+
        geom_hline(yintercept=c(1),colour="black",linetype=2)+
        geom_hline(yintercept=c(1+proc,1-proc),colour="blue",linetype=4)+
        geom_line(colour="firebrick3")+geom_point(colour="firebrick3")+
        geom_point(colour="firebrick3")
      
      output$OldNewPlot2 <- renderPlot({
        print(g2)
      })
      
      
      g1=ggplot(mx,aes(x=variable,y=value,group=Version,colour=Version))+
        
        annotate("rect",ymax=Inf,ymin=-Inf,
                 xmin=reds-0.5,xmax=reds+0.5,fill="blue",alpha=0.05,colour="white")+
        geom_vline(xintercept=c(reds+0.5,reds-0.5),colour="blue",alpha=0.05)+
        geom_line()+geom_point()+
        theme_bw()+xlab("Year")+ylab(unit)+scale_colour_manual(values=cola)+
        ggtitle(bquote(atop(.(as.character(mx$CountryName[1])), atop(italic(.(as.character(mx$ProductName[1])), "")))))+
        theme(legend.position="top")
      
      
      print(g1)
      
      
      
      
    }
    # generate and plot an rnorm distribution with the requested
    # number of observations
  })
  
  createPdfCheck <- function() {
    temp <- tempfile(fileext=".pdf")
    inOld <- input$Older
    inNew <- input$Newer
    
    old <- read.csv(inOld$datapath, stringsAsFactors=FALSE)
    new <- read.csv(inNew$datapath, stringsAsFactors=FALSE)
    
    proc=input$percent/100
    bbbb <- grep("Y",intersect(names(new),names(old))) #[,!sapply(new,function(x) all(is.na(x)))]))
    bounds=bbbb[c(1,length(bbbb))]
    miy=input$range[1]
    may=input$range[2]
    
    new[bounds[1]:bounds[2]]=apply(new[bounds[1]:bounds[2]],2,as.numeric)
    old[bounds[1]:bounds[2]]=apply(old[bounds[1]:bounds[2]],2,as.numeric)
    d=rbind.fill(cbind(Version="new",new),cbind(Version="old",old))
    
    if(input$downloadCheck=="sing"){
      DataSlidersFor <- c("CountryName","ProductName")
      DSlOld <- colnames(old)[colnames(old)%in%DataSlidersFor]
      DSlNew <- colnames(new)[colnames(new)%in%DataSlidersFor]
      DSli <- intersect(DSlOld,DSlNew)
      
      textas <- ""
      for(i in DSli){
        prods <- unique(d[[i]])
        prods <- gsub("\\W"," ",prods)
        txt <-paste(i," == input$",i,"C", sep="")
        if(textas==""){
          textas <- paste(textas,txt,sep="d=subset(d,")
        }else{
          textas <- paste(textas,txt,sep="&") 
        }
      }
      textas <- paste(textas,")")
      cat(textas)
      cat(input$CountryNameC)
      cat(input$ProductNameC)
      eval(parse(text=textas))
    }
    
    
    bounds2=bounds+1
    
    difmat=ddply(d,~ProductName+CountryName,function(x){
      
      x<<-x
      cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
      nx=x[x$Version=="new",]
      ox=x[x$Version=="old",]
      
      dx=nx
      dx$Version="diff ratio"
      dx[bounds2[1]:bounds2[2]]=NA  
      
      dx[bounds2[1]:bounds2[2]]=nx[bounds2[1]:bounds2[2]]/ox[bounds2[1]:bounds2[2]]
      
      return(dx)
    })
    
    
    
    
    logmat=ddply(difmat,~ProductName+CountryName,function(x){
      
      x<<-x
      cat(paste(x$CountryName[1],x$ProductName[1],sep=" >>> "),"\n")
      out=c()
      if(any(x[bounds2[1]:bounds2[2]]> (1 + proc) | 
               x[bounds2[1]:bounds2[2]]< (1 - proc),na.rm=T)){
        nm=which.max(abs(range(x[bounds2[1]:bounds2[2]],na.rm=T)-1))
        out=c(1,range(x[bounds2[1]:bounds2[2]],na.rm=T)[nm])
      }
      return(out)
      
    })
    
    dmat=cbind(d,Change=NA,MaxChangeRatio=NA)
    
    if(ncol(logmat)!=0){
      
      um=unique(logmat[,c("ProductName","CountryName")])
      
      for(i in 1:dim(um)[1]){
        
        cat(paste(um[i,"CountryName"],um[i,"ProductName"],sep=" >>> "),"\n")
        dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"Change"]=1
        dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],"MaxChangeRatio"]=
          logmat[logmat$ProductName==um[i,"ProductName"] & logmat$CountryName==um[i,"CountryName"],"V2"]
        
      }
      
    }
    dmat=arrange(dmat,ProductName,CountryName)
    
    # grpahs ------------------------------------------------------------------
    cola=c("#FAA537","#5F88A1")
    names(cola)=c("new","old")
    
    if(input$downloadCheck=="sing"){
      dimensions=1
    }else{dimensions = dim(um)[1]}
    
    
    pdf(temp,width=14,height=10)
    for(i in 1:dimensions){
      
      miny=miy
      maxy=may
      
      cat(paste(um[i,"CountryName"],um[i,"ProductName"],sep=" >>> "),"\n")
      x=dmat[dmat$ProductName==um[i,"ProductName"] & dmat$CountryName==um[i,"CountryName"],]
      names(x)[bounds2[1]:bounds2[2]]=substr(names(x)[bounds2[1]:bounds2[2]],2,5)
      unit=x$Unit[1]
      
      lx=difmat[difmat$ProductName==um[i,"ProductName"] & difmat$CountryName==um[i,"CountryName"],]
      lx=lx[bounds2[1]:bounds2[2]]
      reds1=lx[which(lx>(1+proc) | lx<(1-proc))]
      
      if(range(as.numeric(substr(names(reds1),2,5)))[1]<miny)miny=range(as.numeric(substr(names(reds1),2,5)))[1]
      if(range(as.numeric(substr(names(reds1),2,5)))[2]>maxy)maxy=range(as.numeric(substr(names(reds1),2,5)))[2]
      
      
      x=cbind(x[,c("Version","ProductName","CountryName")],x[bounds2[1]:bounds2[2]])
      x=x[,c(1:3,which(names(x)==as.character(miny)):which(names(x)==as.character(maxy)))]
      
      mx=melt(x,id.vars=c("Version","ProductName","CountryName"))
      mx$variable=as.numeric(as.character(mx$variable))
      
      dx=mx[mx$Version=="new",]
      dx$value=mx[mx$Version=="new","value"]/mx[mx$Version=="old","value"]
      
      ddx=dx[!is.na(dx$value),]
      reds=ddx$variable[which(ddx$value>(1+proc) | ddx$value<(1-proc))]
      
      
      
      g2=ggplot(dx,aes(x=variable,y=value))+
        annotate("rect",ymax=Inf,ymin=-Inf,
                 xmin=reds-0.5,xmax=reds+0.5,fill="blue",alpha=0.05,colour="white")+
        geom_vline(xintercept=c(reds+0.5,reds-0.5),colour="blue",alpha=0.05)+
        theme_bw()+xlab("Year")+ylab("Change Ratio")+
        geom_hline(yintercept=c(1),colour="black",linetype=2)+
        geom_hline(yintercept=c(1+proc,1-proc),colour="blue",linetype=4)+
        geom_line(colour="firebrick3")+geom_point(colour="firebrick3")+
        geom_point(colour="firebrick3")
      
      
      
      g1=ggplot(mx,aes(x=variable,y=value,group=Version,colour=Version))+
        
        annotate("rect",ymax=Inf,ymin=-Inf,
                 xmin=reds-0.5,xmax=reds+0.5,fill="blue",alpha=0.05,colour="white")+
        geom_vline(xintercept=c(reds+0.5,reds-0.5),colour="blue",alpha=0.05)+
        geom_line()+geom_point()+
        theme_bw()+xlab("Year")+ylab(unit)+scale_colour_manual(values=cola)+
        ggtitle(bquote(atop(.(as.character(mx$CountryName[1])), atop(italic(.(as.character(mx$ProductName[1])), "")))))+
        theme(legend.position="top")
      
      print(grid.arrange(g1,g2,heights=c(4,2)))
    }
    dev.off()
    return(temp)
  }  
  
})



