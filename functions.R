createPdfCheck <- function() {
  temp <- tempfile(fileext=".pdf")
  inOld <- input$Older
  inNew <- input$Newer
  
    old <- read.csv(inOld$datapath, stringsAsFactors=FALSE)
    new <- read.csv(inNew$datapath, stringsAsFactors=FALSE)
    
    proc=0.05
    bbbb <- grep("Y",intersect(names(new),names(old))) #[,!sapply(new,function(x) all(is.na(x)))]))
    bounds=bbbb[c(1,length(bbbb))]
    miy=1990
    may=2015
    
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