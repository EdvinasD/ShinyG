# packages data -----------------------------------------------------------

rm(list=ls())
setwd("C:/Users/povilas.bockus/Documents/euromonitor/C&C/Change")

library(reshape)
library(foreach)
library(quantreg)
library(ggplot2)
library(TTR)
library(gridExtra)
library(gtools)
library(xlsx)
library(googleVis)
library(reshape)
library(plyr)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(RColorBrewer)

blankPanel<-grid.rect(gp=gpar(col="white"))

load("worldmap2.Rdata")
load("EMI base.Rdata")

rnr=(1:25)[unique(regions$Region) %in% c("Western Europe","Eastern Europe","Europe",
                                         "Americas","Africa and Middle East","Asia Pacific",
                                         "World")]

ec = c("#8B6F66","#7C6D96","#0078BF","#206779","#57854E","#FDCF41","#E07A3F","#BD4F5C")
# e.cols=colorRampPalette(ec)(26)
i.dir="input/"

files=dir(i.dir)

new=read.csv(paste(i.dir,files[1],sep=""),check.names=F)
old=read.csv(paste(i.dir,files[2],sep=""),check.names=F)



# constants ---------------------------------------------------------------

proc=0.05
bounds=c(grep(as.character(1977),names(new)),grep(as.character(2030),names(new)))
miy=2000
may=2015

# programming -------------------------------------------------------------

new[bounds[1]:bounds[2]]=apply(new[bounds[1]:bounds[2]],2,as.numeric)
newcopy=new

pr=as.character(unique(new$ProductName))


for(p in pr){
  
  new=newcopy  
  new=new[new$ProductName==p,]
    
  pdf(file=paste("output/graph out ",p," regions.pdf",sep=""),
      width=14,height=10)
  
  for(i in rnr){
    # i=18 # world graph
    print(as.character(region.coords[i,1]))
    
    d=new[new$CountryName %in% regions[regions$Region==unique(regions$Region)[i],"Country"],]
    
    
    d=d[c("ProductName","CountryName",paste("Y",miy:may,sep=""))]
    names(d)[-(1:2)]=as.character(miy:may)
    md=melt(d,id.vars=names(d)[1:2])
    md$variable=as.numeric(as.character(md$variable))
    
    h=ddply(md,~CountryName,function(x){
      x<<-x
      x=x[!is.na(x$value),]
      x=rbind(head(x,1),tail(x,1))
      x[1,"variable"]=x[1,"variable"]-0.5
      x[2,"variable"]=x[2,"variable"]+0.5
      return(x)
    })
    
    
    g1=ggplot(md,aes(x=variable,y=value,colour=CountryName))+geom_line()+geom_point()+
      xlab("Year")+ylab(md$ProductName[1])+theme_bw()+
      scale_colour_manual(values=colorRampPalette(ec)(dim(d)[1]))+
      geom_text(data=h,aes(label=CountryName,col=CountryName), size=3)+
      theme(legend.position="none")
    
    #     print(g1)
    
    g2=ggplot(md,aes(x=value))+theme_bw()+
      scale_fill_manual(values=colorRampPalette(ec)(dim(d)[1]))+
      theme(legend.position="none")+
      geom_density(col="#5F88A1",fill="#5F88A1",alpha=0.5)+coord_flip()+
      scale_y_reverse()+ylab("")+xlab("")
    
    g3=ggplot(md,aes(x=as.factor(variable),y=value))+theme_bw()+
      geom_boxplot(fill="#5F88A1",alpha=0.4,outlier.shape = NA)+
      ylab("")+xlab("")+theme(axis.text.x = element_blank())
    
    
    
    
    print(grid.arrange(arrangeGrob(g2,g1,widths=c(1,5),ncol=2)
                       ,arrangeGrob(blankPanel,g3,widths=c(1,5),ncol=2),
                       heights=c(5,1),nrow=2,main = textGrob(
                         bquote(atop(.(as.character(unique(regions$Region)[i])), atop(italic(.(p), "")))))))
    
  }
  
  dev.off()
  
}


