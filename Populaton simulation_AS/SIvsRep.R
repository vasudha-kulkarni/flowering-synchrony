####### Sampling frequency required for a pop estimate for given rep number #######
library(ggplot2)
library(gridExtra)
library(grid)
setwd("C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations/outfiles-100runs-november-2019-anymorethan10runs")
gplot<-gList()
for(Tsd in c(0,0.333,1.17,2.5,5,10,15))
{
  for(Fsd in c(0.83,1.67,2.5,3.33,5,8.33,15))
  {
    a <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_2.csv",sep="")
    #e <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_1.csv",sep="")      
    b <- read.csv(file = a, header = T, sep = ",")
    #d <- read.csv(file = e, header = T, sep = ",")
    repsi<- data.frame(Rep=numeric(),SI=numeric())
    for(r in seq_along(unique(b$Reps)))
    {
      b1=subset(b, b$Reps==unique(b$Reps)[r])
      #th=(Tsd*6)*0.05
      b1$V35<- round(b1$V35)
      b2=subset(b1, b1$V35>=0)
      repsi=rbind(repsi, data.frame(Rep=unique(b$Reps)[r],SI=(max(b2$SI, na.rm = T))))
     
    }
    repsi[sapply(repsi,is.infinite)]=NA
    repsi$SF<- round(30/repsi$SI, 1)
    p<-(ggplot(repsi)+
            geom_point(aes(x=Rep,y=SI))+
            geom_path(aes(x=Rep,y=SI))+
            scale_x_continuous(limits = c(0,60),breaks = seq(0,60,5))+
            scale_y_continuous(limits = c(0,60),breaks = seq(0,60,5))+
            theme_bw()+ggtitle(paste0("Tsd","_",Tsd,"_","Fsd","_",Fsd))+
            #theme(axis.text = element_text(size=14, colour = "black"),axis.text.x = element_text(angle = 90), axis.title = element_text(size = 20, colour = "black"), title = element_text(size=16, color="black"))+
          coord_fixed(ratio = 1))
    gplot<-c(gplot, list(p))
  }
}

ggsave(grid.arrange(grobs=gplot, ncol=7),filename = "minimumSI-100runs-morethan10.svg", width = 20, height = 20,units = "in", dpi = 1200)



############## NA analysis ##########
library(ggplot2)
library(gridExtra)
library(grid)
setwd("C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations")
gplot<-gList()
for(Tsd in c(0.333,1.17,2.5,5,10,15))
{
  for(Fsd in c(0.83,1.67,2.5,3.33,5,8.33,15))
  {
    a <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_2.csv",sep="")
    #e <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_1.csv",sep="")      
    b <- read.csv(file = a, header = T, sep = ",")
    #d <- read.csv(file = e, header = T, sep = ",")
    repsi<- data.frame(Rep=numeric(),perNA=numeric())
    for(r in seq_along(unique(b$Reps)))
    {
      b1=subset(b, b$Reps==unique(b$Reps)[r])
      #th=((Fsd*6)+(Tsd*6))*0.05
      #b2=subset(b1, b1$V35<=th)
      repsi=rbind(repsi, data.frame(Rep=unique(b$Reps)[r],perNA=max(b1$peron)))
      
    }
    p<-(ggplot(repsi)+
          geom_point(aes(x=Rep,y=perNA))+
          geom_path(aes(x=Rep,y=perNA))+
          scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10))+
          scale_x_continuous(limits = c(0,60),breaks = seq(0,60,5))+
          theme_bw()+ggtitle(paste0("Tsd","_",Tsd,"_","Fsd","_",Fsd))+
          #theme(axis.text = element_text(size=14, colour = "black"),axis.text.x = element_text(angle = 90), axis.title = element_text(size = 20, colour = "black"), title = element_text(size=16, color="black"))+
          coord_fixed(ratio=1))
    gplot<-c(gplot, list(p))
  }
}

ggsave(grid.arrange(grobs=gplot, ncol=7),filename = "grid_NA.svg", width = 20, height = 20,units = "in", dpi = 1200)





############# NA analysis with SI ###########
library(ggplot2)
library(gridExtra)
library(grid)
setwd("C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations/outfiles-100runs-november-2019-anymorethan10runs")
gplot<-gList()
for(Tsd in c(0,0.333,1.17,2.5,5,10,15))
{
  for(Fsd in c(0.83,1.67,2.5,3.33,5,8.33,15))
  {
    a <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_2.csv",sep="")
    #e <- paste("summary_out_T_",Tsd,"_F_",Fsd,"_1.csv",sep="")      
    b <- read.csv(file = a, header = T, sep = ",")
    #d <- read.csv(file = e, header = T, sep = ",")
    #repsi<- data.frame(SI=numeric(),perNA=numeric())
    #for(r in seq_along(unique(b$SI)))
    #{
     # b1=subset(b, b$SI==unique(b$SI)[r])
      #th=((Fsd*6)+(Tsd*6))*0.05
      #b2=subset(b1, b1$V35<=th)
      #repsi=rbind(repsi, data.frame(SI=unique(b$SI)[r],perNA=max(b1$peron)))
      
    #}
    b$Reps<-factor(b$Reps)
    b$SF<- round(30/b$SI,1)
    p<-(ggplot(b)+
          geom_point(aes(x=SI,y=peron,group=Reps,color=Reps))+
          geom_path(aes(x=SI,y=peron,group=Reps,color=Reps))+
          scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10))+
          scale_x_continuous(limits = c(0,60),breaks = seq(0,60,5))+
          theme_bw()+ggtitle(paste0("Tsd","_",Tsd,"_","Fsd","_",Fsd))+
          theme(axis.text.x = element_text(angle = 90))+
          ylab("Percentage NA")+
          coord_fixed(ratio=6/10))
    gplot<-c(gplot, list(p))
  }
}



ggsave(grid.arrange(grobs=gplot, ncol=7),filename = "NAs.svg",path = "C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations", width = 20, height = 20,units = "in", dpi = 1200)
