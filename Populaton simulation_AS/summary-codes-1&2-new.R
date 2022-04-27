########## calculation and writing summary_1 ########
setwd("C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations")
for(Tsd in c(2.5,5))
{
  for (Fsd in c(15))
  {
    Fileout1<-paste("out_T_",Tsd,"_F_",Fsd,".csv",sep="")
    dat2 <- read.csv(file=Fileout1,head=TRUE,sep=",")
    
    summary=data.frame(Run=character(),Reps=character(),SI=character(),on=numeric(),off=numeric(),brdth=numeric(),mean=numeric(),mod=numeric(),med=numeric(),maxfl=numeric(),peron=numeric(),peroff=numeric(),permod=numeric(),permed=numeric(),Census=character(),No.Ind=character())
    
    dat8=data.frame(Reps=character(), SI=character(),on=numeric(),off=numeric(),brdth=numeric(),mean=numeric(),mod=numeric(),med=numeric(),maxflower=numeric(),peron=numeric(),peroff=numeric(),permod=numeric(),permed=numeric(),on5per=numeric(),off5per=numeric(),brdth5per=numeric(),mean5per=numeric(),mod5per=numeric(),med5per=numeric(),maxflower5per=numeric(),on95per=numeric(),off95per=numeric(),brdth95per=numeric(),mean95per=numeric(),mod95per=numeric(),med95per=numeric(),maxflower95per=numeric(),on_cv=numeric(),off_cv=numeric(),brdth_cv=numeric(),mean_cv=numeric(),mod_cv=numeric(),med_cv=numeric(),maxflower_cv=numeric())
    
    
    timeiu<-unique(dat2$Run)
    
    for(w in seq_along(timeiu))
    {
      dat21=dat2[dat2$Run==timeiu[w],]
      
      repsu<- unique(dat2$Reps) #setting reps as unique for every set of SI
      
      for(j in seq_along(repsu)) #looping for every unique rep
      {
        dat3=dat21[dat21$Reps==repsu[j],] #creating a dataframe by susetting using unnique rep number
        SIU <-  unique(dat3$SI) #setting SI as unique
        
        for (i in seq_along(SIU)) #looping for calculating mean for each unique SI in a sequence
        {
          dat20=dat3[dat3$SI==SIU[i],]
          repss=dat20[1,2]
          sumi=colMeans(dat3[dat3$SI==SIU[i],], na.rm = T) #subsetting dat2 wrt unique SI->SIU
          sumph = sumi/repss
          
          navalue=((299-repsu[j])/299)*100
          
          tot=length(dat20$SI)
          per=100*apply(dat3[dat3$SI==SIU[i],12:15],2,max,na.rm=T)/tot
          if(all(per < navalue))
            {
          yy=data.frame(t(c(timeiu[w],repsu[j],SIU[i],sumi[5:11],per[1:4],dat20[1,16],tot)))
          colnames(yy)=c("Run","Reps","SI","on","off","brdth", "mean","mod","med","maxfl","peron","peroff","permod","permed","Census","No.Ind")
          summary= rbind(summary,yy)
          }
          else
          {
            sumii=do.call("cbind", replicate(7,"NA",simplify = F))  
            yy=data.frame(t(c(timeiu[w],repsu[j],SIU[i],sumii,per,dat20[1,16],tot)))
            colnames(yy)=c("Run","Reps","SI","on","off","brdth", "mean","mod","med","maxfl","peron","peroff","permod","permed","Census","No.Ind")
            summary= rbind(summary,yy) 
          }
        }
      }
    }
    
    write.csv(summary, paste("summary_out_T_",Tsd,"_F_",Fsd,"_1.csv",sep=""),row.names = F)
  


    ########## calculation and writing summar_2 ############
    
    
    dat4 <- summary
    dat4$SI<- as.character(dat4$SI)
    dat4$Reps<- as.character(dat4$Reps)
    popm=paste("out_T_",Tsd,"_F_",Fsd,"_300.csv",sep="")
    
    pop=read.csv(file = popm, sep = ",")
    
    popmean= do.call("rbind",replicate(100,colMeans(pop[5:11],na.rm = T), simplify = F))
    
    for(usi in seq_along(unique(dat4$SI)))
    {
      dat5= dat4[dat4$SI==unique(dat4$SI)[usi],]
      
      for(urep in seq_along(unique(dat5$Reps)))
      {
        
        
        dat6=dat5[dat5$Reps==unique(dat5$Reps)[urep],]
        dat6[,4:14]<- as.numeric(unlist(dat6[,4:14]))
        if(any(is.na(dat6[,7]))){
          
          mae= sum(abs(dat6[,7]-popmean[,4]), na.rm = F)/100
          dat7= colMeans(dat6[4:14], na.rm = F)
          dat9= colMeans(dat6[,4:14], na.rm=F)
          dat10= colMeans(dat6[,4:14], na.rm=F)
          dat8= rbind(dat8, as.data.frame(t(c(Reps=unique(dat5$Reps)[urep],SI=unique(dat4$SI)[usi], dat7[1:10], dat9, dat10,mae))))
          
        }
        else{
        mae= sum(abs(dat6[,7]-popmean[,4]), na.rm = T)/100
        dat7= colMeans(dat6[4:14], na.rm = T)
        dat9= apply(dat6[,4:14],2, quantile, probs = c(0.05), na.rm=T)
        dat10= apply(dat6[,4:14],2, quantile, probs = c(0.95), na.rm=T)
        #dat11=apply(dat6[,4:14],2,sd, na.rm=T)
        #dat12=dat11/dat7
        #quantile(dat6[,4], probs=c(0.05,0.95))
        dat8= rbind(dat8, as.data.frame(t(c(Reps=unique(dat5$Reps)[urep],SI=unique(dat4$SI)[usi], dat7[1:10], dat9, dat10,mae))))
        }
        
        
      }
      
    }
    write.csv(dat8, paste("summary_out_T_",Tsd,"_F_",Fsd,"_2.csv",sep=""), row.names = F)
    
  }
}

