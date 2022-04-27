#library(svMisc)
#library(gmodels)
#library(sjstats)


setwd("C:/Users/Aparna/Dropbox/pheno-meta-analysis/flowering simulations")

#t<-txtProgressBar(min = 0, max = 2, initial = 0)
#f<- txtProgressBar(min = 0, max = 2, initial = 0)

for(Tsd in c(2.5,5))
{    
  #setTxtProgressBar(t, Tsd)
  starttime<-(Sys.time())
  for(Fsd in c(0.83,1.67,2.5,3.33,5,8.33,15))
  {
    #setTxtProgressBar(f, Fsd)
    
    FileName<- paste("T_",Tsd,"_F_",Fsd,".csv",sep="")
    
    MyData<- read.csv(file=FileName, header=TRUE, sep=",")
    fida=data.matrix(MyData)
    
    Fileout1<-paste("out_T_",Tsd,"_F_",Fsd,"_300.csv",sep="")
    #Fileout2<-paste("summary_out_T_",Tsd,"_F_",Fsd,"_1.csv",sep="")
    
    #dat8=data.frame(Reps=character(), SI=character(),on=numeric(),off=numeric(),brdth=numeric(),mean=numeric(),mod=numeric(),med=numeric(),maxflower=numeric(),peron=numeric(),peroff=numeric(),permod=numeric(),permed=numeric(),on5per=numeric(),off5per=numeric(),brdth5per=numeric(),mean5per=numeric(),mod5per=numeric(),med5per=numeric(),maxflower5per=numeric(),on95per=numeric(),off95per=numeric(),brdth95per=numeric(),mean95per=numeric(),mod95per=numeric(),med95per=numeric(),maxflower95per=numeric(),on_cv=numeric(),off_cv=numeric(),brdth_cv=numeric(),mean_cv=numeric(),mod_cv=numeric(),med_cv=numeric(),maxflower_cv=numeric())
    
    file.create(Fileout1) 
    #file.create(Fileout2)  
    #filepath2  <- file.path(Fileout2) 
    #fileConn2 <- file(filepath2)
    
    
    #x=c("Run","Reps","SI","on","off","brdth", "mean","mod","med","maxfl","%on","%off","%mod","%med","Census","No.Ind")
    #write.table(as.matrix(t(x)), sep=",", filepath2, col.names=FALSE, row.names=FALSE)
    
    firsttime=0 #for header row in output.csv
    
    filepath<- file.path(Fileout1) 
    fileConn<- file(filepath)
    
    
    #first line
    if(firsttime==0)
    {
      x=c("Run","Reps","SI","#","on","off","brdth","mean","mod","med","maxflower","%on","%off","%mod","%med","Census")
      write.table(as.matrix(t(x)), sep=",", filepath, col.names=FALSE, row.names=FALSE)
      firsttime=99
    }
    
    
    times=1
    #tim<- txtProgressBar(min = 0, max = times, initial = 0)
    
    for(timei in 1:times) 
    {
      starttimerun= Sys.time()
      #setTxtProgressBar(tim, timei)
      ############## random first census date ###############
      
      census= floor(runif(1, min = 1, max =365))
      #census = 215 
      censusf=365
      
      ############ sequence of sampling interval ###############
      
      
      for(SI in c(1)) #SI of 1 to 60 in steps of 2; sampling always starts from day1
      {
        startimesi= Sys.time()
        dates <- seq(census-1, censusf+census-2, SI) %% censusf+1
        
        ########### sequence of number of replicates to be chosen ##########
        
        for(firstreps in c(300))
        {
          startimerep= Sys.time()
          #progress(reps, max.value = NULL, progress.bar = TRUE, char = "|",
          #init = TRUE, console = TRUE, gui = FALSE)
          
          #randreps=sample(floor(runif(300, min = 1, max = 300)), size = firstreps, replace = F)
          
          numstart=0;numstop=0;nummod=0;nummed=0
          
          reps=list()
          
          ########### for each replicate calculating the different parameters #########
          
          r=1
          
          while (r <= firstreps & length(reps)<299) 
          {
            startimefrep=Sys.time()
            loop=sample(floor(runif(300, min = 1, max = 300)), size = 1, replace = F)
            
            while(loop %in% reps)
            {
              loop=sample(floor(runif(300, min = 1, max = 300)), 1, replace = F)
              
            }
            reps=c(reps, c=loop)
            #loop=69
            
            starti=0;stopi=0;stopalt=0;brdth=0;brdth2=0
            totfl=0;cumfl=0;fl=0;maxfl=0
            
            indi=fida[loop,] #individual (indi) defined as the array of the row of the rep randomly generated
            #print(indi)
            maxi=max(indi) #maximum flowers(or data) for that individual
            
            modi=0;mediani=0;stopcheck=0
            
            
            ########### if data present in dates then using the same loop/individual move on #########
            
            
            for(date in dates)
            {
              
              #print(date)
              totfl=totfl+fida[loop,date] #total flowering over the duration of flowering
              cumfl=cumfl+(date*fida[loop,date]) #summation of flowers*date of flowering over the duration of flowering
            }
            
            for(date in dates)
            {
              if(is.na(fl))
              {date=date+SI} 
              fl=fl+fida[loop,date]
              if(fl>(totfl/2)){if(mediani==0){mediani=date}} 
              #print(mediani)
              
              #median= first date on which there are more than half the maximum flowers
            }
            
            for(date in dates)
            {
              if(date==1){if((fida[loop,date])>0){modi=date}} #mode= date of max flower
              if(date>1)
              {
                if((fida[loop,date])>maxfl)
                {modi=date;maxfl=(fida[loop,date])}
                #print(c(SI,loop,date,fida[loop,date],fida[loop,(date-SI)],maxfl,modi))
              }          
              
              if(fida[loop,date]>0)
              {
                if(starti==0){starti=date} #if on a date flowers>0 and start hasnt been set then start= that date
              }
              
              
              if(stopi==0){if(starti>0)
              {
                stopcheck=0 #stopcheck is to check if there is data after daypaststop
                
                for(daypaststop in 1:10) #number of days past stop to be checked (1:number of days)
                {
                  if((date +(SI*(daypaststop-1)))<359)
                  {
                    #            print(c(SI, loop, date, date +(SI*(daypaststop-1))))
                    if(MyData[loop,date+(SI*(daypaststop-1))]==0){stopcheck=0}else{stopcheck=1;break}  
                  }
                }
                if(stopcheck==0){stopi=date}
              }
              }
            }
            
            ############## checkpoint for NA #################
            
            
            
            ############ if NA then repeat with other individuals till no NA is found ################
            
            if(starti==0 || stopi==0 || mediani==0 || modi==0)
            {
              starti="NA";breadthi="NA";breadthalt="NA";numstart=numstart+1
              stopi="NA";stopalt="NA"; breadthi="NA";breadthalt="NA";numstop=numstop+1
              modi="NA";nummod=nummod+1
              mediani="NA";nummed=nummed+1
              maxfl="NA"
              meani="NA"
              # print(c(reps,SI,loop,starti))
              y=c(timei,firstreps,SI,loop,starti,stopalt,breadthalt, meani, modi,mediani,maxfl,numstart,numstop,nummod,nummed,census)
              #print(c(starti,stopalt))
              write.table(as.matrix(t(y)), sep=",", filepath, col.names=FALSE, row.names=FALSE, append=TRUE)
              r=r
              
            }
            
            
            
            else if(starti>0 && stopi>0 && mediani>0 && modi>0)
            {
              # print(starti)
              #  print(stopi)
              
              ########### if no NA; write table ##############
              
              
              meani=cumfl/totfl #mean is the weighted average of the flowering days
              breadthi=stopi-starti+1 #breadth when off(is the first day no flowers seen)
              stopalt=stopi-SI #alternate off (off2) when last sampling day of flowers seen thus stopi-"SI" 
              #    if(stopalt<starti){stopalt=starti}
              breadthalt=stopalt-starti+1 #alternate breadth with alternate stop as off
              # print(c(reps,SI,loop,starti))
              y=c(timei,firstreps,SI,loop,starti,stopalt,breadthalt, meani, modi,mediani,maxfl,numstart,numstop,nummod,nummed,census)
              #print(c(starti,stopalt))
              write.table(as.matrix(t(y)), sep=",", filepath, col.names=FALSE, row.names=FALSE, append=TRUE)
              r=r+1
            }
            
          }
          stoptimefrep=Sys.time()
        }
        stoptimereps=Sys.time()
      }
      stoptimesi=Sys.time()
    }
    stoptimerun=Sys.time()

    close(fileConn)  
    #close(fileConn2)
  }
}
endtime<-(Sys.time())
timeelapsed<- starttime-endtime
timerun= starttimerun-stoptimerun
timesi=startimesi-stoptimesi
timerep= startimerep-stoptimereps
timefr= startimefrep-stoptimefrep