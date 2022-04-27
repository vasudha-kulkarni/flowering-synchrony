#library(svMisc)
#library(gmodels)
#library(sjstats)
for(Tsd in c(2,5))
{    
  starttime<-(Sys.time())
  print(starttime)
  for(Fsd in c(2,5))
  {
    FileName<- paste("T_",Tsd,"_F_",Fsd,".csv",sep="")
    
    MyData<- read.csv(file=FileName, header=TRUE, sep=",")
    fida=data.matrix(MyData)
    
    Fileout1<-paste("out_T_",Tsd,"_F_",Fsd,".csv",sep="")
    
    file.create(Fileout1) 
    
    
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

    ############ sequence of sampling interval ###############
    
    
    for(SI in c(1)) #SI of 1 to 60 in steps of 2; sampling always starts from day1
    {
      
      
      ########### sequence of number of replicates to be chosen ##########
      
      for(firstreps in c(299))
      {
        
        for(timei in 1:times) 
        {
          
          ############## random first census date ###############
          
          census= floor(runif(1, min = 1, max =365))
          #census=1
          censusf=365
          
          dates <- seq(census-1, censusf+census-2, SI) %% censusf+1
          numstart=0;numstop=0;nummod=0;nummed=0
          
          reps=list() ## list of individuals already looked at
          vectori=list() ##list of reps to be used
          
          ########### making a list of all the reps #########
          
          
          r=1
          
          while (r <= firstreps & length(reps)<299) 
          {
            loop=sample(floor(runif(300, min = 1, max = 300)), size = 1, replace = F)
            
            while(loop %in% reps)
            {
              loop=sample(floor(runif(300, min = 1, max = 300)), 1, replace = F)
              
            }
            reps=c(reps, c=loop)
            
            
            ########### if data present in dates then using the same loop/individual move on #########
            totfl=0;cumfl=0
            
            for(date in dates)
            {
              
              #print(date)
              totfl=totfl+fida[loop,date] #total flowering over the duration of flowering
              cumfl=cumfl+(date*fida[loop,date]) #summation of flowers*date of flowering over the duration of flowering
            }
            
            ############## checkpoint for NA #################
            
            
            ############ if NA then repeat with other individuals till no NA is found ################
            
            if(totfl==0 & cumfl==0)
            {
              numstart=numstart+1
              numstop=numstop+1
              nummod=nummod+1
              nummed=nummed+1
              r=r
            } else{
              r=r+1
              vectori=c(vectori,c=loop)}
          }
          #if(repscount>799){break}
          
          if(length(vectori)<firstreps) {break}else{
            
            for(i in vectori)
            {
              
              
              starti=0;stopi=0;stopalt=0;brdth=0;brdth2=0
              totfl=0;cumfl=0;fl=0;maxfl=0
              modi=0;mediani=0;stopcheck=0
              
              indi=fida[i,]
              maxfl=max(indi)
              
              for(date in dates)
              {
                totfl=totfl+fida[i,date] #total flowering over the duration of flowering
                cumfl=cumfl+(date*fida[i,date]) 
              }
              
            for(date in dates)
            {
              
              
              if(is.na(fl))
              {date=date+SI} 
              fl=fl+fida[i,date]
              if(fl>(totfl/2)){if(mediani==0){mediani=date}} 
              #print(mediani)
              
             
              
              #median= first date on which there are more than half the maximum flowers
            }
            
            for(date in dates)
            {
              if(date==census){if((fida[i,date])>0){modi=date}} #mode= date of max flower
              if(date!=census)
              {
                if((fida[i,date])>=maxfl)
                {modi=date;maxfl=(fida[i,date])}
                #print(c(SI,loop,date,fida[loop,date],fida[loop,(date-SI)],maxfl,modi))
              }          
              
              if(fida[i,date]>0)
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
                    if(MyData[i,date+(SI*(daypaststop-1))]==0){stopcheck=0}else{stopcheck=1;break}  
                  }
                }
                if(stopcheck==0){stopi=date}
              }
              }
            }
            
            ########### if no NA; write table ##############
            
            
            meani=cumfl/totfl #mean is the weighted average of the flowering days
            breadthi=stopi-starti+1 #breadth when off(is the first day no flowers seen)
            stopalt=stopi-SI #alternate off (off2) when last sampling day of flowers seen thus stopi-"SI" 
            #    if(stopalt<starti){stopalt=starti}
            breadthalt=stopalt-starti+1 #alternate breadth with alternate stop as off
            # print(c(reps,SI,loop,starti))
            y=c(timei,firstreps,SI,i,starti,stopalt,breadthalt, meani, modi,mediani,maxfl,numstart,numstop,nummod,nummed,census)
            #print(c(starti,stopalt))
            write.table(as.matrix(t(y)), sep=",", filepath, col.names=FALSE, row.names=FALSE, append=TRUE)
           }
          }
        }
      }
    }
    close(fileConn)
    endtime<-(Sys.time())
    print(starttime-endtime)
  }
}

