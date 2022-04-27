#Code from Aparna Sunderesvar to create large populations
#Files will be saved in the main folder

for(Tsd in c(0.0001,0.01, 0.1, 1, 2, 5, 7.5, 10))
{
for(Fsd in c(0.2, 1, 1.5)) {
  #Tonmean=sample(runif(1, min = 1, max = 360), size = 1)
  Tonmean = 180
  #Creating files
  FileName <- paste("T_",Tsd,"_F_",Fsd,".csv",sep="")
  file.create(FileName)
  filepath <- file.path(FileName) 
  fileConn <- file(filepath)
  #Parameter
  maxind = 300
  doy = 365
  tree = matrix(rep(0, maxind*(doy)), nrow = maxind, ncol = doy)

  x = c(1:doy)
  write.table(as.matrix(t(x)), sep=",", filepath, col.names=FALSE, row.names=FALSE)

  for(n in 1:maxind)
  {
    #maxflowers=rnorm(1,1000,100)
    maxflowers = 100
    Ton = rnorm(1,Tonmean,Tsd)
    #Ton=Tonmean
    for(eachfl in 1:maxflowers)
    {
      fldate <- rnorm(1,Ton,Fsd)               #rnorm(n,mean,SD)
      if(fldate < 0) {fldate = fldate + doy + 1}
      if(fldate > doy) {fldate = fldate - doy}
      tree[n, fldate] = tree[n, fldate] + 1
    }
    y = c(tree[n, ])
    write.table(as.matrix(t(y)), sep=",", filepath, col.names=FALSE, row.names = FALSE, append = TRUE)
  }
  close(fileConn)
}
}