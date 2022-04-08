###
#
###

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony")
getwd()

library(vroom)

sp <- vroom("Sample-data_BS_SC.csv")
print(length(sp))

results_all <- matrix(, nrow = nrow(sp), ncol = ncol(sp))

#sp is the data, results_all is the new dataframe for inputting results
#jMax is the dataframe to create maximum overlap, ido to count individual i with other individuals j in population
#Create blank dataframe for maximum overlap
jMax <- data.frame(matrix(0, ncol = 3*length(4:ncol(sp)), nrow = ((2*length(4:ncol(sp)))+1)))
print(length(jMax))

#Input data in blank dataframe with frameshift for each individual i
for(ido in seq_along(sp$ID)[sp$ID!=results_all[id,3]])
{
  for(shift in seq(1,nrow(jMax)))
  {
    jMax[shift,shift:(shift+(length(4:ncol(sp))-1))] <- colMax(rbind(sp[sp[,3]==as.character(results_all[id,3]),4:ncol(sp), drop=FALSE], sp[ido,4:ncol(sp), drop=FALSE]))  
  }
}
#Calculate maximum overlap and individual synchrony
sMax[ido] <- sum(sp[sp[,3]==as.character(results_all[id,3]),4:ncol(sp)]*sp[ido,4:ncol(sp)], na.rm = TRUE)/max(rowSums(jMax*jMax, na.rm = TRUE))
#Calculate mean overlap of species
synchrony <- mean(sMax, na.rm = TRUE)