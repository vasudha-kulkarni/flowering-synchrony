#####
#
#This file contains code to set github directory and calculate synchrony
#indices (with modified intensity) using data in .csv files
#
#####

rm(list = ls())

library(tidyverse)
library(readr)

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations") # nolint
getwd()

source("Synchrony indices_modified intensity.r") #Calculates all modified synchrony indices and mating opportunities #nolint

#################

#Starting with 4 flowering populations from AS's dataset of large populations #nolint
#with low/high synchrony and low/high duration of flowering
#The results of this can be found in Note 6 (Google drive folder)

#First, read and import the data and calculate indices for specific populations

flo_data <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\T_0.01_F_2.csv", col_names = FALSE) # nolint
head(flo_data) #Check whether first row is data or another parameter

#To consider the first row as column numbers
header_true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

flo_data <- header_true(flo_data)

data <- as.matrix(flo_data)
print(data[1:3, 170:190])

####################################################

#To check or confirm

data1 <- array(c(0, 0, 0, 0, 10, 10, 0, 0, 0, 0), dim = c(2, 5))
data3 <- array(c(10, 1, 10, 1, 10, 1, 10, 1, 10, 0), dim = c(2, 5))
data4 <- array(c(10, 1, 10, 1, 10, 1, 10, 1, 10, 1), dim = c(2, 5))

mating_opp(data[1:30, ])
outcross_opp(data[1:30, ])

freitas(data[1:5, ])
chakra_thesis(data[1:5, ])
chakra_new(data[1:5, ])
chakra_april(data[1:5, ])
augspurger(data[1:5, ])
augspurger_mi(data[1:5, ])

####################################################################

#For loop to read all the .csv files in a folder and calculate synchrony indices 
#and mating opportunities and save it as .csv files for further analysis

files <- list.files("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations", pattern = "\\.csv$") #nolint
files

#Compile all the .csv files in one list(?)
data_files <- list()

for (i in seq_along(files)) {
       data_files[[i]] <- read_csv(
           file = files[[i]]
       )
}
data_files

#For some reason, this takes the first row as number of days and not as data.
#Here, header_true is not required

#Create empty lists
freitas_si <- c()
chakra_thesis_si <- c()
chakra_new_si <- c()
chakra_april_si <- c()
augspurger_si <- c()
augspurger_si_mi <- c()
#mahoro_si <- c()
#marquis_si <- c()
#koenig_si <- c()
#albert_si <- c()

mating_opp_si <- c()
outcross_opp_si <- c()
mating_opp_rev <- c()
outcross_opp_rev <- c()

file_names <- as.factor(files) #names of all the files
#file_names

#Calculating indices for different populations using a FOR loop

for (i in seq_along(data_files)) {
    freitas_si <- append(freitas_si, freitas(data_files[[i]][1:30, ]))
    chakra_thesis_si <- append(chakra_thesis_si, chakra_thesis(data_files[[i]][1:30, ]))   #nolint
}

#For reasons unknown, taking index of 'file_contents' is giving an error for
#New and April indices, so I'm converting it to a matrix before calculating these SIs #nolint

for (i in seq_along(data_files)) {
    #print(file_contents[[i]]) #nolint
    data <- as.matrix(data_files[[i]])
    freitas_si <- append(freitas_si, freitas(data[1:30, ]))
    chakra_thesis_si <- append(chakra_thesis_si, chakra_thesis(data[1:30, ]))
    chakra_new_si <- append(chakra_new_si, chakra_new(data[1:30, ]))            #nolint
    chakra_april_si <- append(chakra_april_si, chakra_april(data[1:30, ]))      #nolint
    augspurger_si <- append(augspurger_si, augspurger(data[1:30, ]))
    mating_opp_si <- append(mating_opp_si, mating_opp(data[1:30, ]))
    outcross_opp_si <- append(outcross_opp_si, outcross_opp(data[1:30, ]))      #nolint
}

#Just modified Augspurger and mating opp
for (i in seq_along(data_files)) {
    #print(file_contents[[i]]) #nolint
    data <- as.matrix(data_files[[i]])
    augspurger_si_mi <- append(augspurger_si_mi, augspurger_mi(data[1:100, ]))
    mating_opp_si <- append(mating_opp_si, mating_opp(data[1:100, ]))
}

#for (i in seq_along(data_files)) {
#    #print(file_contents[[i]]) #nolint
#    data <- as.matrix(data_files[[i]])
#    #mating_opp_rev <- append(mating_opp_rev, mating_opp(data[1:30, ])) #nolint
#    augspurger_si <- append(augspurger_si, augspurger(data[1:30, ]))
#    mahoro_si <- append(mahoro_si, mahoro(data[1:30, ]))
#    marquis_si <- append(marquis_si, marquis(data[1:30, ]))
#    koenig_si <- append(koenig_si, koenig(data[1:30, ]))
#    albert_si <- append(albert_si, albert(data[1:30, ]))
#}


#freitas_si
#chakra_thesis_si
#chakra_new_si
#chakra_april_si
#mating_opp_si
#outcross_opp_si
#file_names

#other_si <- data.frame(augspurger_si, mahoro_si, marquis_si, koenig_si, albert_si) #nolint
#write.csv(other_si, "D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\Other_SIs.csv") #nolint

population_data_mod_int <- data.frame(file_names, augspurger_si_mi, mating_opp_si) #nolint
population_data_mod_int

write.csv(population_data_mod_int, "D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\pop_data_100ind_augsperger_mi.csv") #nolint
