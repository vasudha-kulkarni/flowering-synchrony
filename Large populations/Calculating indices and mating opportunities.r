#####
#
#This file contains code to set github directory and calculate synchrony
#indices using data in .csv files
#
#####

rm(list = ls())

library(tidyverse)
library(readr)

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations") # nolint
getwd()

source("Synchrony indices.r")

#################

#Starting with 4 flowering populations from AS's dataset of large populations #nolint
#with low/high synchrony and low/high duration of flowering
#The results of this can be found in Note 6 (Google drive folder)

#First, read and import the data and calculate indices for specific populations

flo_data <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\T_0.01_F_30.csv", col_names = FALSE) # nolint
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

#To calculate total mating opportunities
mating_opp <- function(data) {
    n <- nrow(data)
    t <- ncol(data)
    #Total number of flowers in a day
    no_flo <- colSums(data)
    #To calculate total mating opportunities, we find number combinations
    #of an individuals number of flowers on a day with all flowers 
    #on that day, and sum this over time. Get the mean opportunity of all
    #individuals for population mating opportunity.
    ###
    #When self-fertilisation is allowed i.e., flowers from the same individual
    #can mate with each other
    x1 <- c()
    for (i in 1:n) {
        x <- c()
        for (k in 1:t) {
            x <- append(x, data[i, k] * no_flo[k])
          }
        x1 <- append(x1, sum(x)) #mating opportunities of an individual over it's flowering duration #nolint
        }
    #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements
    mating_opp <- mean(x1)   #mating opportunities for the population as a mean of each individual's mating oppurtinity #nolint
    #print(mating_opp)
    return(mating_opp)
}

#To calculate total mating opportunities
outcross_opp <- function(data) {
    n <- nrow(data)
    t <- ncol(data)
    #Total number of flowers in a day
    no_flo <- colSums(data)
    #To calculate total mating opportunities, we find number combinations
    #of an individuals number of flowers on a day with all OTHER individuals'
    #flowers on that day, and sum this over time. Get the mean opportunity of all #nolint
    #individuals for population outcrossing opportunity.
    ###
    #When self-fertilisation is not allowed i.e., flowers from the same
    #individual can NOT mate with each other
    x3 <- c()
    for (i in 1:n) {
        x <- c()
        for (k in 1:t) {
            x <- append(x, data[i, k] * (no_flo[k] - data[i, k]))
          }
        x3 <- append(x3, sum(x)) #mating opportunities of an individual over it's flowering duration #nolint
        }
    #print(x3)   #x1 is only summed over time, so it has n*(n-1) elements
    outcrossing_opp <- mean(x3)   #mating opportunities for the population as a mean of each individual's mating oppurtinity #nolint
    #print(outcrossing_opp)
    return(outcrossing_opp)
}

#To check or confirm

data1 <- array(c(0, 0, 0, 0, 10, 10, 0, 0, 0, 0), dim = c(2, 5))
data4 <- array(c(10, 1, 10, 1, 10, 1, 10, 1, 10, 1), dim = c(2, 5))

mating_opp(data4)
outcross_opp(data4)

freitas(data4)
chakra_thesis(data4)
chakra_new(data4)
chakra_april(data4)

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
mating_opp_si <- c()
outcross_opp_si <- c()

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
    chakra_new_si <- append(chakra_new_si, chakra_new(data[1:30, ]))            #nolint
    chakra_april_si <- append(chakra_april_si, chakra_april(data[1:30, ]))      #nolint
    mating_opp_si <- append(mating_opp_si, mating_opp(data[1:30, ]))
    outcross_opp_si <- append(outcross_opp_si, outcross_opp(data[1:30, ]))      #nolint
}

#freitas_si
#chakra_thesis_si
#chakra_new_si
#chakra_april_si
#mating_opp_si
#outcross_opp_si
#file_names

population_data <- data.frame(file_names, freitas_si, chakra_thesis_si, chakra_new_si, chakra_april_si, mating_opp_si, outcross_opp_si) #nolint
population_data

write.csv(population_data, "D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\pop_data_30ind.csv") #nolint
