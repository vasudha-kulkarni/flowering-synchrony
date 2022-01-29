#####
#
#This file contains code to set github directory and calculate synchrony
#indices using data in .csv files
#
#####

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(vroom)

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony")
getwd()

source("Synchrony indices.r")

flo_data <- vroom("int1_over2.csv")
flo_data <- flo_data[rowSums(is.na(flo_data)) != ncol(flo_data),]

print(flo_data)

freitas(flo_data, 5)
chakra_thesis(flo_data, 5)
