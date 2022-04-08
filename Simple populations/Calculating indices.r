#####
#
#This file contains code to set github directory and calculate synchrony
#indices using data in .csv files
#
#####

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(readr)
library(gridExtra)

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Simple populations") # nolint
getwd()

source("Synchrony indices.r")

#################

#Data from SC's presentation. Populations with different duration (short vs long) # nolint
#and different intensity (same vs different)
#1-short, same; 2-long, same; 3-short, different; 4-long, different

data1 <- array(c(0, 0, 0, 0, 10, 10, 0, 0, 0, 0), dim = c(2, 5))
data2 <- array(c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10), dim = c(2, 5))
data3 <- array(c(0, 0, 0, 0, 10, 1, 0, 0, 0, 0), dim = c(2, 5))
data4 <- array(c(10, 1, 10, 1, 10, 1, 10, 1, 10, 1), dim = c(2, 5))
data5 <- array(c(10, 5, 10, 5, 10, 5, 10, 5, 10, 5), dim = c(2, 5))


freitas(data1)
chakra_thesis(data1)
chakra_new(data1)

freitas(data2)
chakra_thesis(data2)
chakra_new(data2)

freitas(data3)
chakra_thesis(data3)
chakra_new(data3)

freitas(data4)
chakra_thesis(data4)
chakra_new(data4)

freitas(data5)
chakra_thesis(data5)
chakra_new(data5)

xkcd <- convolve(data2[1,], rev(data2[2,]), conj = FALSE, type = c("open"))
print(xkcd)
print(max(xkcd))

a1 <- array(c(10, 10), dim = c(2, 1))
a2 <- array(c(10, 10, 10, 10), dim = c(2, 2))
a3 <- array(c(10, 10, 10, 10, 10, 10), dim = c(2, 3))
a4 <- array(c(10, 10, 10, 10, 10, 10, 10, 10), dim = c(2, 4))
a5 <- array(c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10), dim = c(2, 5))

print(chakra_new(a1), chakra_new(a2), chakra_new(a3), chakra_new(a4), chakra_new(a5))
####################################################################

flo_data <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Simple populations\\int1_over1.csv", col_names = FALSE) # nolint
#flo_data <- flo_data[rowSums(is.na(flo_data)) != ncol(flo_data),] #nolint

print(flo_data)

data <- as.matrix(flo_data)
print(data)

freitas(data)
chakra_thesis(data)
chakra_new(data)

#Variation of indices with overlap (maximum to least) when max intensity is the same # nolint
f_num <- c(7.36, 5.39, 2.56, 0.96, 0.18)
ct_num <- c(5.47, 3.36, 1.18, 0.23, 0.016)
cn_num <- c(5.47, 3.36, 1.18, 0.23, 0.016)
num <- c(rev(f_num), rev(ct_num), rev(cn_num))

f_den <- c(15, 15, 15, 15, 15)
ct_den <- c(18.07, 18.07, 18.07, 18.07, 18.07)
cn_den <- c(16.43, 16.43, 16.43, 16.43, 16.43)
den <- c(rev(f_den), rev(ct_den), rev(cn_den))

#Synchrony index for the 2nd individual
f <- c(0.49, 0.36, 0.17, 0.06, 0.012)
ct <- c(0.30, 0.18, 0.06, 0.012, 0.0009)
cn <- c(0.33, 0.20, 0.07, 0.014, 0.0010)
ind <- c(rev(f), rev(ct), rev(cn))
#synchrony indices for the population
f_pop <- c(0.49, 0.30, 0.12, 0.04, 0.009)
ct_pop <- c(0.30, 0.15, 0.049, 0.009, 0.0006)
cn_pop <- c(0.33, 0.16, 0.054, 0.010, 0.0007)
pop <- c(rev(f_pop), rev(ct_pop), rev(cn_pop))

overlap <- rep(c(1, 2, 3, 4, 5), times = 3)
indices <- rep(c("Freitas", "Thesis", "New"), times = c(5, 5, 5))

basedata <- data.frame(overlap, indices, num, den, ind, pop)

#####
#
#Plotting variation in indices
#
#####

#Numerators and denominators
p1 <- ggplot(data = basedata, aes(x = overlap, y = num, colour = indices)) +
    geom_line() + theme_minimal() + ggtitle("Numerator") +
    xlab("Overlap (in days)") + ylab("Numerator") +
    theme(legend.position = "none")
p2 <- ggplot(data = basedata, aes(x = overlap, y = den, colour = indices)) +
    geom_line() + theme_minimal() + ggtitle("Denominator") + ylim(12, 20) +
    xlab("Overlap (in days)") + ylab("Denominator")
p3 <- ggplot(data = basedata, aes(x = overlap, y = ind, colour = indices)) +
    geom_line() + theme_minimal() + ggtitle("Index for an individual") +
    xlab("Overlap (in days)") + ylab("Synchrony index") +
    theme(legend.position = "none")
p4 <- ggplot(data = basedata, aes(x = overlap, y = pop, colour = indices)) +
    geom_line() + theme_minimal() + ggtitle("Index for the population") +
    xlab("Overlap (in days)") + ylab("Synchrony index") +
    theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


#################################################################

#Variation of indices when intensity = 0.67 and overlap decreases from 5 to 1

f_num <- c(5.42, 2.13, 0.18)
ct_num <- c(2.91, 0.76, 0.016)
cn_num <- c(2.91, 0.76, 0.016)

f_den <- c(15, 15, 15)
ct_den <- c(13.6, 9.97, 9.97)
cn_den <- c(12.17, 8.75, 8.75)

#Synchrony index for the 2nd individual
f <- c(0.36, 0.14, 0.012)
ct <- c(0.292, 0.077, 0.0016)
cn <- c(0.33, 0.088, 0.0018)

f_pop <- c(0.39, 0.10, 0.009)
ct_pop <- c(0.295, 0.050, 0.001)
cn_pop <- c(0.33, 0.056, 0.0012)