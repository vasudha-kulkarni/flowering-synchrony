#####
#
#This code simulates a relationship between pollinator visitation and flower
#density of a population using the formula for sigmoid function from
#the Feldman 2006 paper
#
######

library(tidyverse)
library(sigmoid)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(readr)

######

#Constructing a sigmoid function of pollinator visitation with flower number

x <- seq(-10, 10, by = 0.5)
y <- c()
for (i in x) {
    y <- append(y, logistic(i))
}

data1 <- data.frame(x, y)

p <- ggplot(data = data1, aes(x, y)) + geom_line() + geom_point() + theme_minimal() #nolint
p
    

####################################################################

#To construct a sigmoid curve between pollinator visitation rate and flower density                     #nolint
#All of this is arbitrary numbers
#Take flower density to be number of flowers divided by 100 (as a proxy for 10x10 plot)                 #nolint
#Set number of pollinators = 10, minimum visitation rate = 2, maximum visitation rate = 16              #nolint
#Set threshold for minimum visitation at number*minimum rate = 20 flowers i.e., 0.2 flower density      #nolint
#Find the constants using maximum rate = 16 i.e., max number of flowers they can visit = 160            #nolint
#This means, 1/b = 1.6/2 = 0.8 -> b = 1.25
#Also, a/b = 16 -> a = 20. Adjust c>1  value to get a nice sigmoid curve

a <- 20
b <- 1.25
c <- 3

D <- c(seq(0, 2, 0.05)) #Trial dataset
visit <- function(D) {
    if (D < 0.2) {
    return(V = 0)
    } else {
    return(V = (a * D ^ c)/(1 + b * D ^ c)) }
    }

V <- c()
for (i in 1:length(D)) {
    V <- append(V, visit(D[i]))
}

data1 <- data.frame(D, V)

p <- ggplot(data = data1, aes(x = D, y = V)) + geom_line() + geom_point() + theme_minimal() + #nolint
    ggtitle("Variation in Pollinator visitation with flower density") +
    xlab("Flower density (#flowers per unit area)") + ylab("Pollinator visitation rate (per patch per pollinator)") #nolint
p

#Read and get the number of flowers each day
setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations") # nolint
getwd()

source("Synchrony indices.r")

flo_data <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations\\T_0.01_F_2.csv", col_names = FALSE) # nolint

#To consider the first row as column numbers
header_true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
flo_data <- header_true(flo_data)
data <- as.matrix(flo_data[1:10, 170:190])

#To calculate visting rate during the flowering duration

num_flo <- function(data) {
    #Total number of flowers in a day
    num_flo <- colSums(data)
    return(mean(num_flo))
}

#N_pollinators = 35, min rate = 2, max rate = 16
#1/b = 5.6/2 -> b = 0.36
a <- 35.84
b <- 2.24
c <- 3
visit_rate <- function(data) {
    visit <- function(D) {
        if (D < 0.2) {
        return(V = 0)
        } else {
        return(V = (a * D ^ c)/(1 + b * D ^ c)) }
        }
    num_flo <- colSums(data) / 100
    V <- c()
    for (i in 1:length(num_flo)) {
        V <- append(V, visit(num_flo[i]))
    }
    return(mean(V))
}

num_flo(data)
visit_rate(data)

######################################

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

freitas_si <- c()
chakra_thesis_si <- c()
chakra_new_si <- c()
chakra_april_si <- c()
num_flo_avg <- c()
visit_rate_avg <- c()
file_names <- as.factor(files)

for (i in seq_along(data_files)) {
    #print(file_contents[[i]]) #nolint
    data <- as.matrix(data_files[[i]])
    freitas_si <- append(freitas_si, freitas(data[1:30, ]))
    chakra_thesis_si <- append(chakra_thesis_si, chakra_thesis(data[1:30, ])) #nolint
    chakra_new_si <- append(chakra_new_si, chakra_new(data[1:30, ]))            #nolint
    chakra_april_si <- append(chakra_april_si, chakra_april(data[1:30, ]))      #nolint
    num_flo_avg <- append(num_flo_avg, num_flo(data[1:30, ]))        #nolint
    visit_rate_avg <- append(visit_rate_avg, visit_rate(data[1:30, ]))
}

pollinator_data <- data.frame(file_names, freitas_si, chakra_thesis_si, chakra_new_si, chakra_april_si, num_flo_avg, visit_rate_avg) #nolint
pollinator_data

data_long <- pollinator_data %>% pivot_longer(cols = -c(file_names, num_flo_avg, visit_rate_avg)) #nolint

p_visit_num <- ggplot(data = pollinator_data, aes(x = num_flo_avg/100, y = visit_rate_avg)) +
            geom_point() + theme_minimal() +
            ggtitle("Variation in Pollinator visitation with flower density across all populations") + #nolint
            xlab("Flower density (#flowers per unit area)") + ylab("Pollinator visitation rate (per patch per pollinator)") #nolint
p_visit_num

p_visit_si <- ggplot(data = data_long, aes(x = value, y = visit_rate_avg, colour = name, group = name)) + #nolint
            geom_line() + geom_point() + theme_minimal() +
            ggtitle("Variation in Pollinator visitation with synchrony") +
            xlab("Synchrony indices") + ylab("pollinator visitation rate")

p_visit_si
