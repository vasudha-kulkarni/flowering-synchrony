#####
#
#This file contains the code to plot the data generated from 'Calculating
#indices and mating opportunities' file in 'Large populations' folder
#
#####

#I edited the pop_data.csv (30 individuals) files generated to include columns
#for Tsd and Fsd so that they can be plotted separately

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(readr)

basedata <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Data\\pop_data_30ind.csv") #nolint
basedata$Tsd <- as.factor(basedata$Tsd)
basedata$Fsd <- as.factor(basedata$Fsd)

#####

#plotting the data

#Tsd represents synchrony i.e., how close the mean flowering days of vaious individuals' are #nolint
#Fsd represents the duration of flowering: long duration -> low intensity

#Variation in synchrony indices with Tsd and Fsd

p_freitas <- ggplot(data = basedata, aes(x = Tsd, y = freitas_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Freitas Index") +      #nolint
    xlab("Tsd") + ylab("Freitas index")

p_thesis <- ggplot(data = basedata, aes(x = Tsd, y = chakra_thesis_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Thesis Index") +      #nolint
    xlab("Tsd") + ylab("Thesis index")

p_new <- ggplot(data = basedata, aes(x = Tsd, y = chakra_new_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in New Index") +      #nolint
    xlab("Tsd") + ylab("New index")

p_april <- ggplot(data = basedata, aes(x = Tsd, y = chakra_april_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in April Index") +      #nolint
    xlab("Tsd") + ylab("April (with fmax) index")

grid.arrange(p_freitas, p_thesis, p_new, p_april, ncol = 2, nrow = 2)

#####

#To make the next section of plotting easier, use 'pivot' function from 'tidyverse' library #nolint
data_long <- basedata %>% pivot_longer(cols = -c(file_names, Tsd, Fsd, mating_opp_si, outcross_opp_si)) #nolint
data_long$name <- as.factor(data_long$name)
data_long <- data_long[(data_long$name != "...1"), ]

#Comparing indices across Fsd
#For loop not working in VSCode. Remove values from fsd and get plots consecutively #nolint

fsd <- c(2, 5, 7.5, 10, 15, 20, 30)
for (i in 1:length(fsd)) {
    tsd_i <- data_long[(data_long$Fsd == fsd[i]), ]
    p_f <- ggplot(data = tsd_i, aes(x = Tsd, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Comparing Indices across Fsd") +      #nolint
        xlab("Tsd") + ylab("Indices")
    print(p_f)
}

#How well do mating opportunities correlate with indices?

p_mat <- ggplot(data = data_long, aes(x = mating_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Indices")
p_mat

p_out <- ggplot(data = data_long, aes(x = outcross_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with outcrossing opportunities") +      #nolint
        xlab("Outcrossing opportunities") + ylab("Indices")
p_out
