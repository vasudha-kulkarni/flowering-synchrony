#####
#
#From the 'Plotting data'  file there emerged an intriguing discrepancy between
#mating opportunities and synchrony indices. New, Paril and Freitas indices
#don't correlate uniformly with mating opportunities. Instead their correlation
#is modified by Fsd and Tsd. Here, I try to understand why this is happening
#
#####

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(readr)

basedata <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Data\\pop_data_30ind.csv") #nolint
basedata$Tsd <- as.factor(basedata$Tsd)
basedata$Fsd <- as.factor(basedata$Fsd)

#####

data_long <- basedata %>% pivot_longer(cols = -c(file_names, Tsd, Fsd, mating_opp_si, outcross_opp_si, mating_opp_rev, outcross_opp_rev)) #nolint
data_long$name <- as.factor(data_long$name)
data_long <- data_long[(data_long$name != "...1"), ]

#Plots of how synchrony indices vary with mating opposrtunities

p_mat <- ggplot(data = data_long, aes(x = mating_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Indices")
p_mat

#How mating opp changes - Same as the behaviour of thesis index
p_mat_line <- ggplot(data = basedata, aes(x = Tsd, y = mating_opp_si, colour = Fsd, group = Fsd)) +                 #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Mating opportunities with Tsd and Fsd") +      #nolint
        xlab("Tsd") + ylab("Mating opportunities (per individual)")

p_mat_line2 <- ggplot(data = basedata, aes(x = Fsd, y = mating_opp_si, colour = Tsd, group = Tsd)) +                 #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Mating opportunities with Tsd and Fsd") +      #nolint
        xlab("Tsd") + ylab("Mating opportunities (per individual)")

grid.arrange(p_mat_line, p_mat_line2)

p_out <- ggplot(data = data_long, aes(x = outcross_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with outcrossing opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Indices")
p_out

#Revised mating opportunities - possible combinations per individual per day

p_mat_rev <- ggplot(data = data_long, aes(x = mating_opp_rev, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with mating opportunities") +      #nolint
        xlab("Mating opportunities (per ind. per day)") + ylab("Indices")
p_mat_rev

p_out_rev <- ggplot(data = data_long, aes(x = outcross_opp_rev, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with mating opportunities") +      #nolint
        xlab("Mating opportunities (per ind. per day)") + ylab("Indices")
p_out_rev

cor(basedata$chakra_thesis_si, basedata$outcross_opp_rev) #0.97 Hmm, why?

#####

#Why the oscillatory behaviour of New and April indices?
#See how it changes with Fsd and Tsd
#Outcross opp. and New index behave in a very similar way, so not plotting them separately

mat_april <- data_long[(data_long$name == "chakra_april_si"), ]
#mat_april <- mat_april[(mat_april$Fsd == 30), ]  #to see how the behaviour changes at particular Fsd #nolint
mat_april_tsd <- rbind(mat_april[(mat_april$Tsd == 10), ], mat_april[(mat_april$Tsd == 0.01), ]) #nolint

#Modified plot parameters - check properly before plotting
p_mat_april_fsd <- ggplot(data = mat_april_tsd, aes(x = outcross_opp_si, y = value, colour = Fsd, group = Tsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("April index vs. outcross opportunities (Tsd = 10, 0.01)") +      #nolint
        xlab("Outcrossing opportunities") + ylab("April index")
p_mat_april_fsd

p_mat_april_tsd <- ggplot(data = mat_april, aes(x = mating_opp_rev, y = value, colour = Tsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("April index vs. mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("April index")
p_mat_april_tsd

###
#How does Freitas vary with mating opportunities modulated by Fsd and Tsd

mat_freitas <- data_long[(data_long$name == "freitas_si"), ]

p_mat_freitas_fsd <- ggplot(data = mat_freitas, aes(x = mating_opp_si, y = value, colour = Fsd)) +                    #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Freitas index vs. mating opportunities (Fsd)") +      #nolint
        xlab("Mating opportunities") + ylab("Freitas index")
p_mat_freitas_fsd

p_mat_freitas_tsd <- ggplot(data = mat_freitas, aes(x = mating_opp_si, y = value, colour = Tsd)) +                    #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Freitas index vs. mating opportunities (Tsd)") +      #nolint
        xlab("Mating opportunities") + ylab("Freitas index")
p_mat_freitas_tsd

###
#How does Augspurger's vary with mating opportunities modulated by Fsd and Tsd

mat_augspurger <- data_long[(data_long$name == "augspurger_si"), ]

p_mat_augs_fsd <- ggplot(data = mat_augspurger, aes(x = mating_opp_si, y = value, colour = Fsd)) +                    #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Augspurger index vs. mating opportunities (Fsd)") +      #nolint
        xlab("Mating opportunities") + ylab("Augspurger index")
p_mat_augs_fsd

p_mat_augs_tsd <- ggplot(data = mat_augspurger, aes(x = mating_opp_si, y = value, colour = Tsd)) +                    #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Augspurger index vs. mating opportunities (Tsd)") +      #nolint
        xlab("Mating opportunities") + ylab("Augspurger index")
p_mat_augs_tsd

#####

setwd("d:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Large populations") # nolint
getwd()

source("Synchrony indices.r")

#Mating opportunity decreases monotonously with increasing Fsd and Tsd
#But based on the insight from Ganguly et al, at high Tsd, synchrony indices
#initially increase and then decrease with increasing Fsd
#To confirm this, let's go back to dummy populations

#2 individuals flowering 10 flowers, as Tsd remains 'high' and Fsd increases
#Refer Note 6?7? - WRITE IN NOTES!

#Outcross opportunity is a better measure for small populations

d1 <- array(c(0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 10, 0, 0, 0, 0), dim = c(2, 8))
d2 <- array(c(0, 0, 0, 0, 0, 0, 5, 0, 5, 0, 0, 5, 0, 5, 0, 0), dim = c(2, 8))
d3 <- array(c(0, 0, 0, 0, 3, 0, 4, 0, 3, 3, 0, 4, 0, 3, 0, 0), dim = c(2, 8))
d4 <- array(c(0, 0, 1, 0, 2, 0, 3, 1, 3, 2, 1, 3, 0, 3, 0, 1), dim = c(2, 8))
d5 <- array(c(1, 0, 2, 0, 2, 1, 2, 2, 2, 2, 1, 2, 0, 2, 0, 1), dim = c(2, 8))
d6 <- array(c(1, 0, 1, 0, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 0, 1, 0, 1), dim = c(2, 9)) #nolint
d7 <- array(c(1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1), dim=c(2, 12)) #nolint

data_files <- list(d1, d2, d3, d4, d5, d6, d7)

freitas_si <- c()
chakra_thesis_si <- c()
chakra_new_si <- c()
chakra_april_si <- c()
augspurger_si <- c()
outcross_opp_si <- c()
mating_opp_si <- c()
file_names <- c(d1, d2, d3, d4, d5, d6, d7)

for (i in seq_along(data_files)) {
    #print(file_contents[[i]]) #nolint
    data <- as.matrix(data_files[[i]])
    freitas_si <- append(freitas_si, freitas(data))
    chakra_thesis_si <- append(chakra_thesis_si, chakra_thesis(data))   #nolint
    chakra_new_si <- append(chakra_new_si, chakra_new(data))            #nolint
    chakra_april_si <- append(chakra_april_si, chakra_april(data))      #nolint
    augspurger_si <- append(augspurger_si, augspurger(data))
    outcross_opp_si <- append(outcross_opp_si, outcross_opp(data))      #nolint
    mating_opp_si <- append(mating_opp_si, mating_opp(data))
}

simple_data <- data.frame(freitas_si, chakra_thesis_si, chakra_new_si, chakra_april_si, augspurger_si, outcross_opp_si, mating_opp_si) #nolint
write.csv(simple_data, "D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Data\\simple_data.csv") #nolint
