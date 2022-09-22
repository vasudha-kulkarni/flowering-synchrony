#####
#
#This file contains the code to plot the data generated from 'Calculating
#indices and mating opportunities' file in 'Large populations' folder
#
#####

#I edited the pop_data.csv (30 individuals) files generated to include columns
#for Tsd and Fsd so that they can be plotted separately

library(tidyverse)
library(readr)

library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)

basedata <- read_csv("D:\\3rd Year\\Semester 6\\BI3613 - Sem project\\flowering-synchrony\\Data\\pop_data_100ind.csv") #nolint
basedata$Tsd <- as.factor(basedata$Tsd)
basedata$Fsd <- as.factor(basedata$Fsd)

#To make the next section of plotting easier, use 'pivot' function from 'tidyverse' library #nolint
data_long <- basedata %>% pivot_longer(cols = -c(...1, file_names, Tsd, Fsd, Mating_opportunities)) #nolint
data_long <- data_long[!(data_long$name == 'New'), ]

#Winnow down basedata - for extra Fsd plooting
#basedata1 <- rbind(basedata[(basedata$Tsd == 10), ], basedata[(basedata$Tsd == 5), ], basedata[(basedata$Tsd == 1), ]) #nolint
#basedata2 <- rbind(basedata1[(basedata1$Fsd == 0.2), ], basedata1[(basedata1$Fsd == 1), ], basedata1[(basedata1$Fsd == 2), ], basedata1[(basedata1$Fsd == 5), ], basedata1[(basedata1$Fsd == 10), ], basedata1[(basedata1$Fsd == 20), ], basedata1[(basedata1$Fsd == 30), ]) #nolint

##############################################################

#Plotting data for 100 individuals

baseplot <- ggplot(data = basedata, aes(x = Tsd, y = SC, colour = Fsd, group = Fsd)) + #nolint
        geom_point() + geom_line()
        #geom_tile()

labelled <- baseplot + labs(x = "Tsd", y = "SC index", title = "Variation in SC index with Tsd") #nolint

styled <- labelled + theme_minimal() + theme(
        text = element_text(family = "mono"),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = rel(0.75)),
        axis.text.y = element_text(size = rel(0.75)),
)
styled

#plotting the data

#Tsd represents synchrony i.e., how close the mean flowering days of vaious individuals' are #nolint
#Fsd represents the duration of flowering: long duration -> low intensity

#Variation in synchrony indices with Tsd and Fsd

p_freitas <- ggplot(data = basedata2, aes(x = Fsd, y = freitas_si, colour = Tsd, group = Tsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Freitas Index") +      #nolint
    xlab("Fsd") + ylab("Freitas index")

p_thesis <- ggplot(data = basedata2, aes(x = Fsd, y = chakra_thesis_si, colour = Tsd, group = Tsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Thesis Index") +      #nolint
    xlab("Fsd") + ylab("Thesis index")

p_new <- ggplot(data = basedata2, aes(x = Fsd, y = chakra_new_si, colour = Tsd, group = Tsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in New Index") +      #nolint
    xlab("Fsd") + ylab("New index")

p_april <- ggplot(data = basedata2, aes(x = Fsd, y = chakra_april_si, colour = Tsd, group = Tsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in April Index") +      #nolint
    xlab("Fsd") + ylab("April index")

p_augspurger <- ggplot(data = basedata2, aes(x = Fsd, y = augspurger_si, colour = Tsd, group = Tsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Augsperger Index") +      #nolint
    xlab("Fsd") + ylab("Augspurger index")

grid.arrange(p_freitas, p_thesis, p_new, p_april, p_augspurger)

#####

p_freitas <- ggplot(data = basedata2, aes(x = Tsd, y = freitas_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Freitas Index") +      #nolint
    xlab("Tsd") + ylab("Freitas index")

p_thesis <- ggplot(data = basedata2, aes(x = Tsd, y = chakra_thesis_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in Thesis Index") +      #nolint
    xlab("Tsd") + ylab("Thesis index")

p_new <- ggplot(data = basedata2, aes(x = Tsd, y = chakra_new_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in New Index") +      #nolint
    xlab("Tsd") + ylab("New index")

p_april <- ggplot(data = basedata2, aes(x = Tsd, y = chakra_april_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Variation in April Index") +      #nolint
    xlab("Tsd") + ylab("April index")


p_augspurger <- ggplot(data = basedata2, aes(x = Tsd, y = augspurger_si, colour = Fsd, group = Fsd)) + #nolint
    geom_line() + geom_point() + theme_minimal() + ggtitle("Augsperger Index") +      #nolint
    xlab("Tsd") + ylab("Augspurger index") 

grid.arrange(p_freitas, p_thesis, p_new, p_april, p_augspurger)

#######################################################################

#Creating a heatmap of indice values while varying Fsd and Tsd

data_freitas <- data_long[(data_long$name == "Freitas_mi"), ]
ph_freitas <- ggplot(data = data_freitas, aes(x = Fsd, y = Tsd, fill = value)) +  geom_tile() + #nolint
        theme_minimal() + ggtitle("Heatmap of Freitas index") +
        xlab("Fsd") + ylab("Tsd")

data_thesis <- data_long[(data_long$name == "Thesis_mi"), ]
ph_thesis <- ggplot(data = data_thesis, aes(x = Fsd, y = Tsd, fill = value)) +  geom_tile() + #nolint
        theme_minimal() + ggtitle("Heatmap of Thesis index") +
        xlab("Fsd") + ylab("Tsd")

data_new <- data_long[(data_long$name == "New_mi"), ]
ph_new <- ggplot(data = data_new, aes(x = Fsd, y = Tsd, fill = value)) +  geom_tile() + #nolint
        theme_minimal() + ggtitle("Heatmap of New index") +
        xlab("Fsd") + ylab("Tsd")

data_april <- data_long[(data_long$name == "April_mi"), ]
ph_april <- ggplot(data = data_april, aes(x = Fsd, y = Tsd, fill = value)) +  geom_tile() + #nolint
        theme_minimal() + ggtitle("Heatmap of April index") +
        xlab("Fsd") + ylab("Tsd")

data_augspurger <- data_long[(data_long$name == "Augspurger_mi"), ]
ph_augspurger <- ggplot(data = data_augspurger, aes(x = Fsd, y = Tsd, fill = value)) +  geom_tile() + #nolint
        theme_minimal() + ggtitle("Augspurger's index") +
        xlab("Fsd") + ylab("Tsd")

grid.arrange(ph_freitas, ph_thesis, ph_new, ph_april, ph_augspurger)

#####

#Creating a heatmap of mating opportunities with Tsd and Fsd

ph_mat_opp <- ggplot(data = data_long, aes(x = Fsd, y = Tsd, fill = mating_opp_si)) + geom_tile() + #nolint
        theme_minimal() + ggtitle("Heatmap of mating opportunities") +
        xlab("Fsd") + ylab("Tsd")
ph_mat_opp

ph_outcross_opp <- ggplot(data = data_long, aes(x = Fsd, y = Tsd, fill = outcross_opp_si)) +         #nolint
        geom_tile() + ggtitle("Heatmap of outcrossing opportunities") +
        xlab("Fsd") + ylab("Tsd")
ph_outcross_opp

#####

#How well do mating opportunities correlate with indices?

p_mat <- ggplot(data = data_long, aes(x = mating_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Indices")

p_out <- ggplot(data = data_long, aes(x = outcross_opp_si, y = value, colour = name, group = name)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Variation of indices with outcrossing opportunities") +      #nolint
        xlab("Outcrossing opportunities") + ylab("Indices")

grid.arrange(p_mat, p_out)

#Correlation coefficient between mating opportunities and synchrony indices
cor(basedata$freitas_si, basedata$mating_opp_si)       #0.80
cor(basedata$chakra_thesis_si, basedata$mating_opp_si) #0.99
cor(basedata$chakra_new_si, basedata$mating_opp_si)    #0.18
cor(basedata$chakra_april_si, basedata$mating_opp_si)  #0.31
cor(basedata$augspurger_si, basedata$mating_opp_si)    #0.56

cor(basedata$freitas_si, basedata$outcross_opp_si)       #0.80
cor(basedata$chakra_thesis_si, basedata$outcross_opp_si) #1
cor(basedata$chakra_new_si, basedata$outcross_opp_si)    #0.20
cor(basedata$chakra_april_si, basedata$outcross_opp_si)  #0.34
cor(basedata$augspurger_si, basedata$outcross_opp_si)    #0.58

#########

#Why the oscillatory behaviour of New and April indices?
#See how it changes with Fsd and Tsd
#Refer to the 'Mating opportunities vs indices.r' file

###

#Temp

mat_april <- data_long[(data_long$name == "New"), ]
#mat_april <- mat_april[(mat_april$Fsd == 30), ]  #to see how the behaviour changes at particular Fsd #nolint
#mat_april_tsd <- rbind(mat_april[(mat_april$Tsd == 10), ], mat_april[(mat_april$Tsd == 0.01), ]) #nolint

p_mat_april_fsd <- ggplot(data = mat_april, aes(x = Mating_opportunities, y = value, colour = Fsd, group = Fsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("New SI vs. Mating opportunities") +      #nolint
        ylab("New index") + xlab("Mating opportunities") + theme(
        text = element_text(family = "mono"),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = rel(0.75)),
        axis.text.y = element_text(size = rel(0.75)),
)

p_mat_april_tsd <- ggplot(data = mat_april, aes(x = Mating_opportunities, y = value, colour = Tsd, group = Tsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() +      #nolint
        xlab("Mating opportunities") + ylab("New index") + theme(
        text = element_text(family = "mono"),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = rel(0.75)),
        axis.text.y = element_text(size = rel(0.75)),
)

grid.arrange(p_mat_april_fsd, p_mat_april_tsd)

#### #Augspurger

mat_april <- data_long[(data_long$name == "augspurger_si"), ]
#mat_april <- mat_april[(mat_april$Fsd == 30), ]  #to see how the behaviour changes at particular Fsd #nolint
mat_april_tsd <- rbind(mat_april[(mat_april$Tsd == 10), ], mat_april[(mat_april$Tsd == 0.01), ]) #nolint

p_mat_april_fsd <- ggplot(data = mat_april_tsd, aes(x = mating_opp_si, y = value, colour = Fsd, group = Tsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Augspurger index vs. mating opportunities (Tsd = 10, 0.01)") +      #nolint
        xlab("Mating opportunities") + ylab("Augspurger index")

p_mat_april_tsd <- ggplot(data = mat_april_tsd, aes(x = mating_opp_si, y = value, colour = Tsd, group = Fsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Augspurger index vs. mating opportunities (Tsd = 10, 0.01)") +      #nolint
        xlab("Mating opportunities") + ylab("Augspurger index")

grid.arrange(p_mat_april_fsd, p_mat_april_tsd)

##### #Freitas

mat_april <- data_long[(data_long$name == "freitas_si"), ]
#mat_april <- mat_april[(mat_april$Fsd == 30), ]  #to see how the behaviour changes at particular Fsd #nolint
mat_april_tsd <- rbind(mat_april[(mat_april$Tsd == 10), ], mat_april[(mat_april$Tsd == 0.01), ]) #nolint

p_mat_april_fsd <- ggplot(data = mat_april, aes(x = mating_opp_si, y = value, colour = Fsd, group = Fsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Freitas index vs. mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Freitas index")

p_mat_april_tsd <- ggplot(data = mat_april, aes(x = mating_opp_si, y = value, colour = Tsd, group = Tsd)) +               #nolint
        geom_line() + geom_point() + theme_minimal() + ggtitle("Freitas index vs. mating opportunities") +      #nolint
        xlab("Mating opportunities") + ylab("Freitas index")

grid.arrange(p_mat_april_fsd, p_mat_april_tsd)
