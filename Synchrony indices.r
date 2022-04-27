######
#
#This file contains functions which, given a 2d array of flowering data,
#will return values of synchrony indices.
#We will focus of Freitas' Index and Chakrabarty's index (both thesis and new).
#The formulae for this can be found in the 3rd document in 'Notes'
#
######

library(tidyverse)
library(flower)

#Freitas index calculation

freitas <- function(data) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flowering number to flowering intensity by dividing
  #number of flowers by max number of flowers produced by that individual
  #Flowering intensity data will also work because **max(data[i]) = 1**
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data)
    data_norm <- rbind(data_norm, tmp)
  }
  #Number of flowering days of each individual
  t_ind <- rowSums(data_norm != 0)
  x1 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        x <- c()
        for (k in 1:t) {
          x <- append(x, sqrt(data_norm[i, k] * data_norm[j, k]))
        }
        x1 <- append(x1, sum(x))
      }
    }
  }
  x1 <- array(x1, dim = c(n - 1, n))
  #print(x1)
  #In x1, each column is the overlap of one individual with every other
  #individual in the population. So, there are n columns and n-1 rows.
  #Summing each column gives us the numerator of Freitas index
  x2 <- c()
  for (i in 1:ncol(x1)) {
    a <- colSums(matrix(x1[, i]))
    x2 <- append(x2, a)
  }
  #print(x2)
  freitas_ind <- array(x2 / (t_ind * (n - 1)), dim = c(1, n))
  freitas_pop <- mean(freitas_ind)
  print(freitas_ind)
  print(freitas_pop)
}

###############################################################################

#Chakrabarty's (thesis) index calculation

chakra_thesis <- function (data) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flower number to flowering intensity
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data)
    data_norm <- rbind(data_norm, tmp)
  }
  #Calculating the numerator
  x1 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        x <- c()
        for (k in 1:t) {
          x <- append(x, data_norm[i, k] * data_norm[j, k])
        }
        x1 <- append(x1, sum(x))
      }
    }
  }
  x1 <- array(x1, dim = c(1, n * (n - 1)))
  #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements
  x2 <- colSums(matrix(x1, nrow = n - 1))   #numerator for each individual
  #print(x2)
  #Calculating the denominator
  x3 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        a <- c()
        b <- c()
        for (k in 1:t) {
          a <- append(a, data_norm[i, k])
          b <- append(b, data_norm[j, k])
        }
        x3 <- append(x3, (sum(a) * sum(b)))
      }
    }
  }
  x3 <- array(x3, dim = c(1, n * (n - 1)))
  #print(x3)
  x4 <- colSums(matrix(x3, nrow = n - 1))
  #print(x4)
  chakra_thesis_ind <- x2/x4
  chakra_thesis_pop <- mean(chakra_thesis_ind)
  print(chakra_thesis_ind)
  print(chakra_thesis_pop)
}

##############################################################################

#Chakrabarty's (new) index calculation

chakra_new <- function (data) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flower number to flowering intensity
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data)
    data_norm <- rbind(data_norm, tmp)
  }
  x1 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        x <- c()
        for (k in 1:t) {
          x <- append(x, data_norm[i, k] * data_norm[j, k])
        }
        x1 <- append(x1, sum(x))
      }
    }
  }
  x1 <- array(x1, dim = c(1, n * (n - 1)))
  #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements # nolint
  x2 <- colSums(matrix(x1, nrow = n - 1))   #numerator for each individual
  #print(x2)
  #Calculating the denominator
  x3 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        #convolution gives the shifted dot product of two vectors
        #so max(z) gives us the maximum overlap of two flowering curves
        #NOTE: NO RIGHT PADDING YET (but works well enough for gaussian curves)
        z <- convolve(data_norm[i, ], rev(data_norm[j, ]), conj = FALSE, type = c("open")) # nolint
        x3 <- append(x3, max(z))
      }
    }
  }
  x3 <- array(x3, dim = c(1, n * (n - 1)))
  x4 <- colSums(matrix(x3, nrow = n - 1))   #denominator for each individual
  #print(x4)
  x4 <- x4 * (n - 1)
  x5 <- x2 / x4
  chakra_new_ind <- x5
  chakra_new_pop <- mean(chakra_new_ind)
  print(chakra_new_ind)
  print(chakra_new_pop)
}

########################################################################

#Modifications to the New Index -
#1. removing (n-1) from the denominator
#2. modifying my maximum intensity of individual

chakra_april <- function (data) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flower number to flowering intensity
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data)
    data_norm <- rbind(data_norm, tmp)
  }
  #Recording maximum intensity of each individual
  max_int <- NULL
  for (i in 1:nrow(data_norm)) {
    max_int <- append(max_int, max(data_norm[i, ]))
  }
  #print(max_int)
  x1 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        x <- c()
        for (k in 1:t) {
          x <- append(x, data_norm[i, k] * data_norm[j, k])
        }
        x1 <- append(x1, sum(x))
      }
    }
  }
  x1 <- array(x1, dim = c(1, n * (n - 1)))
  #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements # nolint
  x2 <- colSums(matrix(x1, nrow = n - 1))   #numerator for each individual
  #print(x2)
  #Calculating the denominator
  x3 <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        #convolution gives the shifted dot product of two vectors
        #so max(z) gives us the maximum overlap of two flowering curves
        #NOTE: NO RIGHT PADDING YET (but works well enough for gaussian curves)
        z <- convolve(data_norm[i, ], rev(data_norm[j, ]), conj = FALSE, type = c("open")) # nolint
        x3 <- append(x3, max(z))
      }
    }
  }
  x3 <- array(x3, dim = c(1, n * (n - 1)))
  x4 <- colSums(matrix(x3, nrow = n - 1))   #denominator for each individual
  #print(x4)
  x4 <- x4        #removed the (n-1) from the denominator
  x5 <- x2 / x4
  #print(x5)
  chakra_new_ind <- x5 * max_int   #Modulating synchrony index of each individual by its max intensity
  chakra_new_pop <- mean(chakra_new_ind)
  #print(chakra_new_ind)
  print(chakra_new_pop)
}

########################################################################

#To calculate mating opportunities
#To calculate total mating opportunities, we find number combinations
#of an individuals number of flowers on a day with all flowers
#on that day, and sum this over time. Divide by flowering duration of individual. #nolint
#Get the mean opportunity of all individuals for population mating opportunity.

#When self-fertilisation is allowed i.e., flowers from the same individual
#can mate with each other. For large populations, outcrossing opportunities is practically the same #nolint

#REVISED mating opportunity by introducing 't_ind'
mating_opp <- function(data) {
    n <- nrow(data)
    t <- ncol(data)
    #Total number of flowers in a day
    no_flo <- colSums(data)
    #print(no_flo)
    #Number of flowering days of each individual
    t_ind <- rowSums(data != 0)
    #print(t_ind)
    #Calculate no. of combinations of mating possible
    x1 <- c()
    x2 <- c()
    for (i in 1:n) {
        x <- c()
        for (k in 1:t) {
            x <- append(x, data[i, k] * no_flo[k])
          }
        x1 <- append(x1, sum(x))
        x2 <- append(x2, sum(x)/t_ind[i]) #mating opportunities of an individual divided by it's flowering duration #nolint
        }
    #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements #nolint
    mating_opp <- mean(x1)   #mating opportunities for the population as a mean of each individual's mating oppurtinity #nolint
    mating_opp_rev <- mean(x2)
    #print(mating_opp) #nolint
    return(mating_opp)
    return(mating_opp_rev)
}

########################################################

#To calculate total mating opportunities
#To calculate total mating opportunities, we find number combinations
#of an individuals number of flowers on a day with all OTHER individuals'
#flowers on that day, and sum this over time. Get the mean opportunity of all #nolint
#individuals for population outcrossing opportunity.

#When self-fertilisation is not allowed i.e., flowers from the same
#individual can NOT mate with each other

outcross_opp <- function(data) {
    n <- nrow(data)
    t <- ncol(data)
    #Total number of flowers in a day
    no_flo <- colSums(data)
    #Number of flowering days of each individual
    t_ind <- rowSums(data != 0)
    #Mating opportunities with other individuals
    x3 <- c()
    x4 <- c()
    for (i in 1:n) {
        x <- c()
        for (k in 1:t) {
            x <- append(x, data[i, k] * (no_flo[k] - data[i, k]))
          }
        x3 <- append(x3, sum(x))
        x4 <- append(x4, sum(x)/t_ind[i]) #mating opportunities of an individual over it's flowering duration #nolint
        }
    #print(x3)   #x1 is only summed over time, so it has n*(n-1) elements
    outcrossing_opp <- mean(x3)   #mating opportunities for the population as a mean of each individual's mating oppurtinity #nolint
    outcrossing_opp_rev <- mean(x4)
    #print(outcrossing_opp)
    return(outcrossing_opp)
    return(outcrossing_opp_rev)
}

########################################################################

#Augspurger's index
#Refer to notes or Augspurger 1983 for the formula
#import 'flower' package to calculate Augspurger's index
#Use 'SI2_onepop' function

data1 <- array(c(0, 0, 0, 0, 10, 10, 0, 0, 0, 0), dim = c(2, 5))
data2 <- array(c(10, 0, 10, 10, 10, 10, 10, 10, 0, 10), dim = c(2, 5))
data3 <- array(c(0, 0, 0, 0, 10, 1, 0, 0, 0, 0), dim = c(2, 5))
data4 <- array(c(10, 1, 10, 1, 10, 1, 10, 1, 10, 1), dim = c(2, 5))

augspurger <- function(data) {
  library(flower)
  n <- nrow(data)
  #Create individual IDs
  ind <- c(1:n)
  augs_si_pop <- mean(SI2_onepop(data, ind))
  return(augs_si_pop)
}

augspurger(data2)
#Problem: Gives Augspurger's for individuals along with the population.

########################################################################

#Data from Souparna Chakrabarty's presentation
data1 <- array(c(0.5, 0, 1, 0, 0.1, 0.5, 0, 1, 0, 0.1), dim = c(2, 5))

freitas(data1)
chakra_thesis(data1)
chakra_new(data1)
chakra_april(data1)

#To show how convolution works
print(data1[1,])
print(data1[2,])
z <- convolve(data1[1,], rev(data1[2,]), conj = FALSE, type = c("open"))
print(z)
print(max(z))

a <- c(1, 2, 1, 0, 0)
b <- c(0, 0, 1, 3, 5)
xkcd <- convolve(a, rev(b), conj = FALSE, type = c("open"))
print(xkcd)
print(max(xkcd))
