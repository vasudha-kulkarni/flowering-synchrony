######
#
#This file contains functions which, given a 2d array of flowering data,
#will return values of synchrony indices.
#We will focus of Freitas' Index and Chakrabarty's index (both thesis and new).
#The formulae for this can be found in the 3rd document in 'Notes'
#
######

rm(list = ls())

library(tidyverse)

data <- array(c(5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5), dim = c(4, 3))

#Data from Souparna Chakrabarty's presentation
data1 <- array(c(0.5, 0, 1, 0, 0.1, 0.5, 0, 1, 0, 0.1), dim = c(2, 5))

#Freitas index calculation
###
#NOTE: This function currently requires all individuals to have equal number
#      of flowering days (t_ind), and requires  t_ind as input.
###

freitas <- function(data, t_ind) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flowering number to flowering intensity by dividing
  #number of flowers by max number of flowers produced by that individual
  #Flowering intensity data will also work because **max(data[i]) = 1**
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data[i, ])
    data_norm <- rbind(data_norm, tmp)
  }
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
  x1 <- array(x1, dim = c(1, n * (n - 1)))
  #print(x1)
  x2 <- colSums(matrix(x1, nrow = n - 1)) / (t_ind * (n - 1))
  #print(x2)
  freitas_ind <- x2
  freitas_pop <- mean(freitas_ind)
  print(freitas_ind)
  print(freitas_pop)
}

############################################################################

#Chakrabarty's (thesis) index calculation
###
#NOTE: This function currently requires all individuals to have equal number
#      of flowering days (t_ind), and requires  t_ind as input.
###

chakra_thesis <- function (data, t_ind) {
  n <- nrow(data)
  t <- ncol(data)
  #Converting flower number to flowering intensity
  data_norm <- NULL
  for (i in 1:nrow(data)) {
    tmp <- data[i, ] / max(data[i, ])
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

####################################################################

freitas(data1, 3)
chakra_thesis(data1, 3)
