######
#
#This file contains functions which, given a 2d array of flowering data,
#will return values of synchrony indices.
#We will focus of Freitas' Index and Chakrabarty's index (both thesis and new).
#The formulae for this can be found in the 3rd document in 'Notes'
#
######

library(tidyverse)
library(stats)

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
  print(x2)
  freitas_ind <- array(x2 / (t_ind * (n - 1)), dim = c(1, n))
  freitas_pop <- mean(freitas_ind)
  print(freitas_ind)
  print(freitas_pop)
}

############################################################################

#Chakrabarty's (thesis) index calculation

chakra_thesis <- function(data) {
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
  #print(x1)   #x1 is only summed over time, so it has n*(n-1) elements # nolint
  x2 <- colSums(matrix(x1, nrow = n - 1))   #numerator for each individual
  print(x2)
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
  print(x4)
  chakra_thesis_ind <- x2/x4
  chakra_thesis_pop <- mean(chakra_thesis_ind)
  print(chakra_thesis_ind)
  print(chakra_thesis_pop)
}

####################################################################

#Chakrabarty's (new) index calculation
#(n - 1) in the denominator removed

chakra_new <- function(data) {
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
  x4 <- x4 
  #print(x4)
  x5 <- x2 / x4
  chakra_new_ind <- x5
  chakra_new_pop <- mean(chakra_new_ind)
  print(chakra_new_ind)
  print(chakra_new_pop)
}

a <- c(0, 0, 0, 0, 1, 3, 1)
b <- c(0, 1, 3, 5, 1, 0, 0)
xkcd <- convolve(a, rev(b), conj = FALSE, type = c("open"))
print(xkcd)
print(max(xkcd))
