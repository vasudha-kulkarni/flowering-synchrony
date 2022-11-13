#This file is to work out some code. Specifically, code for Augspurger's index now

library(tidyverse)

augspurger_mi <- function(data) {
    n <- nrow(data)
    t <- ncol(data)
    #Number of flowering days of each individual
    t_ind <- rowSums(data != 0)
    max_t_ind <- max(t_ind)
    #Create an zeroes dataframe as a placeholder
    flo_day <- matrix(0, n, t)
    #If individual is flowering non-zero flowers, add 1 to the flow_day matrix
    for (i in 1:n) {
        for (k in 1:t) {
            if (data[i, k] > 0) {
                flo_day[i, k] = 1
            }
        }
    }
    x1 <- c()
    for (i in 1:n) {
        for (j in 1:n) {
          if (i != j) {
            x <- c()
            for (k in 1:t) {
                x <- append(x, flo_day[i, k] * flo_day[j, k])
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
  augspurgers_ind <- array(x2 / (max_t_ind * (n - 1)), dim = c(1, n))
  augspurgers_pop <- mean(augspurgers_ind)
  print(augspurgers_ind)
  print(augspurgers_pop)
}

data1 <- array(c(0, 0, 0, 0, 10, 10, 0, 0, 0, 0), dim = c(2, 5))
print(data1)
data2 <- array(c(10, 10, 10, 10, 10, 10, 10, 10, 0, 10), dim = c(2, 5))
print(data2)

augspurger_mi(data2)

#Modified Augspurgers index

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
