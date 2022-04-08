#####
#
#This is a file to create toy populations where you can vary the number of individuals, flowering days, and intensity 
#and distribution of flowering. The data is given out as a CSV file, which is easy to manipulate in R
#
#####

import numpy as np
import matplotlib.pyplot as plt
import csv

N = 4    #number of individuals
T = 5    #number of days of flowering for the population
T_mean = np.array([2, 2, 2, 2])     #mean flowering day for each individual
T_sd = np.array([1, 1, 1, 1])       #standard deviation of distribution of flowering days #Fsd
scale = np.array([30, 30, 30, 30])  #sets the intensity of flowering for each individual

flowering_data = np.zeros((N, T))  

#define a function that gives standard normal distribution centred at x
def gaussian(x, mean, std):
    value = (1/(std*np.sqrt(2*np.pi)) * np.exp(-((x - mean)**2)/2*(std**2)))
    return value

for i in range(N):
    for j in range(T):
        flowering_data[i, j] = int(scale[i]*gaussian(j, T_mean[i], T_sd[i]))

print(flowering_data)

#File names - intensity of 1 or 0.67 (for 2 individuals, staggered); 
#Overlap - number of days of overlap b/w consecutive individuals

#Create and write into csv files, which are malleable in R
with open("myfile.csv","w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows(flowering_data)
