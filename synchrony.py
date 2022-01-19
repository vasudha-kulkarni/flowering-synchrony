#Practice file to understand how new index differs from Frietas index
import numpy as np
from matplotlib import pyplot as plt

#A symmetrical data set for 5 individuals for 5 days. Matrix elements are intensities of flowering
#Rows are different individuals and columns are different days
#They vary normally in intensity (intensity values range between 0-1)
#Intensity peaks shift by a day (1-2 on day 2, 3rd on day 3, 4-5 on day 4)

#data = np.array([0.25, 0.5, 0.25, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0.25, 0.5, 0.25]).reshape((5,5))
#data = np.array([0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33]).reshape((3,3))
data = np.array([0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25]).reshape((4, 4))

#Constants (total number of days and individuals)
N = len(data)       #rows
T = len(data[0])    #columns

#Frietas index - calculating the numerator separately and then incorporating in the formula
x1 = []
for i in range(N):
    for j in range(N):
        if i!=j:
            x = []
            for t in range(T):
                x.append(np.sqrt(data[i, t]*data[j, t]))
                sum(x)
            x1.append(sum(x))
frietas = (1/T*1/(N-1)*sum(x1))/N
#This is for the population, not the individuals

#print(sum(x2))
#print(frietas) 
#frietas = 0.11

#Modified Frietas index - modf. Calculating numerator and denominator separately
x2 = []
for i in range(N):
    for j in range(N):
        if i!=j:
            x = []
            for t in range(T):
                x.append(data[i, t]*data[j, t])
                sum(x)
            x2.append(sum(x))
modf_num = sum(x2)

x3 = []
for i in range(N):
    for j in range(N):
        if i!=j:
            x = []
            y = []
            z = []
            for t in range(T):
                x.append(data[i, t])
                y.append(data[j, t])
                #z.append(sum(x)*sum(y))
            x3.append(sum(x)*sum(y))
modf_den = sum(x3)

modf = modf_num/modf_den*1/N

#print(modf)

print(x1)
print(x2)
print(x3)
print(frietas, modf)

#For symmetrically distributed data (n=5, t=5), frietas = 0.11, modf = 0.04
#For perfectly synchronous data (n=3, t=3), frietas = 0.33, modf = 0.11
#For perfectly synchronous data (n=3, t=4), frietas = 0.25, modf = 0.083 
#For perfectly synchronous data (n=4, t=4), frietas = 0.25, modf = 0.0625
#For perefectly synchronous: modf = frietas/n -- why??
