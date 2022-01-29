#Practice file to understand how new index differs from Frietas index
import numpy as np
from matplotlib import pyplot as plt

#A symmetrical data set for 5 individuals for 5 days. Matrix elements are intensities of flowering
#Rows are different individuals and columns are different days

data = np.array([[0.5]*5]*5)
#print(data)

#data = np.array([0.5, 0.5, 0.5, 0, 0, 0.5, 0.5, 0.5, 0, 0]).reshape((2,5))   #data1
#data = np.array([0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5, 0]).reshape((2,5))   #data2
#data = np.array([0.5, 0.5, 0.5, 0, 0, 0, 0, 0.5, 0.5, 0.5]).reshape((2,5))   #data3
#data = np.array([0.5, 0.5, 0, 0, 0, 0, 0, 0, 0.5, 0.5]).reshape((2,5))       #data4


#Other types of toy data sets
#N=5, T=5, symmetrical but not perfect synchrony
#data = np.array([0.25, 0.5, 0.25, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0, 0.25, 0.5, 0.25, 0, 0, 0.25, 0.5, 0.25]).reshape((5,5))
#N=5, T=5, symmetrical (not uniform) and perfect synchrony
#data = np.array([0.25, 0.5, 0.5, 0.25, 0.25, 0.5, 0.5, 0.25, 0.25, 0.5, 0.5, 0.25, 0.25, 0.5, 0.5, 0.25]).reshape((4, 4))

#data from Souparna Chakrabarty's presentation
#data = np.array([0.5, 1, 0.1, 0, 0, 0, 0, 0.5, 1, 0.1]).reshape(2,5)

#They vary normally in intensity (intensity values range between 0-1)
#Intensity peaks shift by a day (1-2 on day 2, 3rd on day 3, 4-5 on day 4)

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
y1 = []
for i in range(0, len(x1)):
    a = sum(x1[i:i+(N-1)])         #sums the first n-1 elements, frameshifts and sums the next n-1 elements again
    y1.append(a/(T*(N-1)))         #Not quite right because it has to be divided by T(i) - number of days the individual flowers
y1 = np.array(y1)
frietas_ind = y1[0::N-1]           #only keep every (n-1)th element, in between are sums of frameshifted elements
frietas_pop = np.mean(frietas_ind)

#frietas = (sum(x1)/(T*(N-1)))/N
#This is for the entire population, not the individuals. sum(x1) gives the sum of root square of intensities
#for all pairs of individuals in the population

####################################

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

#From x2 and x3 we have the elements that would go into the numerator and denominator. Calculation from these
#has to be done carefully. They will have N*(N-1) elements. (N-1) elements will have to be grouped and 
#summed and added to x4, x5. Then respective elements will have to be divided, which will give the synchrony 
#index for individuals. Mean of this will be population synchrony
x4 = []
x5 = []
for i in range(0, len(x2)):
    a = sum(x2[i:i+(N-1)])
    x4.append(a)
x4 = np.array(x4)
x4 = x4[0::N-1]

for i in range(0, len(x3)):
    a = sum(x3[i:i+(N-1)])
    x5.append(a)
x5 = np.array(x5)
x5 = x5[0::N-1]

modf_ind = x4/x5
modf_pop = np.mean(modf_ind)

print(x1)
#print(x2)
#print(x3)  #To understand the numerators and denominators in the indices
print(frietas_pop, frietas_ind, modf_pop, modf_ind)

#How would these indices vary with changes in number of individuals and number of days?
#N=5, f = 0.5, T=5, 10, 15, 20, 25
f_t = np.array([0.5, 0.5, 0.5, 0.5, 0.5])
modf_t = np.array([0.2, 0.1, 0.067, 0.05, 0.04])

#T=5, f = 0.5,  N=5, 10, 20, 40, 100
#Indices doesn't change with number of individuals
f_n = np.array([0.5, 0.5, 0.5, 0.5])
modf_n = np.array([0.2, 0.2, 0.2, 0.2])

#T=5, N=5, intensity, f = 0.1, 0.25, 0.5, 0.75, 1
f_i = np.array([0.1, 0.25, 0.5, 0.75, 1.0])
modf_i = np.array([0.2, 0.2, 0.2, 0.2, 0.2])

#variation with overlap - data1 (100%), data2 (66%), data3 (33%), and data4(0%)
f_ov = np.array([0.33, 0.2, 0.1, 0])
modf_ov = np.array([0.33, 0.22, 0.11, 0])

################################################

#Plotting the variations
#Variation with number of days (N = 5, f = 0.5)
x = np.array([5, 10, 15, 20, 25])
plt.plot(x, f_t, '--og', label="Frietas")
plt.plot(x, modf_t, '-.ob', label="Modified Frietas")
plt.ylim(0, 1)
plt.title("Variation in indices with duration")
plt.xlabel("Duration")
plt.ylabel("Synchrony index")
plt.legend()
plt.show()

#Variation in intensity (N = 5, T = 5)
x = np.array([0.1, 0.25, 0.5, 0.75, 1])
plt.plot(x, f_i, '--og', label="Frietas")
plt.plot(x, modf_i, '-.ob', label="Modified Frietas")
plt.ylim(0, 1.2)
plt.title("Variation in indices with intensity")
plt.xlabel("Intensity")
plt.ylabel("Synchrony index")
plt.legend()
plt.show()

#Variation in overlap of 2 individuals
x = np.array([100, 66, 33, 0])
plt.plot(x, f_ov, '--og', label="Frietas")
plt.plot(x, modf_ov, '-.ob', label="Modified Frietas")
plt.ylim(0, 1)
plt.title("Variation in indices with overlap")
plt.xlabel("Overlap (in %)")
plt.ylabel("Synchrony index")
plt.legend()
plt.show()