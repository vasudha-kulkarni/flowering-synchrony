#temp file for miscellanious code

#To plot illustrative graph for 'Notes'
import numpy as np
import matplotlib.pyplot as plt

N = 4    #number of individuals
T = 14    #number of days of flowering for the population
T_mean = np.array([2, 5, 8, 11])     #mean flowering day for each individual
T_sd = np.array([1, 1, 1, 1])       #standard deviation of distribution of flowering days
scale = np.array([30, 30, 30, 30])  #sets the intensity of flowering for each individual

flowering_data = np.zeros((N, T))  

#define a function that gives standard normal distribution centred at x
def gaussian(x, mean, std):
    value = (1/(std*np.sqrt(2*np.pi)) * np.exp(-((x - mean)**2)/2*(std**2)))
    return value

for i in range(N):
    for j in range(T):
        flowering_data[i, j] = int(scale[i]*gaussian(j, T_mean[i], T_sd[i]))

t = np.arange(T)

#for i in range(N):
#    plt.plot(t, flowering_data[i])

#plt.xlabel('Time (days)')
#plt.ylabel('Number of flowers')
#plt.title('Flowering overlap = 2 days')
#plt.ylim(1, 15)
#plt.show()

f_num = np.array([7.36, 5.39, 2.56, 0.96, 0.18])
ct_num = np.array([5.47, 3.36, 1.18, 0.23, 0.016])
cn_num = np.array([5.47, 3.36, 1.18, 0.23, 0.016])

f_den = np.array([15, 15, 15, 15, 15])
ct_den = np.array([18.07, 18.07, 18.07, 18.07, 18.07])
cn_den = np.array([16.43, 16.43, 16.43, 16.43, 16.43])

#Synchrony index for the 2nd individual
f = np.array([0.49, 0.36, 0.17, 0.06, 0.012])
ct = np.array([0.30, 0.18, 0.06, 0.012, 0.0009])
cn = np.array([0.33, 0.20, 0.07, 0.014, 0.0010])

f_pop = np.array([0.49, 0.30, 0.12, 0.04, 0.009])
ct_pop = np.array([0.30, 0.15, 0.049, 0.009, 0.0006])
cn_pop = np.array([0.33, 0.16, 0.054, 0.010, 0.0007])

x = np.array([5, 4, 3, 2, 1])

plt.plot(x, f_num, 'g-', label='f_num')
plt.plot(x, ct_num, 'b-', label='ct and cn den')
plt.plot(x, f_den/2, 'g-.', ct_den/2, 'b-.', cn_den/2, 'r-.')
plt.xlabel('Overlap (days)')
plt.ylabel('Synchrony index numerator')
plt.show()