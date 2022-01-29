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

for i in range(N):
    plt.plot(t, flowering_data[i])

plt.xlabel('Time (days)')
plt.ylabel('Number of flowers')
plt.title('Flowering overlap = 2 days')
plt.ylim(1, 15)
plt.show()
