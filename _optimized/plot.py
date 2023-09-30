# Testing scriot the reads comma separated bytes from output.txt and plots them
import numpy as np
import matplotlib.pyplot as plt

# Reading the file
with open('output.txt', 'r') as file:
    data = file.read()

# Converting the string of bytes into a list
byte_values = [int(b) for b in data.split(',')]

# Reshaping the list into a 1024x1024 matrix
matrix = np.array(byte_values).reshape(1024, 1024)

# Plotting the matrix
plt.imshow(matrix, cmap='gray')
plt.show()