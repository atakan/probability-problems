import numpy as np
import matplotlib.pyplot as plt

class Walker:
    def __init__(self, position_index):
        self.p = position_index  # Changed attribute name to "p"

def create_walkers(Nwalkers, Nbins):
    walkers = []
    for _ in range(Nwalkers):
        position_index = np.random.randint(1, Nbins-1)  # Random integer between 1 (inclusive) and Nbins-2 (inclusive)
        walker = Walker(position_index)
        walkers.append(walker)
    return walkers

def diffuse_walkers(walkers):
    new_walkers = []
    for walker in walkers:
        # Decide whether to increase or decrease randomly
        direction = np.random.choice([-1, 1])
        # Create a new walker with the updated position index
        new_position_index = walker.p + direction  # Updated attribute access to "p"
        new_walker = Walker(new_position_index)
        new_walkers.append(new_walker)
    return new_walkers

def multiply_walkers(walkers, npot):
    new_walkers = []
    for walker in walkers:
        np_index = walker.p
        if npot[np_index] < 0:
            prob = min(1, -npot[np_index])
            if np.random.rand() > prob:
                new_walkers.append(walker)
        else:
            prob = min(1, npot[np_index])
            if np.random.rand() < prob:
                new_walkers.append(walker)
                new_walkers.append(walker)  # Add two identical walkers
            else:
                new_walkers.append(walker)
    return new_walkers


def plot_walker_histogram(walkers, Nbins):
    positions = [walker.p for walker in walkers]
    #plt.hist(positions, bins=Nbins, color='skyblue', edgecolor='black')
    plt.hist(positions, bins=31, color='skyblue', edgecolor='black',
            density=True)
    plt.plot(np.arange(0,120),
            np.sin(np.arange(0,120)/120*3.1416)/31/2, "--")
    plt.title('Walker Distribution')
    plt.xlabel('Position')
    plt.ylabel('Number of Walkers')
    plt.show()

# Define parameters
xmin = 0.0
xmax = 1.0
Nbins = 120
Nwalkers = 8
Nevol = 30000
rseed = 4212

# Seed the random number generator
np.random.seed(rseed)

# Generate x array
x = np.linspace(xmin, xmax, Nbins)
dx = x[1] - x[0]

# Create potential array
pot = np.zeros_like(x)
pot[0] = 1e16
pot[-1] = 1e16

# Create walkers
walkers = create_walkers(Nwalkers, Nbins)

# Generate normalized potential array
npot = np.full_like(x, 0.0006)  # Set all elements to 0.5
npot[0] = -1e16  # Set the first element to -1e16
npot[-1] = -1e16  # Set the last element to -1e16

# Perform evolution for Nevol iterations
for i in range(Nevol):
    # Print current iteration number and maximum iteration number
    print(f"Iteration {i+1}/{Nevol} number of walkers {len(walkers)}")
    
    # Plot histogram of walker distribution before diffusing walkers
#    plot_walker_histogram(walkers, Nbins)
    
    # Diffuse the walkers
    walkers = diffuse_walkers(walkers)
    # Multiply walkers based on normalized potential
    walkers = multiply_walkers(walkers, npot)

plot_walker_histogram(walkers, Nbins)
