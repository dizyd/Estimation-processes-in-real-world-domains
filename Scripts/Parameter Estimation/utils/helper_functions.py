import numpy as np
import numpy.random as rng
from scipy.stats import truncnorm
import matplotlib.pyplot as plt
import seaborn as sns


def truncnorm_r(mean=0, sd=1, low=-10, upp=10, size=1):

  return truncnorm.rvs(
    (low - mean) / sd, (upp - mean) / sd, loc=mean, scale=sd, size=size
  )



def timer(func):
    @wraps(func)
    def inner(*args, **kwargs):
        start_time = time.time()
        retval = func(*args, **kwargs)
        print(f"{time.time() - start_time:.2f} seconds elapsed")
        return retval
    return inner



def reparametarize_gamma(gamma_mode, gamma_sigma):
  """
  Calculates 'rate' and 'shape' using Numba for potential speedup.

  Args:
    gamma_mode: The mode parameter (float or numpy array).
    gamma_sigma: The standard deviation parameter (float or numpy array).

  Returns:
    A tuple containing 'rate' and 'shape' (both float or numpy arrays).
  """

  rate = (gamma_mode + np.sqrt(gamma_mode**2 + 4 * gamma_sigma**2)) / (2 * gamma_sigma**2)
  shape = 1 + gamma_mode * rate
  
  return shape, rate





def truncated_cauchy_np(loc=0, scale=1, lower=0.01, upper=100, size=1):

    samples = []

    while len(samples) < size:

        s = np.random.standard_cauchy(size=size) * scale + loc
        s = s[(s >= lower) & (s <= upper)]
        samples.extend(s[:size - len(samples)])
        
    return np.array(samples)


def truncated_poisson_np(l, lower=1, upper=10, size=1):
    samples = []
    while len(samples) < size:
        s = np.random.poisson(l,size=size)
        s = s[(s >= lower) & (s <= upper)]
        samples.extend(s[:size - len(samples)])
    return np.array(samples)





def describe(arr):
  """
  Calculates and returns the mean, standard deviation, and range of a NumPy array.

  Args:
    arr (np.ndarray): The input NumPy array.

  Returns:
    tuple: A tuple containing the mean, standard deviation, and range
           in that order. Returns None if the input is not a NumPy array
           or if the array is empty.
  """
  if not isinstance(arr, np.ndarray) or arr.size == 0:
    return None

  mean_value  = np.mean(arr)
  sd_value    = np.std(arr)
  range_value = np.max(arr) - np.min(arr)

  return mean_value, sd_value, range_value



def plot_prior_predictives(simulator, model_name, n_sim_datasets=50):
    """
    Plots prior predictive distributions from a given simulator model.

    Parameters:
    -----------
    simulator : object
        A simulator object with a `.sample(n)` method that returns a dictionary
        with key 'x' containing the simulated datasets (shape: [n, trials]).

    model_name : str
        Name of the model to include in the plot title.

    n_sim_datasets : int, optional (default=50)
        Number of datasets to simulate and plot.

    Returns:
    --------
    fig : matplotlib.figure.Figure
        The matplotlib figure object containing the plot.

    axes : matplotlib.axes._subplots.AxesSubplot
        The axes object for further customization if needed.
    """
    fig, axes = plt.subplots(figsize=(10, 5))
    
    dataset_colors = sns.color_palette("plasma", n_sim_datasets).as_hex()
    data = simulator.sample(n_sim_datasets)['x']

    for dataset in range(n_sim_datasets):
        color = dataset_colors[dataset]
        sns.kdeplot(data[dataset, :].squeeze(), ax=axes, alpha=0.3, legend=False, color=color)

    axes.set_title(f"Prior Predictive {model_name} (for {n_sim_datasets} Participants)")

    return fig, axes