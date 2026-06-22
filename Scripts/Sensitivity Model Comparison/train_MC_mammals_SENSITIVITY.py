"""
BayesFlow: Personwise Model Comparison (Mammals) - Sensitivity Analysis
Converted from BF2_Personwise_MC_mammals_SENSITIVITY.ipynb for cluster execution.

Trains ONE network (given by --network_id), so it can be run as a
Slurm job array task (one task per network) instead of one long
sequential loop inside a notebook.

Author: David Izydorczyk (script conversion for HPC)
"""

import argparse
import os

# ---- CLI args -------------------------------------------------------------
parser = argparse.ArgumentParser()
parser.add_argument("--network_id", type=int, required=True,
                     help="Index of the network to train (was the loop variable 'network').")
parser.add_argument("--epochs", type=int, default=2)
parser.add_argument("--num_batches_per_epoch", type=int, default=2)
parser.add_argument("--batch_size", type=int, default=64)
parser.add_argument("--n_networks_total", type=int, default=10,
                     help="Only used for sanity-checking network_id range.")
parser.add_argument("--base_dir", type=str, default="..",
                     help="Project root that contains Materials/, Data/, Results/.")
args = parser.parse_args()

assert 0 <= args.network_id < args.n_networks_total, "network_id out of range"

# ---- Imports (after KERAS_BACKEND is set!) ---------------------------------
if "KERAS_BACKEND" not in os.environ:
    os.environ["KERAS_BACKEND"] = "jax"

import numpy as np
import numpy.random as rng
import pandas as pd
import keras
import bayesflow as bf
import statsmodels.formula.api as sm

import utils.helper_functions as fn
import utils.model_functions as mf

np.set_printoptions(suppress=True)

# ---- Paths (Linux-style, relative to base_dir) -----------------------------
MATERIALS_DIR = os.path.join(args.base_dir, "Materials")
DATA_DIR      = os.path.join(args.base_dir, "Data")
RESULTS_DIR   = os.path.join(args.base_dir, "Results", "Sensitivity Model Comparison")
NETWORKS_DIR  = os.path.join(RESULTS_DIR, "Trained Networks")

os.makedirs(NETWORKS_DIR, exist_ok=True)
os.makedirs(RESULTS_DIR, exist_ok=True)

design_csv = os.path.join(MATERIALS_DIR, "design_data_mammals.csv")
data_csv   = os.path.join(DATA_DIR, "data_analysis_mammals.csv")

# ---- 1. Load design data ----------------------------------------------------
df = pd.read_csv(design_csv, sep=";", decimal=",")

all_cues = df[[f"dim_{i}" for i in range(1, 11)]].to_numpy(dtype=float)
all_crit = df[["crit"]].to_numpy(dtype=float).flatten()

exemplars = df.loc[df["training"] == 1, :]
ex_cues   = exemplars[[f"dim_{i}" for i in range(1, 11)]].to_numpy(dtype=float)
ex_crit   = exemplars[["crit"]].to_numpy(dtype=float).flatten()
ex_IDs    = exemplars[["ID"]].to_numpy(dtype=float).squeeze().astype(int)

testing  = df.loc[df["training"] == 0, :]
test_IDs = testing[["ID"]].to_numpy(dtype=float).squeeze().astype(int)
cues     = testing[[f"dim_{i}" for i in range(1, 11)]].to_numpy(dtype=float)
dict_cues = {f"cue_{i}": cues[:, i] for i in range(cues.shape[1])}

n_trials, n_dim = cues.shape
rate = 0.01
position_encodings = np.linspace(0, 1, n_trials, dtype=np.float32)

# ---- 2. Define models -------------------------------------------------------
# 2.1 CAM
result = sm.ols(
    formula="crit ~ " + " + ".join(f"dim_{i}" for i in range(1, 11)),
    data=df,
).fit()

def prior_CAM(n_dim=n_dim, rate=rate):
    w = np.zeros(n_dim + 1)
    w[0] = rng.normal(984.65, 300)
    w[1:] = rng.normal(0, 750, size=n_dim)
    sigma = rng.exponential(1 / rate)
    return dict(w=w, sigma=sigma)

def model_CAM(w, sigma, cues=cues, p=position_encodings):
    n_trials, _ = cues.shape
    pred_crit = mf.CAM_experiment(w, cues)
    x = fn.truncnorm_r(mean=pred_crit, sd=sigma, low=0, upp=10000, size=n_trials)
    return dict(x=x, p=p)

simulator_CAM = bf.make_simulator([prior_CAM, model_CAM])

# 2.2 GCM
def prior_GCM(n_dim=n_dim, rate=rate):
    c = rng.exponential(1 / 0.1)
    w = rng.dirichlet(np.ones(n_dim), size=1) * n_dim
    sigma = rng.exponential(1 / rate)
    return dict(c=c, w=w.squeeze(), sigma=sigma)

def model_GCM(c, w, sigma, cues=cues, ex_cues=ex_cues, ex_crit=ex_crit, p=position_encodings):
    n_trials = cues.shape[0]
    pred_crit = mf.GCM_experiment(cues, ex_cues, ex_crit, w, c)
    x = fn.truncnorm_r(mean=pred_crit, sd=sigma, low=0, upp=10000, size=n_trials)
    return dict(x=x, p=p)

simulator_GCM = bf.make_simulator([prior_GCM, model_GCM])

# 2.3 RulExJ
def prior_RULEXJ(n_dim=n_dim, rate=rate):
    CAM_pars = prior_CAM(n_dim=n_dim, rate=rate)
    GCM_pars = prior_GCM(n_dim=n_dim, rate=rate)
    a = rng.uniform(0, 1)
    return dict(alpha=a, w_CAM=CAM_pars["w"], c=GCM_pars["c"], w_GCM=GCM_pars["w"], sigma=CAM_pars["sigma"])

def model_RULEXJ(alpha, w_CAM, c, w_GCM, sigma, cues=cues, ex_cues=ex_cues, ex_crit=ex_crit, p=position_encodings):
    n_trials, _ = cues.shape
    pred_CAM = mf.CAM_experiment(w_CAM, cues)
    pred_GCM = mf.GCM_experiment(cues, ex_cues, ex_crit, w_GCM, c)
    pred_RULEXJ = alpha * pred_CAM + (1 - alpha) * pred_GCM
    x = fn.truncnorm_r(mean=pred_RULEXJ, sd=sigma, low=0, upp=10000, size=n_trials)
    return dict(x=x, p=p)

simulator_RULEXJ = bf.make_simulator([prior_RULEXJ, model_RULEXJ])

# 2.4 Mapping model (MAPP)
all_mapp_cues  = mf.preprocess_cues(all_cues, all_crit)
ex_mapp_cues   = all_mapp_cues[ex_IDs - 1, :]
mapp_cues      = all_mapp_cues[test_IDs - 1, :]
dict_mapp_cues = {f"cue_{i}": mapp_cues[:, i] for i in range(mapp_cues.shape[1])}

def prior_MAPP(lower=2, upper=12, rate=rate):
    n_cats = fn.truncated_poisson_np(5, lower=lower, upper=upper)
    sigma = rng.exponential(1 / rate)
    return dict(n_cats=n_cats[0], sigma=sigma)

def model_MAPP(n_cats, sigma, cues=mapp_cues, ex_cues=ex_mapp_cues, ex_crit=ex_crit, p=position_encodings):
    n_trials, _ = cues.shape
    pred_crit = mf.MAPP_experiment(n_cats, cues, ex_cues, ex_crit)
    x = fn.truncnorm_r(mean=pred_crit, sd=sigma, low=0, upp=10000, size=n_trials)
    return dict(x=x, p=p)

simulator_MAPP = bf.make_simulator([prior_MAPP, model_MAPP])

# 2.5 QuickEst
all_QEst_cues  = mf.preprocess_cues_QuickEst(all_cues, all_crit)
ex_QEst_cues   = all_QEst_cues[ex_IDs - 1, :]
QEst_cues      = all_QEst_cues[test_IDs - 1, :]
dict_QEst_cues = {f"cue_{i}": QEst_cues[:, i] for i in range(QEst_cues.shape[1])}

def prior_QEst():
    sigma = fn.truncated_cauchy_np(loc=0, scale=100, lower=0.001, upper=1000, size=1)
    return dict(sigma=sigma)

def model_QEst(sigma, cues=QEst_cues, ex_cues=ex_QEst_cues, ex_crit=ex_crit, p=position_encodings):
    mem_ex_crit = np.random.normal(ex_crit, sigma)
    x = mf.QuickEst_experiment(cues, ex_cues, mem_ex_crit)
    return dict(x=np.clip(x, 0, 10000), p=p)

simulator_QEst = bf.make_simulator([prior_QEst, model_QEst])

# 2.6 RGuess
def prior_RGuess():
    sigma_range = rng.exponential(1 / 0.01, size=1)
    return dict(sigma_range=sigma_range)

def model_RGuess(sigma_range, cues=cues, ex_crit=ex_crit, p=position_encodings):
    n_trials, _ = cues.shape
    min_observed_crit = rng.normal(np.min(ex_crit), sigma_range)
    range_width = max(0.01, rng.normal(np.max(ex_crit) - np.min(ex_crit), sigma_range))
    max_observed_crit = min_observed_crit + abs(range_width)
    x = rng.uniform(min_observed_crit, max_observed_crit, size=n_trials)
    return dict(x=np.clip(x, 0, 10000), p=p)

simulator_RGuess = bf.make_simulator([prior_RGuess, model_RGuess])

# ---- 3. Training data --------------------------------------------------
df_data = pd.read_csv(data_csv, sep=",")
data = df_data.to_numpy()
data = np.float32(data[(test_IDs - 1), 5:])
n_trials, n_sub = data.shape
n_models = 6

simulator = bf.simulators.ModelComparisonSimulator(
    simulators=[simulator_RULEXJ, simulator_CAM, simulator_GCM, simulator_MAPP, simulator_QEst, simulator_RGuess],
    use_mixed_batches=True,
)

adapter = (
    bf.adapters.Adapter()
    .convert_dtype("float64", "float32")
    .as_time_series(["x", "p"])
    .standardize("x")
    .drop(["alpha", "c", "sigma", "w_GCM", "w_CAM", "n_cats", "sigma_range"])
    .concatenate(["x", "p"], into="summary_variables")
)

summary_network = bf.networks.TimeSeriesNetwork(summary_dim=20)
classifier_network = bf.networks.MLP(widths=[256] * 32, activation="silu", dropout=None)

learning_rate = keras.optimizers.schedules.CosineDecay(
    1e-4, decay_steps=args.epochs * args.num_batches_per_epoch
)
optimizer = keras.optimizers.Adam(learning_rate=learning_rate)

# ---- 4. Train ONE network (this array task's job) --------------------------
network = args.network_id
print(f"Starting to train network {network} using ...", flush=True)

approximator = bf.approximators.ModelComparisonApproximator(
    num_models=6,
    classifier_network=classifier_network,
    summary_network=summary_network,
    adapter=adapter,
)
approximator.compile(optimizer=optimizer)

history = approximator.fit(
    epochs=args.epochs,
    batch_size=args.batch_size,
    num_batches=args.num_batches_per_epoch,
    adapter=adapter,
    simulator=simulator,
)

model_path = os.path.join(NETWORKS_DIR, f"sensitivity_MC_MAMMALS_{network}.keras")
approximator.save(model_path)
print(f"Finished training network {network}, saved to {model_path}", flush=True)

all_pmps = np.zeros((n_sub, n_models))
for i in range(n_sub):
    pmp = approximator.predict(conditions=dict(x=data[:, i], p=position_encodings))[0]
    all_pmps[i] = pmp.flatten()

all_pmps_df = pd.DataFrame(all_pmps)
all_pmps_df.columns = ["RULEXJ", "CAM", "GCM", "MAPP", "QEst", "RGuess"]

csv_path = os.path.join(RESULTS_DIR, f"pmp_MAMMALS_{network}.csv")
all_pmps_df.to_csv(csv_path)
print(f"Finished fitting network {network} to participants, saved to {csv_path}", flush=True)
