#!/bin/bash
# Run ONCE on a login node (or inside an interactive salloc session) to build the venv.
# Do NOT run heavy installs on a login node for long -- this is light enough (pip installs only),
# but if unsure, request a short interactive session first:
#   salloc --partition=dev_cpu --ntasks=1 --time=30 --mem=4000

set -euo pipefail

module purge
module load devel/python/3.11     # check with `module avail python` -- exact name/version may differ

mkdir -p "${HOME}/envs"
python3 -m venv "${HOME}/envs/personwise_BF"
source "${HOME}/envs/personwise_BF/bin/activate"

pip install --upgrade pip

# NOTE: versions below are pinned to match Scripts/requirements.txt (the working
# Windows environment) so the cluster reproduces the same results. requirements.txt
# itself is a Windows freeze (pywin32, CPU-only jax, ...) and must NOT be installed
# verbatim on the Linux GPU nodes -- hence the curated list here.

# JAX with CUDA support (replaces the CPU jax==0.6.1 in requirements.txt).
# NOTE: we intentionally do NOT pin to 0.6.1 here. The bwUniCluster H100 nodes run a
# CUDA 13.x driver (check with `nvidia-smi` -> top-right "CUDA Version"), and the
# CUDA-12 jaxlib 0.6.1 wheel fails at runtime with
#   "gpusolverDnCreate(&handle) failed: cuSolver internal error".
# Installing the cuda13 build to match the driver fixes it. If a future node has a
# CUDA 12.x driver, use "jax[cuda12]" instead. See
# https://docs.jax.dev/en/latest/installation.html
pip install --upgrade "jax[cuda13]"

# BayesFlow: pinned to the exact git commit used in the project. The training script
# relies on the BayesFlow 2 dev API (bf.make_simulator, bf.adapters.Adapter,
# ModelComparisonApproximator), which the PyPI release does NOT provide.
pip install "bayesflow @ git+https://github.com/bayesflow-org/bayesflow.git@735969c29ef19bbc81d545c9b1a9643074eaec9b"

# Remaining dependencies, pinned to match requirements.txt.
pip install \
    "keras==3.10.0" \
    "numpy==1.26.4" \
    "pandas==2.2.3" \
    "scipy==1.15.3" \
    "statsmodels==0.14.4" \
    "seaborn==0.13.2" \
    "matplotlib==3.10.3" \
    "plotnine==0.14.5"

echo "Done. Activate with: source ${HOME}/envs/personwise_BF/bin/activate"
