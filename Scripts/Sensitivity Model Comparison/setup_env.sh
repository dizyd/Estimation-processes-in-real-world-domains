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
# Check https://docs.jax.dev/en/latest/installation.html for the wheel matching the
# cluster's CUDA version (`nvidia-smi` on a GPU node, or `module avail` for a cuda
# module / ask HPC support which CUDA version the gpu_h100 / gpu_a100_il nodes expose).
pip install --upgrade "jax[cuda12]==0.6.1"

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
