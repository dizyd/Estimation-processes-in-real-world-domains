#!/bin/bash
#SBATCH --job-name=sens_mc_mammals
#SBATCH --partition=gpu_a100_il       
#SBATCH --gres=gpu:1		
#SBATCH --ntasks=1
#SBATCH --nodes=1  				# compute nodes, for most jobs one is enough
#SBATCH --cpus-per-task=8
#SBATCH --time=3:00:00                # stay safely under any 4h-type limit
#SBATCH --array=0-9                   # one task per network (n_networks_total=10)
#SBATCH --output=logs/train_%A_%a.out
#SBATCH --error=logs/train_%A_%a.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=izydorczyk@uni-mannheim.de

set -euo pipefail

mkdir -p logs

# --- Environment -------------------------------------------------------
module purge
module load devel/python/3.11    # adjust to whatever `module avail python` shows on bwUniCluster 3.0
# If you use CUDA/cuDNN modules explicitly (rather than pip-installed jax[cuda] wheels), load them here, e.g.:
# module load devel/cuda/12.x

source "${HOME}/envs/personwise_BF/bin/activate"

export KERAS_BACKEND=jax
# Keep JAX/XLA from grabbing more than the GPU assigned by Slurm:
export CUDA_VISIBLE_DEVICES=${CUDA_VISIBLE_DEVICES:-0}

cd "${SLURM_SUBMIT_DIR}/Scripts/Sensitivity Model Comparison"

echo "Running network id ${SLURM_ARRAY_TASK_ID} on $(hostname), GPU(s): ${CUDA_VISIBLE_DEVICES}"

python train_MC_mammals_SENSITIVITY.py \
    --network_id "${SLURM_ARRAY_TASK_ID}" \
    --n_networks_total 10 \
    --epochs 50 \
    --num_batches_per_epoch 512 \
    --batch_size 64 \
    --base_dir "../.."

deactivate