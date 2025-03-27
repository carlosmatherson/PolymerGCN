#!/bin/bash
#SBATCH --job-name=ai4polymers_optimization
#SBATCH --account=HYDROSM
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --time=24:00:00
#SBATCH --output=TgGCN_optimization.out
#SBATCH --error=TgGCN_optimization.error

cd /lcrc/project/hydrosm/ai4polymers/
source activate ./.env
export CUBLAS_WORKSPACE_CONFIG=:4096:8
python ./run_optimization.py -t tg -rp ./data -o ./results/tuning_tg -v True -rs 9700
