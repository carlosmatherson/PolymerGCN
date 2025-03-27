#!/bin/bash
#SBATCH --job-name=tg
#SBATCH --account=HYDROSM
#SBATCH --nodes=1
#SBATCH --gres=gpu:1
#SBATCH --time=1:00:00
#SBATCH --output=tg.out
#SBATCH --error=tg.error

cd /lcrc/project/hydrosm/ai4polymers/
source activate ./.env
export CUBLAS_WORKSPACE_CONFIG=:16:8
python ./run_experiment.py -t tg -rp ./data -o ./results/tg_tg_hyperparams2 -v True -lr 8.41957131e-04 -wd 1.0e-05 -bs 256 -c 9 -cd 128 -f 7 -fd 128
