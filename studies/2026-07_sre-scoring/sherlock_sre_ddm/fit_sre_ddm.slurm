#!/bin/bash
#SBATCH --job-name=sre_ddm
#SBATCH --partition=normal
#SBATCH --time=16:00:00
#SBATCH --cpus-per-task=24
#SBATCH --mem=32G
#SBATCH --ntasks=1
#SBATCH --output=slurm-%j.out

module load R
export STAN_THREADS_PER_CHAIN=6

cd $SCRATCH/sre_ddm
Rscript run_fit.R
