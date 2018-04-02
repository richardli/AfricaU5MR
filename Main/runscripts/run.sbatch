#!/bin/bash
#SBATCH --job-name IGME      # Set a name for your job. This is especially useful if you have multiple jobs queued.
#SBATCH --partition short     # Slurm partition to use
#SBATCH --ntasks 1          # Number of tasks to run. By default, one CPU core will be allocated per task
#SBATCH --time 0-10:00        # Wall time limit in D-HH:MM
#SBATCH --mem-per-cpu=4000     # Memory limit for each tasks (in MB)
#SBATCH -o out/Rt_%j.out    # File to which STDOUT will be written
#SBATCH -e out/Rt_%j.err    # File to which STDERR will be written

module load R
Rscript run.R $SLURM_ARRAY_TASK_ID > log/+C$SLURM_ARRAY_TASK_ID

# sbatch --array=1-35 run.sbatch
# sbatch --array=32-35 run.sbatch
# sbatch --array=1 run.sbatch
# sbatch --array=31 run.sbatch
# Angola (array=1) requires larger memory
# Tanzania (array=31) requires larger memory
