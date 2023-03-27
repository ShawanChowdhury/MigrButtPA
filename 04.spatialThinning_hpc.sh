#!/bin/bash
 
#SBATCH --job-name=04.spatialThinning
#SBATCH --chdir=/work/chowdhus
#SBATCH --output=/work/%u/%x-%j.log
#SBATCH --time=30-0:00:00

#SBATCH --mem-per-cpu=12G

module load foss/2020b R/4.0.4-2 

# get list of input files from first argument
input_files="$1"

# get the specific input file for this task
# this is the n-th (where n is current task ID) line of the file
species_csv=$(awk "NR==$SLURM_ARRAY_TASK_ID" "$input_files")

mkdir -p data/thinned

Rscript --vanilla /home/$USER/04.spatialThinning_hpc.R "$species_csv"