#!/bin/bash
#SBATCH -J nfbuffers_sgfcm # job name (I was not always good about updating this)
#SBATCH -o log_slurm.o%j # output and error file name (%j expands to jobID)
#SBATCH -n 48            # total number of tasks requested
#SBATCH -N 1             # number of nodes you want to run on
#SBATCH -p short        # queue (partition)
#SBATCH -t 168:00:00      # run time (hh:mm:ss)

# (optional) Print some debugging information
echo "Date              = $(date)"
echo "Slurm Job Name    = $SLURM_JOB_NAME"
echo "Hostname          = $(hostname -s)"
echo "Working Directory = $(pwd)"
echo ""
echo "Number of Nodes Allocated  = $SLURM_JOB_NUM_NODES"
echo "Number of Tasks Allocated  = $SLURM_NTASKS"
echo "Job Partition              =  $SLURM_PARTITION"
echo ""

# Load the module
module load slurm
#module load borah-misc r/4.2.2 # comment this out if youare using a mamba/conda environment
export TMPDIR=/local

# Or activate your mamba environment
. ~/.bashrc
mamba activate r-env

# Script to run
Rscript nf_level_sgfcm_parameter_selection.R
