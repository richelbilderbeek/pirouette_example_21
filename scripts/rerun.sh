#!/bin/bash
#
# Re-run the code locally, to re-create the data and figure.
#
# Usage:
#
#   ./scripts/rerun.sh
#
#SBATCH --partition=gelifes
#SBATCH --time=96:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --job-name=pirex21
#SBATCH --output=example_21.log
#
rm -rf example_21
rm *.png
time Rscript example_21.R
zip -r pirouette_example_21.zip example_21 example_21.R scripts *.png

