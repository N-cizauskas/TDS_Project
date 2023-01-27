#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N recoding


cd /rds/general/project/hda-22-23/live/TDS/General/extraction_and_recoding/scripts

module load anaconda3/personal
source activate r413

Rscript 3-recode_variables.R

