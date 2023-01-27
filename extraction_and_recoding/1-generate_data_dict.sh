#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N dict

cd /rds/general/user/dds122/projects/hda-22-23/live/TDS/Group8/extraction_and_recoding/scripts

module load anaconda3/personal
source activate r413

ukb_path=/rds/general/project/hda-22-23/live/TDS/General/Data/ukb669759.csv

Rscript 1-make_data_dict.R $ukb_path
