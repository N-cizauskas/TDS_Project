#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N aggregating


cd /rds/general/project/hda-22-23/live/TDS/General/extraction_and_recoding/scripts

module load anaconda3/personal
source activate r413

Rscript 4-aggregate_arrays.R

# Creating zip file with parameters used for this project
cd ../
project_name=myproject
cp -r parameters parameters_$project_name
zip -r parameters_$project_name.zip parameters_$project_name
rm -rf parameters_$project_name
