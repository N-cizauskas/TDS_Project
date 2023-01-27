Folder structure:
- scripts: R scripts used for extracting and preparation
- docs: descriptions of field IDs, documents downloaded from the UK Biobank website
This folder must contain: "Data_Dictionary_Showcase.csv", "Codings.csv".

Step 1: Generate the UK Biobank dictionary with list of all available field IDs and some description
What to do: 

a) Write the path to UK Biobank dataset (csv format) in 1-generate_data_dict.sh
b) Submit script: 1-generate_data_dict.sh
Output(s): 
parameters/selection.xlsx

Step 2: Selection of field IDs to extract 

What to do: 
a) In parameters/selection.xlsx, manually fill in the columns "CodingName" (required), "FigureName" (optional) and "UnitInName" (optional). 
Fields with non-missing CodingName are extracted. The "UnitInName" is included in brackets in "FigureName" if provided.
b) Choose the required instances (i.e. time points) among available ones in column "InstanceRequired". By default, all instances are extracted.

c) Submit script: 2-extract_selected.sh

Output(s):
outputs/ukb_extracted.rds (intermediate dataset)
outputs/annot.rds (annotation file, with information on the selected fields and FigureName)
parameters/coding (folder containing txt files with data coding)

##### STOP HERE FOR NOW - Term 1

Step 3: Recoding for continuous/categorical variables for each instance and array
What to do:
a) In parameters/codings/, manually change the columns "RecodedValue" and "RecodedMeaning" if needed. 
Original categories can be re-named, grouped, dropped, re-ordered. 
To set some categories as missing values, write "NA" in both "RecodedValue" and "RecodedMeaning".
b) Optionally, continuous variables can be categorised by creating txt files starting with "codes_field_" followed by the corresponding field ID.
A template for such files is provided as "codes_template_continuous.txt".
Include as many rows as categories in this file, each category will be made of observations with values >= MinValue and < MaxValue.
If MinValue is left as NA, the observed minimum in the data will be used.
If MaxValue is left as NA, the observed maximum in the data will be used. 
c) Submit script: 3-recode_extracted.sh
Output(s):
outputs/ukb_recoded.rds (intermediate dataset)
parameters/parameters.xlsx

Step 4: Aggregating recoding over arrays
What to do:
a) In parameters/parameters.xlsx, manually define the "ArrayMethod" for all fields with multiple arrays.
Possible values are:
# 0: keep all arrays
# 1: take min over arrays
# 2: take max over arrays
# 3: take mean over arrays
# 4: take median over arrays
# 5: take first
# 6: take last
b) Submit script: 4-aggregate_arrays.sh 
Output(s):
parameters_myproject.zip (zip file containing folder parameters with all parameters used for extraction/preparation)
outputs/output_final.rds (final prepared dataset)

In brief:
The prepared dataset is "outputs/output_final.rds" and annotation with alternative field names is "outputs/annot.rds"