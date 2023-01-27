Objective: definition of incident/prevalent case/control status and extraction of dates of diagnosis
and time from recruitment to diagnosis based on HES, death registry and additional field IDs

.sh script to submit: submit_health_outcome_definition.sh

Needs access to four scripts:
- extract_hes.R (scanning through HES diagnoses and operations)
- extract_death.R (scanning through death registry for cause and date of death)
- extract_baseline.R (scanning through additional field IDs specified by the user)
- functions.R (functions used within the three scripts mentioned above)

Needs access to six datasets:
- hesin.txt (main HES dataset containing dates of admission/discharge, etc)
- hesin_diag.txt (diagnoses from HES)
- hesin_oper.txt (operations and procedures from HES)
- death.txt (dates of death)
- death_cause.txt (causes of death)
- ukbXXX.csv (main dataset from UK Biobank application with all requested field IDs)

Needs access to txt files for parametrisation:
- diagnoses_icd10.txt (optional ICD-10 codes of disease of interest, used to scan through HES diagnoses and death causes)
- diagnoses_icd9.txt (optional ICD-9 codes of disease of interest, used to scan through HES diagnoses)
- operations_opcs4.txt (optional OPCS-4 codes, used to scan through HES operations and procedures)
- codes_XXX.txt (optional files used to scan through field ID XXX in main UK Biobank dataset)
