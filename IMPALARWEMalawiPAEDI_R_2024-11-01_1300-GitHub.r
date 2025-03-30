##### RWE STUDY - St. Luke's pediatric ward database

### Load libraries #############################################################
# Load libraries
# use install.packages("R package name") in case you do not have a package previously installed on your computer
library(haven)
library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(mice)
library(SuperLearner)
library(boot)
library(ggpointdensity)
library(glmnet)
library(parallel)
library(cem)
library(systemfit)

### Pediatric ward data cleaning ###############################################
### Labeling data ##############################################################
# Clear existing data and graphics
rm(list=ls())
graphics.off()

# Load libraries
library(Hmisc)
library(readr)

# Import data
data <- read_csv("RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300.csv")

# Setting labels
label(data$rwe_pw_study_id)="1. Study ID"
label(data$rwe_pw_name_hosp)="2. Name of hospital:"
label(data$rwe_pw_name_staff_initial)="2a. Staff initial:"
label(data$rwe_pw_name_date_extracted)="2b. Date extracted:"
label(data$rwe_pw_admit_date_hosp)="3. Admission date in hospital:"
label(data$rwe_pw_admit_date_nicu_hdu)="4. Admission date in NICU/HDU:"
label(data$rwe_pw_admit_age___1)="5. Age units at admission (Check all that apply): (choice=Days)"
label(data$rwe_pw_admit_age___2)="5. Age units at admission (Check all that apply): (choice=Weeks)"
label(data$rwe_pw_admit_age___3)="5. Age units at admission (Check all that apply): (choice=Months)"
label(data$rwe_pw_admit_age___4)="5. Age units at admission (Check all that apply): (choice=Years)"
label(data$rwe_pw_admit_age_days)="5a. Age at admission (days portion):"
label(data$rwe_pw_admit_age_weeks)="5b. Age at admission (weeks portion):"
label(data$rwe_pw_admit_age_months)="5c. Age at admission (months portion):"
label(data$rwe_pw_admit_age_years)="5d. Age at admission (years portion):"
label(data$rwe_pw_calc_admit_age)="6. Overall calculated age at admission in months:"
label(data$rwe_pw_sex_parent)="7. Sex of child:"
label(data$rwe_pw_preg)="7b. Is parent/guardian pregnant?"
label(data$rwe_pw_hiv_status)="8. HIV status of child:"
label(data$rwe_pw_hiv_child)="8a. Was HIV test of child done?"
label(data$rwe_pw_hiv_status_2)="8b. HIV status of child:"
label(data$rwe_pw_admit_diagnosis___1)="9. Admission diagnosis (Check all that apply): (choice=Malaria)"
label(data$rwe_pw_admit_diagnosis___2)="9. Admission diagnosis (Check all that apply): (choice=URTI)"
label(data$rwe_pw_admit_diagnosis___3)="9. Admission diagnosis (Check all that apply): (choice=Fracture)"
label(data$rwe_pw_admit_diagnosis___4)="9. Admission diagnosis (Check all that apply): (choice=Wound)"
label(data$rwe_pw_admit_diagnosis___5)="9. Admission diagnosis (Check all that apply): (choice=Sepsis)"
label(data$rwe_pw_admit_diagnosis___6)="9. Admission diagnosis (Check all that apply): (choice=Anaemia)"
label(data$rwe_pw_admit_diagnosis___7)="9. Admission diagnosis (Check all that apply): (choice=Pneumonia)"
label(data$rwe_pw_admit_diagnosis___8)="9. Admission diagnosis (Check all that apply): (choice=Diarrhoea)"
label(data$rwe_pw_admit_diagnosis___9)="9. Admission diagnosis (Check all that apply): (choice=Bronchitis)"
label(data$rwe_pw_admit_diagnosis___10)="9. Admission diagnosis (Check all that apply): (choice=ARI)"
label(data$rwe_pw_admit_diagnosis___11)="9. Admission diagnosis (Check all that apply): (choice=Asthma)"
label(data$rwe_pw_admit_diagnosis___12)="9. Admission diagnosis (Check all that apply): (choice=Malnutrition)"
label(data$rwe_pw_admit_diagnosis___13)="9. Admission diagnosis (Check all that apply): (choice=Other)"
label(data$rwe_pw_admit_diagnosis___14)="9. Admission diagnosis (Check all that apply): (choice=Epilepsy)"
label(data$rwe_pw_admit_diagnosis___15)="9. Admission diagnosis (Check all that apply): (choice=Meningitis)"
label(data$rwe_pw_admit_other_diagno)="9b. Other, specify:"
label(data$rwe_pw_any_tests)="10. Apart from HIV, was any test done?"
label(data$rwe_pw_tests_done___1)="10b. Tests done (Check all that apply): (choice=PVC)"
label(data$rwe_pw_tests_done___2)="10b. Tests done (Check all that apply): (choice=Malaria microscopy)"
label(data$rwe_pw_tests_done___3)="10b. Tests done (Check all that apply): (choice=Malaria RDT)"
label(data$rwe_pw_tests_done___4)="10b. Tests done (Check all that apply): (choice=FBC)"
label(data$rwe_pw_tests_done___5)="10b. Tests done (Check all that apply): (choice=Blood glucose)"
label(data$rwe_pw_tests_done___6)="10b. Tests done (Check all that apply): (choice=Blood culture)"
label(data$rwe_pw_tests_done___7)="10b. Tests done (Check all that apply): (choice=Other)"
label(data$rwe_pw_tests_other)="10c. Other, specify:"
label(data$rwe_pw_treat_admit_received)="11. Was treatment received at admission?"
label(data$rwe_pw_admit_treatment___1)="11b. Treatment received at admission (Check all that apply): (choice=Acyclovir)"
label(data$rwe_pw_admit_treatment___2)="11b. Treatment received at admission (Check all that apply): (choice=Adrenaline)"
label(data$rwe_pw_admit_treatment___3)="11b. Treatment received at admission (Check all that apply): (choice=Albendazole)"
label(data$rwe_pw_admit_treatment___4)="11b. Treatment received at admission (Check all that apply): (choice=Amikacin)"
label(data$rwe_pw_admit_treatment___5)="11b. Treatment received at admission (Check all that apply): (choice=Aminophylline)"
label(data$rwe_pw_admit_treatment___6)="11b. Treatment received at admission (Check all that apply): (choice=Amitriptyline)"
label(data$rwe_pw_admit_treatment___7)="11b. Treatment received at admission (Check all that apply): (choice=Amoxicillin)"
label(data$rwe_pw_admit_treatment___8)="11b. Treatment received at admission (Check all that apply): (choice=Amoxicillin+Clavulanic acid)"
label(data$rwe_pw_admit_treatment___9)="11b. Treatment received at admission (Check all that apply): (choice=Artesunate)"
label(data$rwe_pw_admit_treatment___10)="11b. Treatment received at admission (Check all that apply): (choice=Atenolol)"
label(data$rwe_pw_admit_treatment___11)="11b. Treatment received at admission (Check all that apply): (choice=Atropine)"
label(data$rwe_pw_admit_treatment___12)="11b. Treatment received at admission (Check all that apply): (choice=Azithromycin)"
label(data$rwe_pw_admit_treatment___13)="11b. Treatment received at admission (Check all that apply): (choice=Baclofen)"
label(data$rwe_pw_admit_treatment___14)="11b. Treatment received at admission (Check all that apply): (choice=Benzathine penicillin)"
label(data$rwe_pw_admit_treatment___15)="11b. Treatment received at admission (Check all that apply): (choice=Benzylpenicillin)"
label(data$rwe_pw_admit_treatment___16)="11b. Treatment received at admission (Check all that apply): (choice=Calcium gluconate)"
label(data$rwe_pw_admit_treatment___17)="11b. Treatment received at admission (Check all that apply): (choice=Captopril)"
label(data$rwe_pw_admit_treatment___18)="11b. Treatment received at admission (Check all that apply): (choice=Carbamazepin)"
label(data$rwe_pw_admit_treatment___19)="11b. Treatment received at admission (Check all that apply): (choice=Ceftriaxone)"
label(data$rwe_pw_admit_treatment___20)="11b. Treatment received at admission (Check all that apply): (choice=Chlorphenamine)"
label(data$rwe_pw_admit_treatment___21)="11b. Treatment received at admission (Check all that apply): (choice=Ciprofloxacine)"
label(data$rwe_pw_admit_treatment___22)="11b. Treatment received at admission (Check all that apply): (choice=Codeine phosphate)"
label(data$rwe_pw_admit_treatment___23)="11b. Treatment received at admission (Check all that apply): (choice=Cotrimoxazole (Trimethoprim/ sulphamethoxazole))"
label(data$rwe_pw_admit_treatment___24)="11b. Treatment received at admission (Check all that apply): (choice=Dexamethasone)"
label(data$rwe_pw_admit_treatment___25)="11b. Treatment received at admission (Check all that apply): (choice=Diazepam)"
label(data$rwe_pw_admit_treatment___26)="11b. Treatment received at admission (Check all that apply): (choice=Digoxin (no digitalization)"
label(data$rwe_pw_admit_treatment___27)="11b. Treatment received at admission (Check all that apply): (choice=Enalapril)"
label(data$rwe_pw_admit_treatment___28)="11b. Treatment received at admission (Check all that apply): (choice=Ferrous Sulphate)"
label(data$rwe_pw_admit_treatment___29)="11b. Treatment received at admission (Check all that apply): (choice=FFP)"
label(data$rwe_pw_admit_treatment___30)="11b. Treatment received at admission (Check all that apply): (choice=Flucloxacillin)"
label(data$rwe_pw_admit_treatment___31)="11b. Treatment received at admission (Check all that apply): (choice=Fluconazole)"
label(data$rwe_pw_admit_treatment___32)="11b. Treatment received at admission (Check all that apply): (choice=Folic acid)"
label(data$rwe_pw_admit_treatment___33)="11b. Treatment received at admission (Check all that apply): (choice=Furosemide)"
label(data$rwe_pw_admit_treatment___34)="11b. Treatment received at admission (Check all that apply): (choice=Gentamicin)"
label(data$rwe_pw_admit_treatment___35)="11b. Treatment received at admission (Check all that apply): (choice=Glucose 10% and above)"
label(data$rwe_pw_admit_treatment___36)="11b. Treatment received at admission (Check all that apply): (choice=Griseofulvin)"
label(data$rwe_pw_admit_treatment___37)="11b. Treatment received at admission (Check all that apply): (choice=Hydralazine)"
label(data$rwe_pw_admit_treatment___38)="11b. Treatment received at admission (Check all that apply): (choice=Hydrochlorthiazide)"
label(data$rwe_pw_admit_treatment___39)="11b. Treatment received at admission (Check all that apply): (choice=Hydrocortisone)"
label(data$rwe_pw_admit_treatment___40)="11b. Treatment received at admission (Check all that apply): (choice=Ibuprofen)"
label(data$rwe_pw_admit_treatment___41)="11b. Treatment received at admission (Check all that apply): (choice=Insulin)"
label(data$rwe_pw_admit_treatment___42)="11b. Treatment received at admission (Check all that apply): (choice=Isoniazid)"
label(data$rwe_pw_admit_treatment___43)="11b. Treatment received at admission (Check all that apply): (choice=Ketamin)"
label(data$rwe_pw_admit_treatment___44)="11b. Treatment received at admission (Check all that apply): (choice=Ketoconazole)"
label(data$rwe_pw_admit_treatment___45)="11b. Treatment received at admission (Check all that apply): (choice=Lisinopril)"
label(data$rwe_pw_admit_treatment___46)="11b. Treatment received at admission (Check all that apply): (choice=Lumefantrine-Artemether (LA))"
label(data$rwe_pw_admit_treatment___47)="11b. Treatment received at admission (Check all that apply): (choice=Magnesium-sulfate)"
label(data$rwe_pw_admit_treatment___48)="11b. Treatment received at admission (Check all that apply): (choice=Mebendazole)"
label(data$rwe_pw_admit_treatment___49)="11b. Treatment received at admission (Check all that apply): (choice=Metronidazole)"
label(data$rwe_pw_admit_treatment___50)="11b. Treatment received at admission (Check all that apply): (choice=Morphine)"
label(data$rwe_pw_admit_treatment___51)="11b. Treatment received at admission (Check all that apply): (choice=NaCl 0.9% / Normal Saline)"
label(data$rwe_pw_admit_treatment___52)="11b. Treatment received at admission (Check all that apply): (choice=Nevirapine)"
label(data$rwe_pw_admit_treatment___53)="11b. Treatment received at admission (Check all that apply): (choice=Nifedipine)"
label(data$rwe_pw_admit_treatment___54)="11b. Treatment received at admission (Check all that apply): (choice=Nystatin)"
label(data$rwe_pw_admit_treatment___55)="11b. Treatment received at admission (Check all that apply): (choice=Omeprazole)"
label(data$rwe_pw_admit_treatment___56)="11b. Treatment received at admission (Check all that apply): (choice=ORS)"
label(data$rwe_pw_admit_treatment___57)="11b. Treatment received at admission (Check all that apply): (choice=Packed Cells)"
label(data$rwe_pw_admit_treatment___58)="11b. Treatment received at admission (Check all that apply): (choice=Paracetamol)"
label(data$rwe_pw_admit_treatment___59)="11b. Treatment received at admission (Check all that apply): (choice=Paraldehyde)"
label(data$rwe_pw_admit_treatment___60)="11b. Treatment received at admission (Check all that apply): (choice=Pethidine)"
label(data$rwe_pw_admit_treatment___61)="11b. Treatment received at admission (Check all that apply): (choice=Phenobarbitone)"
label(data$rwe_pw_admit_treatment___62)="11b. Treatment received at admission (Check all that apply): (choice=Phenytoin)"
label(data$rwe_pw_admit_treatment___63)="11b. Treatment received at admission (Check all that apply): (choice=Platelets)"
label(data$rwe_pw_admit_treatment___64)="11b. Treatment received at admission (Check all that apply): (choice=Potassium (Slow K))"
label(data$rwe_pw_admit_treatment___65)="11b. Treatment received at admission (Check all that apply): (choice=Praziquantel)"
label(data$rwe_pw_admit_treatment___66)="11b. Treatment received at admission (Check all that apply): (choice=Prednisolone)"
label(data$rwe_pw_admit_treatment___67)="11b. Treatment received at admission (Check all that apply): (choice=Promethazine)"
label(data$rwe_pw_admit_treatment___68)="11b. Treatment received at admission (Check all that apply): (choice=Propranolol)"
label(data$rwe_pw_admit_treatment___69)="11b. Treatment received at admission (Check all that apply): (choice=Ringers lactate)"
label(data$rwe_pw_admit_treatment___70)="11b. Treatment received at admission (Check all that apply): (choice=Salbutamol)"
label(data$rwe_pw_admit_treatment___71)="11b. Treatment received at admission (Check all that apply): (choice=Spironolactone)"
label(data$rwe_pw_admit_treatment___72)="11b. Treatment received at admission (Check all that apply): (choice=Sulfadoxine/ Pyrimethamine (Fansidar))"
label(data$rwe_pw_admit_treatment___73)="11b. Treatment received at admission (Check all that apply): (choice=Valproate (Sodium))"
label(data$rwe_pw_admit_treatment___74)="11b. Treatment received at admission (Check all that apply): (choice=Vitamin A)"
label(data$rwe_pw_admit_treatment___75)="11b. Treatment received at admission (Check all that apply): (choice=Vitamin K)"
label(data$rwe_pw_admit_treatment___76)="11b. Treatment received at admission (Check all that apply): (choice=Whole Blood)"
label(data$rwe_pw_admit_treatment___77)="11b. Treatment received at admission (Check all that apply): (choice=X-Penicillin)"
label(data$rwe_pw_admit_treatment___78)="11b. Treatment received at admission (Check all that apply): (choice=Zinc)"
label(data$rwe_pw_admit_treatment___98)="11b. Treatment received at admission (Check all that apply): (choice=Other drugs)"
label(data$rwe_pw_treatment_other)="11c. Other, specify:"
label(data$rwe_pw_disch_date_nicu_hdu)="12. Discharge date from NICU/HDU:"
label(data$rwe_pw_outcome_disch_hosp)="13. Outcome of admission:"
label(data$rwe_pw_disch_date_hosp)="13b. Date of admission outcome:"
label(data$rwe_pw_admit_hosp_days)="14. Days stayed in hospital:"
label(data$rwe_pw_disch_diagnosis___1)="15. Discharge diagnosis (Check all that apply): (choice=Malaria)"
label(data$rwe_pw_disch_diagnosis___2)="15. Discharge diagnosis (Check all that apply): (choice=URTI)"
label(data$rwe_pw_disch_diagnosis___3)="15. Discharge diagnosis (Check all that apply): (choice=Fracture)"
label(data$rwe_pw_disch_diagnosis___4)="15. Discharge diagnosis (Check all that apply): (choice=Wound)"
label(data$rwe_pw_disch_diagnosis___5)="15. Discharge diagnosis (Check all that apply): (choice=Sepsis)"
label(data$rwe_pw_disch_diagnosis___6)="15. Discharge diagnosis (Check all that apply): (choice=Anaemia)"
label(data$rwe_pw_disch_diagnosis___7)="15. Discharge diagnosis (Check all that apply): (choice=Pneumonia)"
label(data$rwe_pw_disch_diagnosis___8)="15. Discharge diagnosis (Check all that apply): (choice=Diarrhoea)"
label(data$rwe_pw_disch_diagnosis___9)="15. Discharge diagnosis (Check all that apply): (choice=Bronchitis)"
label(data$rwe_pw_disch_diagnosis___10)="15. Discharge diagnosis (Check all that apply): (choice=ARI)"
label(data$rwe_pw_disch_diagnosis___11)="15. Discharge diagnosis (Check all that apply): (choice=Asthma)"
label(data$rwe_pw_disch_diagnosis___12)="15. Discharge diagnosis (Check all that apply): (choice=Malnutrition)"
label(data$rwe_pw_disch_diagnosis___13)="15. Discharge diagnosis (Check all that apply): (choice=Other)"
label(data$rwe_pw_disch_other_diagno)="15b. Other, specify:"
label(data$rwe_pw_treat_disch_recvd)="16. Was treatment received at discharge?"
label(data$rwe_pw_disch_treatment___1)="16b. Treatment received at discharge (Check all that apply): (choice=Acyclovir)"
label(data$rwe_pw_disch_treatment___2)="16b. Treatment received at discharge (Check all that apply): (choice=Adrenaline)"
label(data$rwe_pw_disch_treatment___3)="16b. Treatment received at discharge (Check all that apply): (choice=Albendazole)"
label(data$rwe_pw_disch_treatment___4)="16b. Treatment received at discharge (Check all that apply): (choice=Amikacin)"
label(data$rwe_pw_disch_treatment___5)="16b. Treatment received at discharge (Check all that apply): (choice=Aminophylline)"
label(data$rwe_pw_disch_treatment___6)="16b. Treatment received at discharge (Check all that apply): (choice=Amitriptyline)"
label(data$rwe_pw_disch_treatment___7)="16b. Treatment received at discharge (Check all that apply): (choice=Amoxicillin)"
label(data$rwe_pw_disch_treatment___8)="16b. Treatment received at discharge (Check all that apply): (choice=Amoxicillin+Clavulanic acid)"
label(data$rwe_pw_disch_treatment___9)="16b. Treatment received at discharge (Check all that apply): (choice=Artesunate)"
label(data$rwe_pw_disch_treatment___10)="16b. Treatment received at discharge (Check all that apply): (choice=Atenolol)"
label(data$rwe_pw_disch_treatment___11)="16b. Treatment received at discharge (Check all that apply): (choice=Atropine)"
label(data$rwe_pw_disch_treatment___12)="16b. Treatment received at discharge (Check all that apply): (choice=Azithromycin)"
label(data$rwe_pw_disch_treatment___13)="16b. Treatment received at discharge (Check all that apply): (choice=Baclofen)"
label(data$rwe_pw_disch_treatment___14)="16b. Treatment received at discharge (Check all that apply): (choice=Benzathine penicillin)"
label(data$rwe_pw_disch_treatment___15)="16b. Treatment received at discharge (Check all that apply): (choice=Benzylpenicillin)"
label(data$rwe_pw_disch_treatment___16)="16b. Treatment received at discharge (Check all that apply): (choice=Calcium gluconate)"
label(data$rwe_pw_disch_treatment___17)="16b. Treatment received at discharge (Check all that apply): (choice=Captopril)"
label(data$rwe_pw_disch_treatment___18)="16b. Treatment received at discharge (Check all that apply): (choice=Carbamazepin)"
label(data$rwe_pw_disch_treatment___19)="16b. Treatment received at discharge (Check all that apply): (choice=Ceftriaxone)"
label(data$rwe_pw_disch_treatment___20)="16b. Treatment received at discharge (Check all that apply): (choice=Chlorphenamine)"
label(data$rwe_pw_disch_treatment___21)="16b. Treatment received at discharge (Check all that apply): (choice=Ciprofloxacine)"
label(data$rwe_pw_disch_treatment___22)="16b. Treatment received at discharge (Check all that apply): (choice=Codeine phosphate)"
label(data$rwe_pw_disch_treatment___23)="16b. Treatment received at discharge (Check all that apply): (choice=Cotrimoxazole (Trimethoprim/ sulphamethoxazole))"
label(data$rwe_pw_disch_treatment___24)="16b. Treatment received at discharge (Check all that apply): (choice=Dexamethasone)"
label(data$rwe_pw_disch_treatment___25)="16b. Treatment received at discharge (Check all that apply): (choice=Diazepam)"
label(data$rwe_pw_disch_treatment___26)="16b. Treatment received at discharge (Check all that apply): (choice=Digoxin (no digitalization)"
label(data$rwe_pw_disch_treatment___27)="16b. Treatment received at discharge (Check all that apply): (choice=Enalapril)"
label(data$rwe_pw_disch_treatment___28)="16b. Treatment received at discharge (Check all that apply): (choice=Ferrous Sulphate)"
label(data$rwe_pw_disch_treatment___29)="16b. Treatment received at discharge (Check all that apply): (choice=FFP)"
label(data$rwe_pw_disch_treatment___30)="16b. Treatment received at discharge (Check all that apply): (choice=Flucloxacillin)"
label(data$rwe_pw_disch_treatment___31)="16b. Treatment received at discharge (Check all that apply): (choice=Fluconazole)"
label(data$rwe_pw_disch_treatment___32)="16b. Treatment received at discharge (Check all that apply): (choice=Folic acid)"
label(data$rwe_pw_disch_treatment___33)="16b. Treatment received at discharge (Check all that apply): (choice=Furosemide)"
label(data$rwe_pw_disch_treatment___34)="16b. Treatment received at discharge (Check all that apply): (choice=Gentamicin)"
label(data$rwe_pw_disch_treatment___35)="16b. Treatment received at discharge (Check all that apply): (choice=Glucose 10% and above)"
label(data$rwe_pw_disch_treatment___36)="16b. Treatment received at discharge (Check all that apply): (choice=Griseofulvin)"
label(data$rwe_pw_disch_treatment___37)="16b. Treatment received at discharge (Check all that apply): (choice=Hydralazine)"
label(data$rwe_pw_disch_treatment___38)="16b. Treatment received at discharge (Check all that apply): (choice=Hydrochlorthiazide)"
label(data$rwe_pw_disch_treatment___39)="16b. Treatment received at discharge (Check all that apply): (choice=Hydrocortisone)"
label(data$rwe_pw_disch_treatment___40)="16b. Treatment received at discharge (Check all that apply): (choice=Ibuprofen)"
label(data$rwe_pw_disch_treatment___41)="16b. Treatment received at discharge (Check all that apply): (choice=Insulin)"
label(data$rwe_pw_disch_treatment___42)="16b. Treatment received at discharge (Check all that apply): (choice=Isoniazid)"
label(data$rwe_pw_disch_treatment___43)="16b. Treatment received at discharge (Check all that apply): (choice=Ketamin)"
label(data$rwe_pw_disch_treatment___44)="16b. Treatment received at discharge (Check all that apply): (choice=Ketoconazole)"
label(data$rwe_pw_disch_treatment___45)="16b. Treatment received at discharge (Check all that apply): (choice=Lisinopril)"
label(data$rwe_pw_disch_treatment___46)="16b. Treatment received at discharge (Check all that apply): (choice=Lumefantrine-Artemether (LA))"
label(data$rwe_pw_disch_treatment___47)="16b. Treatment received at discharge (Check all that apply): (choice=Magnesium-sulfate)"
label(data$rwe_pw_disch_treatment___48)="16b. Treatment received at discharge (Check all that apply): (choice=Mebendazole)"
label(data$rwe_pw_disch_treatment___49)="16b. Treatment received at discharge (Check all that apply): (choice=Metronidazole)"
label(data$rwe_pw_disch_treatment___50)="16b. Treatment received at discharge (Check all that apply): (choice=Morphine)"
label(data$rwe_pw_disch_treatment___51)="16b. Treatment received at discharge (Check all that apply): (choice=NaCl 0.9% / Normal Saline)"
label(data$rwe_pw_disch_treatment___52)="16b. Treatment received at discharge (Check all that apply): (choice=Nevirapine)"
label(data$rwe_pw_disch_treatment___53)="16b. Treatment received at discharge (Check all that apply): (choice=Nifedipine)"
label(data$rwe_pw_disch_treatment___54)="16b. Treatment received at discharge (Check all that apply): (choice=Nystatin)"
label(data$rwe_pw_disch_treatment___55)="16b. Treatment received at discharge (Check all that apply): (choice=Omeprazole)"
label(data$rwe_pw_disch_treatment___56)="16b. Treatment received at discharge (Check all that apply): (choice=ORS)"
label(data$rwe_pw_disch_treatment___57)="16b. Treatment received at discharge (Check all that apply): (choice=Packed Cells)"
label(data$rwe_pw_disch_treatment___58)="16b. Treatment received at discharge (Check all that apply): (choice=Paracetamol)"
label(data$rwe_pw_disch_treatment___59)="16b. Treatment received at discharge (Check all that apply): (choice=Paraldehyde)"
label(data$rwe_pw_disch_treatment___60)="16b. Treatment received at discharge (Check all that apply): (choice=Pethidine)"
label(data$rwe_pw_disch_treatment___61)="16b. Treatment received at discharge (Check all that apply): (choice=Phenobarbitone)"
label(data$rwe_pw_disch_treatment___62)="16b. Treatment received at discharge (Check all that apply): (choice=Phenytoin)"
label(data$rwe_pw_disch_treatment___63)="16b. Treatment received at discharge (Check all that apply): (choice=Platelets)"
label(data$rwe_pw_disch_treatment___64)="16b. Treatment received at discharge (Check all that apply): (choice=Potassium (Slow K))"
label(data$rwe_pw_disch_treatment___65)="16b. Treatment received at discharge (Check all that apply): (choice=Praziquantel)"
label(data$rwe_pw_disch_treatment___66)="16b. Treatment received at discharge (Check all that apply): (choice=Prednisolone)"
label(data$rwe_pw_disch_treatment___67)="16b. Treatment received at discharge (Check all that apply): (choice=Promethazine)"
label(data$rwe_pw_disch_treatment___68)="16b. Treatment received at discharge (Check all that apply): (choice=Propranolol)"
label(data$rwe_pw_disch_treatment___69)="16b. Treatment received at discharge (Check all that apply): (choice=Ringers lactate)"
label(data$rwe_pw_disch_treatment___70)="16b. Treatment received at discharge (Check all that apply): (choice=Salbutamol)"
label(data$rwe_pw_disch_treatment___71)="16b. Treatment received at discharge (Check all that apply): (choice=Spironolactone)"
label(data$rwe_pw_disch_treatment___72)="16b. Treatment received at discharge (Check all that apply): (choice=Sulfadoxine/ Pyrimethamine (Fansidar))"
label(data$rwe_pw_disch_treatment___73)="16b. Treatment received at discharge (Check all that apply): (choice=Valproate (Sodium))"
label(data$rwe_pw_disch_treatment___74)="16b. Treatment received at discharge (Check all that apply): (choice=Vitamin A)"
label(data$rwe_pw_disch_treatment___75)="16b. Treatment received at discharge (Check all that apply): (choice=Vitamin K)"
label(data$rwe_pw_disch_treatment___76)="16b. Treatment received at discharge (Check all that apply): (choice=Whole Blood)"
label(data$rwe_pw_disch_treatment___77)="16b. Treatment received at discharge (Check all that apply): (choice=X-Penicillin)"
label(data$rwe_pw_disch_treatment___78)="16b. Treatment received at discharge (Check all that apply): (choice=Zinc)"
label(data$rwe_pw_disch_treatment___98)="16b. Treatment received at discharge (Check all that apply): (choice=Other drugs)"
label(data$rwe_pw_treatment_other_2)="16c. Other, specify:"
label(data$rwe_pw_bill)="17. Total billed amount in MWK:"
label(data$paediatrics_ward_medical_complete)="Complete?"
label(data$recru_staff_initial)="Staff initial:"
label(data$recru_date_extracted)="Date extracted:"
label(data$recru_num_potential_critic)="(Q395) Number of  critical illness events on admission"
label(data$recru_type_cie)="(Q396) Potential Critical Illness Events on admission"
label(data$recru_respiratory)="(Q397) Respiratory"
label(data$recru_circulatory)="(Q398) Circulatory"
label(data$recru_neurological)="(Q399) Neurological"
label(data$recru_infectious)="(Q400) Infectious"
label(data$recru_cie_other)="(Q401) Other"
label(data$recru_type_cie_2)="(Q402) Potential Critical Illness Events on admission"
label(data$recru_respiratory_2)="(Q403) Respiratory"
label(data$recru_circulatory_2)="(Q406) Circulatory"
label(data$recru_neurological_2)="(Q408) Neurological"
label(data$recru_infectious_2)="(Q409) Infectious"
label(data$recru_cie_other_2)="(Q410) Other"
label(data$recru_type_cie_3)="(Q411) Potential Critical Illness Events on admission"
label(data$recru_respiratory_3)="(Q412) Respiratory"
label(data$recru_circulatory_3)="(Q413) Circulatory"
label(data$recru_neurological_3)="(Q414) Neurological"
label(data$recru_infectious_3)="(Q415) Infectious"
label(data$recru_cie_other_3)="(Q416) Other"
label(data$recru_type_cie_4)="(Q417) Potential Critical Illness Events on admission"
label(data$recru_respiratory_4)="(Q418) Respiratory"
label(data$recru_circulatory_4)="(Q419) Circulatory"
label(data$recru_neurological_4)="(Q420) Neurological"
label(data$recru_infectious_4)="(Q421) Infectious"
label(data$recru_cie_other_4)="(Q422) Other"
label(data$recru_type_cie_5)="(Q423) Potential Critical Illness Events on admission"
label(data$recru_respiratory_5)="(Q424) Respiratory"
label(data$recru_circulatory_5)="(Q423) Circulatory"
label(data$recru_neurological_5)="(Q425) Neurological"
label(data$recru_infectious_5)="(Q426) Infectious"
label(data$recru_cie_other_5)="(Q427) Other"
label(data$recru_type_cie_6)="(Q428) Potential Critical Illness Events on admission"
label(data$recru_respiratory_6)="(Q429) Respiratory"
label(data$recru_circulatory_6)="(Q430) Circulatory"
label(data$recru_neurological_6)="(Q431) Neurological"
label(data$recru_infectious_6)="(Q432) Infectious"
label(data$recru_cie_other_6)="(Q431) Other"
label(data$recru_type_cie_7)="(Q432) Potential Critical Illness Events on admission"
label(data$recru_respiratory_7)="(Q433) Respiratory"
label(data$recru_circulatory_7)="(Q434) Circulatory"
label(data$recru_neurological_7)="(Q435) Neurological"
label(data$recru_infectious_7)="(Q436) Infectious"
label(data$recru_cie_other_7)="(Q437) Other"
label(data$recruitment_complete)="Complete?"
label(data$dly_staff_initial)="Staff initial:"
label(data$dly_date_extracted)="Date extracted:"
label(data$dly_new_critical1)="(Q21) New Critical Illness Event During hospitalization (CIE)"
label(data$dly_new_critical_num)="(Q22) Number of  New critical Illness events during hospitalization (CIE)"
label(data$dly_time_new_cie1a)="(Q23) Time new CIE 1"
label(data$dly_type_cie1)="(Q24) Type of CIE 1"
label(data$dly_respiratory)="(Q25) Respiratory"
label(data$dly_circulatory)="(Q26) Circulatory"
label(data$dly_infectious)="(Q27) Infectious"
label(data$dly_neurological)="(Q28) Neurological"
label(data$dly_cie_other)="(Q29) Other"
label(data$dly_time_new_cie1a_2)="(Q34a) Time new CIE 2"
label(data$dly_type_cie2)="(Q34) Type of CIE 2"
label(data$dly_respiratory_2)="(Q35) Respiratory"
label(data$dly_circulatory_2)="(Q36) Circulatory"
label(data$dly_neurological2)="(Q37) Neurological"
label(data$dly_infectious_2)="(Q38) Infectious"
label(data$dly_cie_other_2)="(Q39) Other"
label(data$dly_time_new_cie1a_3)="(Q44a) Time new CIE 3"
label(data$dly_type_cie3)="(Q44) Type of CIE 3"
label(data$dly_respiratory_3)="(Q45) Respiratory"
label(data$dly_circulatory_3)="(Q46) Circulatory"
label(data$dly_neurological3)="(Q47) Neurological"
label(data$dly_infectious_3)="(Q48) Infectious"
label(data$dly_cie_other_3)="(Q49) Other"
label(data$dly_time_new_cie1a_4)="(Q54a) Time new CIE 4"
label(data$dly_type_cie4)="(Q54) Type of CIE 4"
label(data$dly_respiratory_4)="(Q55) Respiratory"
label(data$dly_circulatory_4)="(Q56) Circulatory"
label(data$dly_neurological4)="(Q57) Neurological"
label(data$dly_infectious_4)="(Q58) Infectious"
label(data$dly_cie_other_4)="(Q59) Other"
label(data$dly_time_new_cie1a_5)="(Q64a) Time new CIE 5"
label(data$dly_type_cie5)="(Q64) Type of CIE 5"
label(data$dly_respiratory_5)="(Q65) Respiratory"
label(data$dly_circulatory_5)="(Q61) Circulatory"
label(data$dly_neurological5)="(Q62) Neurological"
label(data$dly_infectious_5)="(Q63) Infectious"
label(data$dly_cie_other_5)="(Q64) Other"
label(data$dly_time_new_cie1a_6)="(Q64a) Time new CIE 6"
label(data$dly_type_cie6)="(Q69) Type of CIE 6"
label(data$dly_respiratory_6)="(Q70) Respiratory"
label(data$dly_circulatory_6)="(Q71) Circulatory"
label(data$dly_neurological6)="(Q72) Neurological"
label(data$dly_infectious_6)="(Q73) Infectious"
label(data$dly_cie_other_6)="(Q74) Other"
label(data$dly_time_new_cie1a_7)="(Q79a) Time new CIE 7"
label(data$dly_type_cie7)="(Q79) Type of CIE 7"
label(data$dly_respiratory_7)="(Q80) Respiratory"
label(data$dly_circulatory_7)="(Q81) Circulatory"
label(data$dly_neurological7)="(Q82) Neurological"
label(data$dly_infectious_7)="(Q83) Infectious"
label(data$dly_cie_other_7)="(Q84) Other"
label(data$dly_time_new_cie1a_8)="(Q89a) Time new CIE 8"
label(data$dly_type_cie8)="(Q89) Type of CIE 8"
label(data$dly_respiratory_8)="(Q90) Respiratory"
label(data$dly_circulatory_8)="(Q91) Circulatory"
label(data$dly_neurological8)="(Q92) Neurological"
label(data$dly_infectious_8)="(Q93) Infectious"
label(data$dly_cie_other_8)="(Q94) Other"
label(data$daily_sheet_complete)="Complete?"
# Setting Units


# Setting Factors(will create new variable for factors)
data$rwe_pw_name_hosp.factor = factor(data$rwe_pw_name_hosp,levels=c("1","2","3","4","5"))
data$rwe_pw_admit_age___1.factor = factor(data$rwe_pw_admit_age___1,levels=c("0","1"))
data$rwe_pw_admit_age___2.factor = factor(data$rwe_pw_admit_age___2,levels=c("0","1"))
data$rwe_pw_admit_age___3.factor = factor(data$rwe_pw_admit_age___3,levels=c("0","1"))
data$rwe_pw_admit_age___4.factor = factor(data$rwe_pw_admit_age___4,levels=c("0","1"))
data$rwe_pw_sex_parent.factor = factor(data$rwe_pw_sex_parent,levels=c("1","2"))
data$rwe_pw_preg.factor = factor(data$rwe_pw_preg,levels=c("1","2","3"))
data$rwe_pw_hiv_status.factor = factor(data$rwe_pw_hiv_status,levels=c("1","2","3"))
data$rwe_pw_hiv_child.factor = factor(data$rwe_pw_hiv_child,levels=c("1","0"))
data$rwe_pw_hiv_status_2.factor = factor(data$rwe_pw_hiv_status_2,levels=c("1","2","3"))
data$rwe_pw_admit_diagnosis___1.factor = factor(data$rwe_pw_admit_diagnosis___1,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___2.factor = factor(data$rwe_pw_admit_diagnosis___2,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___3.factor = factor(data$rwe_pw_admit_diagnosis___3,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___4.factor = factor(data$rwe_pw_admit_diagnosis___4,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___5.factor = factor(data$rwe_pw_admit_diagnosis___5,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___6.factor = factor(data$rwe_pw_admit_diagnosis___6,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___7.factor = factor(data$rwe_pw_admit_diagnosis___7,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___8.factor = factor(data$rwe_pw_admit_diagnosis___8,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___9.factor = factor(data$rwe_pw_admit_diagnosis___9,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___10.factor = factor(data$rwe_pw_admit_diagnosis___10,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___11.factor = factor(data$rwe_pw_admit_diagnosis___11,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___12.factor = factor(data$rwe_pw_admit_diagnosis___12,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___13.factor = factor(data$rwe_pw_admit_diagnosis___13,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___14.factor = factor(data$rwe_pw_admit_diagnosis___14,levels=c("0","1"))
data$rwe_pw_admit_diagnosis___15.factor = factor(data$rwe_pw_admit_diagnosis___15,levels=c("0","1"))
data$rwe_pw_any_tests.factor = factor(data$rwe_pw_any_tests,levels=c("1","0"))
data$rwe_pw_tests_done___1.factor = factor(data$rwe_pw_tests_done___1,levels=c("0","1"))
data$rwe_pw_tests_done___2.factor = factor(data$rwe_pw_tests_done___2,levels=c("0","1"))
data$rwe_pw_tests_done___3.factor = factor(data$rwe_pw_tests_done___3,levels=c("0","1"))
data$rwe_pw_tests_done___4.factor = factor(data$rwe_pw_tests_done___4,levels=c("0","1"))
data$rwe_pw_tests_done___5.factor = factor(data$rwe_pw_tests_done___5,levels=c("0","1"))
data$rwe_pw_tests_done___6.factor = factor(data$rwe_pw_tests_done___6,levels=c("0","1"))
data$rwe_pw_tests_done___7.factor = factor(data$rwe_pw_tests_done___7,levels=c("0","1"))
data$rwe_pw_treat_admit_received.factor = factor(data$rwe_pw_treat_admit_received,levels=c("1","0"))
data$rwe_pw_admit_treatment___1.factor = factor(data$rwe_pw_admit_treatment___1,levels=c("0","1"))
data$rwe_pw_admit_treatment___2.factor = factor(data$rwe_pw_admit_treatment___2,levels=c("0","1"))
data$rwe_pw_admit_treatment___3.factor = factor(data$rwe_pw_admit_treatment___3,levels=c("0","1"))
data$rwe_pw_admit_treatment___4.factor = factor(data$rwe_pw_admit_treatment___4,levels=c("0","1"))
data$rwe_pw_admit_treatment___5.factor = factor(data$rwe_pw_admit_treatment___5,levels=c("0","1"))
data$rwe_pw_admit_treatment___6.factor = factor(data$rwe_pw_admit_treatment___6,levels=c("0","1"))
data$rwe_pw_admit_treatment___7.factor = factor(data$rwe_pw_admit_treatment___7,levels=c("0","1"))
data$rwe_pw_admit_treatment___8.factor = factor(data$rwe_pw_admit_treatment___8,levels=c("0","1"))
data$rwe_pw_admit_treatment___9.factor = factor(data$rwe_pw_admit_treatment___9,levels=c("0","1"))
data$rwe_pw_admit_treatment___10.factor = factor(data$rwe_pw_admit_treatment___10,levels=c("0","1"))
data$rwe_pw_admit_treatment___11.factor = factor(data$rwe_pw_admit_treatment___11,levels=c("0","1"))
data$rwe_pw_admit_treatment___12.factor = factor(data$rwe_pw_admit_treatment___12,levels=c("0","1"))
data$rwe_pw_admit_treatment___13.factor = factor(data$rwe_pw_admit_treatment___13,levels=c("0","1"))
data$rwe_pw_admit_treatment___14.factor = factor(data$rwe_pw_admit_treatment___14,levels=c("0","1"))
data$rwe_pw_admit_treatment___15.factor = factor(data$rwe_pw_admit_treatment___15,levels=c("0","1"))
data$rwe_pw_admit_treatment___16.factor = factor(data$rwe_pw_admit_treatment___16,levels=c("0","1"))
data$rwe_pw_admit_treatment___17.factor = factor(data$rwe_pw_admit_treatment___17,levels=c("0","1"))
data$rwe_pw_admit_treatment___18.factor = factor(data$rwe_pw_admit_treatment___18,levels=c("0","1"))
data$rwe_pw_admit_treatment___19.factor = factor(data$rwe_pw_admit_treatment___19,levels=c("0","1"))
data$rwe_pw_admit_treatment___20.factor = factor(data$rwe_pw_admit_treatment___20,levels=c("0","1"))
data$rwe_pw_admit_treatment___21.factor = factor(data$rwe_pw_admit_treatment___21,levels=c("0","1"))
data$rwe_pw_admit_treatment___22.factor = factor(data$rwe_pw_admit_treatment___22,levels=c("0","1"))
data$rwe_pw_admit_treatment___23.factor = factor(data$rwe_pw_admit_treatment___23,levels=c("0","1"))
data$rwe_pw_admit_treatment___24.factor = factor(data$rwe_pw_admit_treatment___24,levels=c("0","1"))
data$rwe_pw_admit_treatment___25.factor = factor(data$rwe_pw_admit_treatment___25,levels=c("0","1"))
data$rwe_pw_admit_treatment___26.factor = factor(data$rwe_pw_admit_treatment___26,levels=c("0","1"))
data$rwe_pw_admit_treatment___27.factor = factor(data$rwe_pw_admit_treatment___27,levels=c("0","1"))
data$rwe_pw_admit_treatment___28.factor = factor(data$rwe_pw_admit_treatment___28,levels=c("0","1"))
data$rwe_pw_admit_treatment___29.factor = factor(data$rwe_pw_admit_treatment___29,levels=c("0","1"))
data$rwe_pw_admit_treatment___30.factor = factor(data$rwe_pw_admit_treatment___30,levels=c("0","1"))
data$rwe_pw_admit_treatment___31.factor = factor(data$rwe_pw_admit_treatment___31,levels=c("0","1"))
data$rwe_pw_admit_treatment___32.factor = factor(data$rwe_pw_admit_treatment___32,levels=c("0","1"))
data$rwe_pw_admit_treatment___33.factor = factor(data$rwe_pw_admit_treatment___33,levels=c("0","1"))
data$rwe_pw_admit_treatment___34.factor = factor(data$rwe_pw_admit_treatment___34,levels=c("0","1"))
data$rwe_pw_admit_treatment___35.factor = factor(data$rwe_pw_admit_treatment___35,levels=c("0","1"))
data$rwe_pw_admit_treatment___36.factor = factor(data$rwe_pw_admit_treatment___36,levels=c("0","1"))
data$rwe_pw_admit_treatment___37.factor = factor(data$rwe_pw_admit_treatment___37,levels=c("0","1"))
data$rwe_pw_admit_treatment___38.factor = factor(data$rwe_pw_admit_treatment___38,levels=c("0","1"))
data$rwe_pw_admit_treatment___39.factor = factor(data$rwe_pw_admit_treatment___39,levels=c("0","1"))
data$rwe_pw_admit_treatment___40.factor = factor(data$rwe_pw_admit_treatment___40,levels=c("0","1"))
data$rwe_pw_admit_treatment___41.factor = factor(data$rwe_pw_admit_treatment___41,levels=c("0","1"))
data$rwe_pw_admit_treatment___42.factor = factor(data$rwe_pw_admit_treatment___42,levels=c("0","1"))
data$rwe_pw_admit_treatment___43.factor = factor(data$rwe_pw_admit_treatment___43,levels=c("0","1"))
data$rwe_pw_admit_treatment___44.factor = factor(data$rwe_pw_admit_treatment___44,levels=c("0","1"))
data$rwe_pw_admit_treatment___45.factor = factor(data$rwe_pw_admit_treatment___45,levels=c("0","1"))
data$rwe_pw_admit_treatment___46.factor = factor(data$rwe_pw_admit_treatment___46,levels=c("0","1"))
data$rwe_pw_admit_treatment___47.factor = factor(data$rwe_pw_admit_treatment___47,levels=c("0","1"))
data$rwe_pw_admit_treatment___48.factor = factor(data$rwe_pw_admit_treatment___48,levels=c("0","1"))
data$rwe_pw_admit_treatment___49.factor = factor(data$rwe_pw_admit_treatment___49,levels=c("0","1"))
data$rwe_pw_admit_treatment___50.factor = factor(data$rwe_pw_admit_treatment___50,levels=c("0","1"))
data$rwe_pw_admit_treatment___51.factor = factor(data$rwe_pw_admit_treatment___51,levels=c("0","1"))
data$rwe_pw_admit_treatment___52.factor = factor(data$rwe_pw_admit_treatment___52,levels=c("0","1"))
data$rwe_pw_admit_treatment___53.factor = factor(data$rwe_pw_admit_treatment___53,levels=c("0","1"))
data$rwe_pw_admit_treatment___54.factor = factor(data$rwe_pw_admit_treatment___54,levels=c("0","1"))
data$rwe_pw_admit_treatment___55.factor = factor(data$rwe_pw_admit_treatment___55,levels=c("0","1"))
data$rwe_pw_admit_treatment___56.factor = factor(data$rwe_pw_admit_treatment___56,levels=c("0","1"))
data$rwe_pw_admit_treatment___57.factor = factor(data$rwe_pw_admit_treatment___57,levels=c("0","1"))
data$rwe_pw_admit_treatment___58.factor = factor(data$rwe_pw_admit_treatment___58,levels=c("0","1"))
data$rwe_pw_admit_treatment___59.factor = factor(data$rwe_pw_admit_treatment___59,levels=c("0","1"))
data$rwe_pw_admit_treatment___60.factor = factor(data$rwe_pw_admit_treatment___60,levels=c("0","1"))
data$rwe_pw_admit_treatment___61.factor = factor(data$rwe_pw_admit_treatment___61,levels=c("0","1"))
data$rwe_pw_admit_treatment___62.factor = factor(data$rwe_pw_admit_treatment___62,levels=c("0","1"))
data$rwe_pw_admit_treatment___63.factor = factor(data$rwe_pw_admit_treatment___63,levels=c("0","1"))
data$rwe_pw_admit_treatment___64.factor = factor(data$rwe_pw_admit_treatment___64,levels=c("0","1"))
data$rwe_pw_admit_treatment___65.factor = factor(data$rwe_pw_admit_treatment___65,levels=c("0","1"))
data$rwe_pw_admit_treatment___66.factor = factor(data$rwe_pw_admit_treatment___66,levels=c("0","1"))
data$rwe_pw_admit_treatment___67.factor = factor(data$rwe_pw_admit_treatment___67,levels=c("0","1"))
data$rwe_pw_admit_treatment___68.factor = factor(data$rwe_pw_admit_treatment___68,levels=c("0","1"))
data$rwe_pw_admit_treatment___69.factor = factor(data$rwe_pw_admit_treatment___69,levels=c("0","1"))
data$rwe_pw_admit_treatment___70.factor = factor(data$rwe_pw_admit_treatment___70,levels=c("0","1"))
data$rwe_pw_admit_treatment___71.factor = factor(data$rwe_pw_admit_treatment___71,levels=c("0","1"))
data$rwe_pw_admit_treatment___72.factor = factor(data$rwe_pw_admit_treatment___72,levels=c("0","1"))
data$rwe_pw_admit_treatment___73.factor = factor(data$rwe_pw_admit_treatment___73,levels=c("0","1"))
data$rwe_pw_admit_treatment___74.factor = factor(data$rwe_pw_admit_treatment___74,levels=c("0","1"))
data$rwe_pw_admit_treatment___75.factor = factor(data$rwe_pw_admit_treatment___75,levels=c("0","1"))
data$rwe_pw_admit_treatment___76.factor = factor(data$rwe_pw_admit_treatment___76,levels=c("0","1"))
data$rwe_pw_admit_treatment___77.factor = factor(data$rwe_pw_admit_treatment___77,levels=c("0","1"))
data$rwe_pw_admit_treatment___78.factor = factor(data$rwe_pw_admit_treatment___78,levels=c("0","1"))
data$rwe_pw_admit_treatment___98.factor = factor(data$rwe_pw_admit_treatment___98,levels=c("0","1"))
data$rwe_pw_outcome_disch_hosp.factor = factor(data$rwe_pw_outcome_disch_hosp,levels=c("2","3","4","5","6"))
data$rwe_pw_disch_diagnosis___1.factor = factor(data$rwe_pw_disch_diagnosis___1,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___2.factor = factor(data$rwe_pw_disch_diagnosis___2,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___3.factor = factor(data$rwe_pw_disch_diagnosis___3,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___4.factor = factor(data$rwe_pw_disch_diagnosis___4,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___5.factor = factor(data$rwe_pw_disch_diagnosis___5,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___6.factor = factor(data$rwe_pw_disch_diagnosis___6,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___7.factor = factor(data$rwe_pw_disch_diagnosis___7,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___8.factor = factor(data$rwe_pw_disch_diagnosis___8,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___9.factor = factor(data$rwe_pw_disch_diagnosis___9,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___10.factor = factor(data$rwe_pw_disch_diagnosis___10,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___11.factor = factor(data$rwe_pw_disch_diagnosis___11,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___12.factor = factor(data$rwe_pw_disch_diagnosis___12,levels=c("0","1"))
data$rwe_pw_disch_diagnosis___13.factor = factor(data$rwe_pw_disch_diagnosis___13,levels=c("0","1"))
data$rwe_pw_treat_disch_recvd.factor = factor(data$rwe_pw_treat_disch_recvd,levels=c("1","0"))
data$rwe_pw_disch_treatment___1.factor = factor(data$rwe_pw_disch_treatment___1,levels=c("0","1"))
data$rwe_pw_disch_treatment___2.factor = factor(data$rwe_pw_disch_treatment___2,levels=c("0","1"))
data$rwe_pw_disch_treatment___3.factor = factor(data$rwe_pw_disch_treatment___3,levels=c("0","1"))
data$rwe_pw_disch_treatment___4.factor = factor(data$rwe_pw_disch_treatment___4,levels=c("0","1"))
data$rwe_pw_disch_treatment___5.factor = factor(data$rwe_pw_disch_treatment___5,levels=c("0","1"))
data$rwe_pw_disch_treatment___6.factor = factor(data$rwe_pw_disch_treatment___6,levels=c("0","1"))
data$rwe_pw_disch_treatment___7.factor = factor(data$rwe_pw_disch_treatment___7,levels=c("0","1"))
data$rwe_pw_disch_treatment___8.factor = factor(data$rwe_pw_disch_treatment___8,levels=c("0","1"))
data$rwe_pw_disch_treatment___9.factor = factor(data$rwe_pw_disch_treatment___9,levels=c("0","1"))
data$rwe_pw_disch_treatment___10.factor = factor(data$rwe_pw_disch_treatment___10,levels=c("0","1"))
data$rwe_pw_disch_treatment___11.factor = factor(data$rwe_pw_disch_treatment___11,levels=c("0","1"))
data$rwe_pw_disch_treatment___12.factor = factor(data$rwe_pw_disch_treatment___12,levels=c("0","1"))
data$rwe_pw_disch_treatment___13.factor = factor(data$rwe_pw_disch_treatment___13,levels=c("0","1"))
data$rwe_pw_disch_treatment___14.factor = factor(data$rwe_pw_disch_treatment___14,levels=c("0","1"))
data$rwe_pw_disch_treatment___15.factor = factor(data$rwe_pw_disch_treatment___15,levels=c("0","1"))
data$rwe_pw_disch_treatment___16.factor = factor(data$rwe_pw_disch_treatment___16,levels=c("0","1"))
data$rwe_pw_disch_treatment___17.factor = factor(data$rwe_pw_disch_treatment___17,levels=c("0","1"))
data$rwe_pw_disch_treatment___18.factor = factor(data$rwe_pw_disch_treatment___18,levels=c("0","1"))
data$rwe_pw_disch_treatment___19.factor = factor(data$rwe_pw_disch_treatment___19,levels=c("0","1"))
data$rwe_pw_disch_treatment___20.factor = factor(data$rwe_pw_disch_treatment___20,levels=c("0","1"))
data$rwe_pw_disch_treatment___21.factor = factor(data$rwe_pw_disch_treatment___21,levels=c("0","1"))
data$rwe_pw_disch_treatment___22.factor = factor(data$rwe_pw_disch_treatment___22,levels=c("0","1"))
data$rwe_pw_disch_treatment___23.factor = factor(data$rwe_pw_disch_treatment___23,levels=c("0","1"))
data$rwe_pw_disch_treatment___24.factor = factor(data$rwe_pw_disch_treatment___24,levels=c("0","1"))
data$rwe_pw_disch_treatment___25.factor = factor(data$rwe_pw_disch_treatment___25,levels=c("0","1"))
data$rwe_pw_disch_treatment___26.factor = factor(data$rwe_pw_disch_treatment___26,levels=c("0","1"))
data$rwe_pw_disch_treatment___27.factor = factor(data$rwe_pw_disch_treatment___27,levels=c("0","1"))
data$rwe_pw_disch_treatment___28.factor = factor(data$rwe_pw_disch_treatment___28,levels=c("0","1"))
data$rwe_pw_disch_treatment___29.factor = factor(data$rwe_pw_disch_treatment___29,levels=c("0","1"))
data$rwe_pw_disch_treatment___30.factor = factor(data$rwe_pw_disch_treatment___30,levels=c("0","1"))
data$rwe_pw_disch_treatment___31.factor = factor(data$rwe_pw_disch_treatment___31,levels=c("0","1"))
data$rwe_pw_disch_treatment___32.factor = factor(data$rwe_pw_disch_treatment___32,levels=c("0","1"))
data$rwe_pw_disch_treatment___33.factor = factor(data$rwe_pw_disch_treatment___33,levels=c("0","1"))
data$rwe_pw_disch_treatment___34.factor = factor(data$rwe_pw_disch_treatment___34,levels=c("0","1"))
data$rwe_pw_disch_treatment___35.factor = factor(data$rwe_pw_disch_treatment___35,levels=c("0","1"))
data$rwe_pw_disch_treatment___36.factor = factor(data$rwe_pw_disch_treatment___36,levels=c("0","1"))
data$rwe_pw_disch_treatment___37.factor = factor(data$rwe_pw_disch_treatment___37,levels=c("0","1"))
data$rwe_pw_disch_treatment___38.factor = factor(data$rwe_pw_disch_treatment___38,levels=c("0","1"))
data$rwe_pw_disch_treatment___39.factor = factor(data$rwe_pw_disch_treatment___39,levels=c("0","1"))
data$rwe_pw_disch_treatment___40.factor = factor(data$rwe_pw_disch_treatment___40,levels=c("0","1"))
data$rwe_pw_disch_treatment___41.factor = factor(data$rwe_pw_disch_treatment___41,levels=c("0","1"))
data$rwe_pw_disch_treatment___42.factor = factor(data$rwe_pw_disch_treatment___42,levels=c("0","1"))
data$rwe_pw_disch_treatment___43.factor = factor(data$rwe_pw_disch_treatment___43,levels=c("0","1"))
data$rwe_pw_disch_treatment___44.factor = factor(data$rwe_pw_disch_treatment___44,levels=c("0","1"))
data$rwe_pw_disch_treatment___45.factor = factor(data$rwe_pw_disch_treatment___45,levels=c("0","1"))
data$rwe_pw_disch_treatment___46.factor = factor(data$rwe_pw_disch_treatment___46,levels=c("0","1"))
data$rwe_pw_disch_treatment___47.factor = factor(data$rwe_pw_disch_treatment___47,levels=c("0","1"))
data$rwe_pw_disch_treatment___48.factor = factor(data$rwe_pw_disch_treatment___48,levels=c("0","1"))
data$rwe_pw_disch_treatment___49.factor = factor(data$rwe_pw_disch_treatment___49,levels=c("0","1"))
data$rwe_pw_disch_treatment___50.factor = factor(data$rwe_pw_disch_treatment___50,levels=c("0","1"))
data$rwe_pw_disch_treatment___51.factor = factor(data$rwe_pw_disch_treatment___51,levels=c("0","1"))
data$rwe_pw_disch_treatment___52.factor = factor(data$rwe_pw_disch_treatment___52,levels=c("0","1"))
data$rwe_pw_disch_treatment___53.factor = factor(data$rwe_pw_disch_treatment___53,levels=c("0","1"))
data$rwe_pw_disch_treatment___54.factor = factor(data$rwe_pw_disch_treatment___54,levels=c("0","1"))
data$rwe_pw_disch_treatment___55.factor = factor(data$rwe_pw_disch_treatment___55,levels=c("0","1"))
data$rwe_pw_disch_treatment___56.factor = factor(data$rwe_pw_disch_treatment___56,levels=c("0","1"))
data$rwe_pw_disch_treatment___57.factor = factor(data$rwe_pw_disch_treatment___57,levels=c("0","1"))
data$rwe_pw_disch_treatment___58.factor = factor(data$rwe_pw_disch_treatment___58,levels=c("0","1"))
data$rwe_pw_disch_treatment___59.factor = factor(data$rwe_pw_disch_treatment___59,levels=c("0","1"))
data$rwe_pw_disch_treatment___60.factor = factor(data$rwe_pw_disch_treatment___60,levels=c("0","1"))
data$rwe_pw_disch_treatment___61.factor = factor(data$rwe_pw_disch_treatment___61,levels=c("0","1"))
data$rwe_pw_disch_treatment___62.factor = factor(data$rwe_pw_disch_treatment___62,levels=c("0","1"))
data$rwe_pw_disch_treatment___63.factor = factor(data$rwe_pw_disch_treatment___63,levels=c("0","1"))
data$rwe_pw_disch_treatment___64.factor = factor(data$rwe_pw_disch_treatment___64,levels=c("0","1"))
data$rwe_pw_disch_treatment___65.factor = factor(data$rwe_pw_disch_treatment___65,levels=c("0","1"))
data$rwe_pw_disch_treatment___66.factor = factor(data$rwe_pw_disch_treatment___66,levels=c("0","1"))
data$rwe_pw_disch_treatment___67.factor = factor(data$rwe_pw_disch_treatment___67,levels=c("0","1"))
data$rwe_pw_disch_treatment___68.factor = factor(data$rwe_pw_disch_treatment___68,levels=c("0","1"))
data$rwe_pw_disch_treatment___69.factor = factor(data$rwe_pw_disch_treatment___69,levels=c("0","1"))
data$rwe_pw_disch_treatment___70.factor = factor(data$rwe_pw_disch_treatment___70,levels=c("0","1"))
data$rwe_pw_disch_treatment___71.factor = factor(data$rwe_pw_disch_treatment___71,levels=c("0","1"))
data$rwe_pw_disch_treatment___72.factor = factor(data$rwe_pw_disch_treatment___72,levels=c("0","1"))
data$rwe_pw_disch_treatment___73.factor = factor(data$rwe_pw_disch_treatment___73,levels=c("0","1"))
data$rwe_pw_disch_treatment___74.factor = factor(data$rwe_pw_disch_treatment___74,levels=c("0","1"))
data$rwe_pw_disch_treatment___75.factor = factor(data$rwe_pw_disch_treatment___75,levels=c("0","1"))
data$rwe_pw_disch_treatment___76.factor = factor(data$rwe_pw_disch_treatment___76,levels=c("0","1"))
data$rwe_pw_disch_treatment___77.factor = factor(data$rwe_pw_disch_treatment___77,levels=c("0","1"))
data$rwe_pw_disch_treatment___78.factor = factor(data$rwe_pw_disch_treatment___78,levels=c("0","1"))
data$rwe_pw_disch_treatment___98.factor = factor(data$rwe_pw_disch_treatment___98,levels=c("0","1"))
data$paediatrics_ward_medical_complete.factor = factor(data$paediatrics_ward_medical_complete,levels=c("0","1","2"))
data$recru_num_potential_critic.factor = factor(data$recru_num_potential_critic,levels=c("0","1","2","3","4","5","6","7"))
data$recru_type_cie.factor = factor(data$recru_type_cie,levels=c("1","2","3","4","98"))
data$recru_respiratory.factor = factor(data$recru_respiratory,levels=c("1","2","3","4"))
data$recru_circulatory.factor = factor(data$recru_circulatory,levels=c("1","2","3","4"))
data$recru_neurological.factor = factor(data$recru_neurological,levels=c("1","2"))
data$recru_infectious.factor = factor(data$recru_infectious,levels=c("1","2"))
data$recru_cie_other.factor = factor(data$recru_cie_other,levels=c("1","2","3","4"))
data$recru_type_cie_2.factor = factor(data$recru_type_cie_2,levels=c("1","2","3","4","98"))
data$recru_respiratory_2.factor = factor(data$recru_respiratory_2,levels=c("1","2","3","4"))
data$recru_circulatory_2.factor = factor(data$recru_circulatory_2,levels=c("1","2","3","4"))
data$recru_neurological_2.factor = factor(data$recru_neurological_2,levels=c("1","2"))
data$recru_infectious_2.factor = factor(data$recru_infectious_2,levels=c("1","2"))
data$recru_cie_other_2.factor = factor(data$recru_cie_other_2,levels=c("1","2","3","4"))
data$recru_type_cie_3.factor = factor(data$recru_type_cie_3,levels=c("1","2","3","4","98"))
data$recru_respiratory_3.factor = factor(data$recru_respiratory_3,levels=c("1","2","3","4"))
data$recru_circulatory_3.factor = factor(data$recru_circulatory_3,levels=c("1","2","3","4"))
data$recru_neurological_3.factor = factor(data$recru_neurological_3,levels=c("1","2"))
data$recru_infectious_3.factor = factor(data$recru_infectious_3,levels=c("1","2"))
data$recru_cie_other_3.factor = factor(data$recru_cie_other_3,levels=c("1","2","3","4"))
data$recru_type_cie_4.factor = factor(data$recru_type_cie_4,levels=c("1","2","3","4","98"))
data$recru_respiratory_4.factor = factor(data$recru_respiratory_4,levels=c("1","2","3","4"))
data$recru_circulatory_4.factor = factor(data$recru_circulatory_4,levels=c("1","2","3","4"))
data$recru_neurological_4.factor = factor(data$recru_neurological_4,levels=c("1","2"))
data$recru_infectious_4.factor = factor(data$recru_infectious_4,levels=c("1","2"))
data$recru_cie_other_4.factor = factor(data$recru_cie_other_4,levels=c("1","2","3","4"))
data$recru_type_cie_5.factor = factor(data$recru_type_cie_5,levels=c("1","2","3","4","98"))
data$recru_respiratory_5.factor = factor(data$recru_respiratory_5,levels=c("1","2","3","4"))
data$recru_circulatory_5.factor = factor(data$recru_circulatory_5,levels=c("1","2","3","4"))
data$recru_neurological_5.factor = factor(data$recru_neurological_5,levels=c("1","2"))
data$recru_infectious_5.factor = factor(data$recru_infectious_5,levels=c("1","2"))
data$recru_cie_other_5.factor = factor(data$recru_cie_other_5,levels=c("1","2","3","4"))
data$recru_type_cie_6.factor = factor(data$recru_type_cie_6,levels=c("1","2","3","4","98"))
data$recru_respiratory_6.factor = factor(data$recru_respiratory_6,levels=c("1","2","3","4"))
data$recru_circulatory_6.factor = factor(data$recru_circulatory_6,levels=c("1","2","3","4"))
data$recru_neurological_6.factor = factor(data$recru_neurological_6,levels=c("1","2"))
data$recru_infectious_6.factor = factor(data$recru_infectious_6,levels=c("1","2"))
data$recru_cie_other_6.factor = factor(data$recru_cie_other_6,levels=c("1","2","3","4"))
data$recru_type_cie_7.factor = factor(data$recru_type_cie_7,levels=c("1","2","3","4","98"))
data$recru_respiratory_7.factor = factor(data$recru_respiratory_7,levels=c("1","2","3","4"))
data$recru_circulatory_7.factor = factor(data$recru_circulatory_7,levels=c("1","2","3","4"))
data$recru_neurological_7.factor = factor(data$recru_neurological_7,levels=c("1","2"))
data$recru_infectious_7.factor = factor(data$recru_infectious_7,levels=c("1","2"))
data$recru_cie_other_7.factor = factor(data$recru_cie_other_7,levels=c("1","2","3","4"))
data$recruitment_complete.factor = factor(data$recruitment_complete,levels=c("0","1","2"))
data$dly_new_critical1.factor = factor(data$dly_new_critical1,levels=c("1","0","99"))
data$dly_new_critical_num.factor = factor(data$dly_new_critical_num,levels=c("1","2","3","4","5","6","7","8"))
data$dly_type_cie1.factor = factor(data$dly_type_cie1,levels=c("1","2","3","4","98"))
data$dly_respiratory.factor = factor(data$dly_respiratory,levels=c("1","2","3","4"))
data$dly_circulatory.factor = factor(data$dly_circulatory,levels=c("1","2","3","4"))
data$dly_infectious.factor = factor(data$dly_infectious,levels=c("1","2"))
data$dly_neurological.factor = factor(data$dly_neurological,levels=c("1","2"))
data$dly_cie_other.factor = factor(data$dly_cie_other,levels=c("1","2","3","4"))
data$dly_type_cie2.factor = factor(data$dly_type_cie2,levels=c("1","2","3","4","98"))
data$dly_respiratory_2.factor = factor(data$dly_respiratory_2,levels=c("1","2","3","4"))
data$dly_circulatory_2.factor = factor(data$dly_circulatory_2,levels=c("1","2","3","4"))
data$dly_neurological2.factor = factor(data$dly_neurological2,levels=c("1","2"))
data$dly_infectious_2.factor = factor(data$dly_infectious_2,levels=c("1","2"))
data$dly_cie_other_2.factor = factor(data$dly_cie_other_2,levels=c("1","2","3","4"))
data$dly_type_cie3.factor = factor(data$dly_type_cie3,levels=c("1","2","3","4","98"))
data$dly_respiratory_3.factor = factor(data$dly_respiratory_3,levels=c("1","2","3","4"))
data$dly_circulatory_3.factor = factor(data$dly_circulatory_3,levels=c("1","2","3","4"))
data$dly_neurological3.factor = factor(data$dly_neurological3,levels=c("1","2"))
data$dly_infectious_3.factor = factor(data$dly_infectious_3,levels=c("1","2"))
data$dly_cie_other_3.factor = factor(data$dly_cie_other_3,levels=c("1","2","3","4"))
data$dly_type_cie4.factor = factor(data$dly_type_cie4,levels=c("1","2","3","4","98"))
data$dly_respiratory_4.factor = factor(data$dly_respiratory_4,levels=c("1","2","3","4"))
data$dly_circulatory_4.factor = factor(data$dly_circulatory_4,levels=c("1","2","3","4"))
data$dly_neurological4.factor = factor(data$dly_neurological4,levels=c("1","2"))
data$dly_infectious_4.factor = factor(data$dly_infectious_4,levels=c("1","2"))
data$dly_cie_other_4.factor = factor(data$dly_cie_other_4,levels=c("1","2","3","4"))
data$dly_type_cie5.factor = factor(data$dly_type_cie5,levels=c("1","2","3","4","98"))
data$dly_respiratory_5.factor = factor(data$dly_respiratory_5,levels=c("1","2","3","4"))
data$dly_circulatory_5.factor = factor(data$dly_circulatory_5,levels=c("1","2","3","4"))
data$dly_neurological5.factor = factor(data$dly_neurological5,levels=c("1","2"))
data$dly_infectious_5.factor = factor(data$dly_infectious_5,levels=c("1","2"))
data$dly_cie_other_5.factor = factor(data$dly_cie_other_5,levels=c("1","2","3","4"))
data$dly_type_cie6.factor = factor(data$dly_type_cie6,levels=c("1","2","3","4","98"))
data$dly_respiratory_6.factor = factor(data$dly_respiratory_6,levels=c("1","2","3","4"))
data$dly_circulatory_6.factor = factor(data$dly_circulatory_6,levels=c("1","2","3","4"))
data$dly_neurological6.factor = factor(data$dly_neurological6,levels=c("1","2"))
data$dly_infectious_6.factor = factor(data$dly_infectious_6,levels=c("1","2"))
data$dly_cie_other_6.factor = factor(data$dly_cie_other_6,levels=c("1","2","3","4"))
data$dly_type_cie7.factor = factor(data$dly_type_cie7,levels=c("1","2","3","4","98"))
data$dly_respiratory_7.factor = factor(data$dly_respiratory_7,levels=c("1","2","3","4"))
data$dly_circulatory_7.factor = factor(data$dly_circulatory_7,levels=c("1","2","3","4"))
data$dly_neurological7.factor = factor(data$dly_neurological7,levels=c("1","2"))
data$dly_infectious_7.factor = factor(data$dly_infectious_7,levels=c("1","2"))
data$dly_cie_other_7.factor = factor(data$dly_cie_other_7,levels=c("1","2","3","4"))
data$dly_type_cie8.factor = factor(data$dly_type_cie8,levels=c("1","2","3","4","98"))
data$dly_respiratory_8.factor = factor(data$dly_respiratory_8,levels=c("1","2","3","4"))
data$dly_circulatory_8.factor = factor(data$dly_circulatory_8,levels=c("1","2","3","4"))
data$dly_neurological8.factor = factor(data$dly_neurological8,levels=c("1","2"))
data$dly_infectious_8.factor = factor(data$dly_infectious_8,levels=c("1","2"))
data$dly_cie_other_8.factor = factor(data$dly_cie_other_8,levels=c("1","2","3","4"))
data$daily_sheet_complete.factor = factor(data$daily_sheet_complete,levels=c("0","1","2"))

levels(data$rwe_pw_name_hosp.factor)=c("St. Lukes hospital (ZOMBA)","OMLC-Kapiri hospital (MCHINJI)","Mchinji district hospital (MCHINJI)","Mangochi district hospital (MANGOCHI)","Thyolo district hospital (THYOLO)")
levels(data$rwe_pw_admit_age___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_age___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_age___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_age___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_sex_parent.factor)=c("Male","Female")
levels(data$rwe_pw_preg.factor)=c("Yes","No","Unknown")
levels(data$rwe_pw_hiv_status.factor)=c("New negative","New positive","Test not done")
levels(data$rwe_pw_hiv_child.factor)=c("Yes","No")
levels(data$rwe_pw_hiv_status_2.factor)=c("New negative","New positive","Unrecorded")
levels(data$rwe_pw_admit_diagnosis___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___5.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___6.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___7.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___8.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___9.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___10.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___11.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___12.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___13.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___14.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_diagnosis___15.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_any_tests.factor)=c("Yes","No")
levels(data$rwe_pw_tests_done___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___5.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___6.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_tests_done___7.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_treat_admit_received.factor)=c("Yes","No")
levels(data$rwe_pw_admit_treatment___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___5.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___6.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___7.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___8.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___9.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___10.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___11.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___12.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___13.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___14.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___15.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___16.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___17.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___18.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___19.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___20.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___21.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___22.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___23.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___24.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___25.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___26.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___27.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___28.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___29.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___30.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___31.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___32.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___33.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___34.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___35.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___36.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___37.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___38.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___39.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___40.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___41.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___42.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___43.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___44.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___45.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___46.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___47.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___48.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___49.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___50.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___51.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___52.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___53.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___54.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___55.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___56.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___57.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___58.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___59.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___60.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___61.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___62.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___63.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___64.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___65.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___66.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___67.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___68.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___69.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___70.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___71.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___72.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___73.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___74.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___75.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___76.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___77.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___78.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_admit_treatment___98.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_outcome_disch_hosp.factor)=c("Died","Home based care","Absconded","Referred","Discharged/Home")
levels(data$rwe_pw_disch_diagnosis___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___5.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___6.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___7.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___8.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___9.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___10.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___11.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___12.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_diagnosis___13.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_treat_disch_recvd.factor)=c("Yes","No")
levels(data$rwe_pw_disch_treatment___1.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___2.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___3.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___4.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___5.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___6.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___7.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___8.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___9.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___10.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___11.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___12.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___13.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___14.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___15.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___16.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___17.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___18.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___19.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___20.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___21.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___22.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___23.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___24.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___25.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___26.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___27.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___28.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___29.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___30.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___31.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___32.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___33.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___34.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___35.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___36.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___37.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___38.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___39.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___40.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___41.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___42.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___43.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___44.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___45.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___46.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___47.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___48.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___49.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___50.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___51.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___52.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___53.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___54.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___55.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___56.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___57.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___58.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___59.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___60.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___61.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___62.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___63.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___64.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___65.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___66.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___67.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___68.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___69.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___70.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___71.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___72.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___73.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___74.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___75.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___76.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___77.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___78.factor)=c("Unchecked","Checked")
levels(data$rwe_pw_disch_treatment___98.factor)=c("Unchecked","Checked")
levels(data$paediatrics_ward_medical_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$recru_num_potential_critic.factor)=c("0","1","2","3","4","5","6","7")
levels(data$recru_type_cie.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_2.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_2.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_2.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_2.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_2.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_2.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_3.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_3.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_3.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_3.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_3.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_3.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_4.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_4.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_4.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_4.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_4.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_4.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_5.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_5.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_5.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_5.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_5.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_5.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_6.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_6.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_6.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_6.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_6.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_6.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recru_type_cie_7.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$recru_respiratory_7.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$recru_circulatory_7.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$recru_neurological_7.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$recru_infectious_7.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$recru_cie_other_7.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$recruitment_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$dly_new_critical1.factor)=c("Yes","No","Unknown")
levels(data$dly_new_critical_num.factor)=c("1","2","3","4","5","6","7","8")
levels(data$dly_type_cie1.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_infectious.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_neurological.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_cie_other.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie2.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_2.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_2.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological2.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_2.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_2.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie3.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_3.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_3.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological3.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_3.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_3.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie4.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_4.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_4.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological4.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_4.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_4.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie5.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_5.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_5.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological5.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_5.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_5.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie6.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_6.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_6.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological6.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_6.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_6.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie7.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_7.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_7.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological7.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_7.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_7.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$dly_type_cie8.factor)=c("Respiratory","Circulatory","Neurological","Infectious","Other")
levels(data$dly_respiratory_8.factor)=c("Start or increase of respiratory support: oxygen or CPAP","(Non)Invasive ventilation: bag & mask ventilation or intubation","Start or increase of bronchodilator support","Opening The Airway/Suctioning")
levels(data$dly_circulatory_8.factor)=c("Transfusion of blood (products)","Intravenous fluid bolus of 10ml/kg or more","Start or increase of continuous/intermittent inotropic support (IV/IM adrenalin)","Cardio-pulmonary resuscitation (CPR): resuscitation setting involving chest compressions")
levels(data$dly_neurological8.factor)=c("Decrease in Blantyre Coma Score of 1 point or more","Convulsion requiring anticonvulsants")
levels(data$dly_infectious_8.factor)=c("Sepsis: clinical suspicion of sepsis that has led to the collection of a new blood culture and/or start or change in antibiotic treatment","Start of anti-malarial treatment")
levels(data$dly_cie_other_8.factor)=c("Objectified hypoglycemia requiring correction (IV or enteral)","Unplanned admission to the (P)ICU","Unplanned surgical procedure (including chest drains)","Death")
levels(data$daily_sheet_complete.factor)=c("Incomplete","Unverified","Complete")

# Set numeric variables
numeric <- c("dly_respiratory", paste0("dly_respiratory_",2:8), 
             "dly_circulatory", paste0("dly_circulatory_",2:8),
             "dly_neurological", paste0("dly_neurological",2:8),
             "dly_infectious", paste0("dly_infectious_",2:8),
             "dly_cie_other", paste0("dly_cie_other_",2:8))
data[numeric] <- lapply(data[numeric], as.numeric)

# Save data
saveRDS(data,file="~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300.rds")

### Define data pre- and post-IMPALA ###########################################
# Import data
df <- readRDS("~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300.rds")
str(df)
table(df$rwe_pw_name_hosp.factor)
class(df$rwe_pw_name_hosp.factor)

# Select St. Lukes hospital data
df <- df[df$rwe_pw_name_hosp == 1, ]

# Define data pre- and post-IMPALA
# PRE = Feb 2022 - Jan 2023
# POST = Feb 2023 - Jan 2024

# Create the new variable 'trt'
df$trt <- ifelse(df$rwe_pw_admit_date_hosp >= as.Date("2022-02-01") & 
                  df$rwe_pw_admit_date_hosp <= as.Date("2023-01-31"), 0, 
                ifelse(df$rwe_pw_admit_date_hosp >= as.Date("2023-02-01") & 
                         df$rwe_pw_admit_date_hosp <= as.Date("2024-01-31"), 1, NA))
table(df$trt)
sum(is.na(df$trt))

# Exclude observations with missing values in trt (out of study period)
df <- df[!is.na(df$trt), ]

### Pediatric ward admission (rwe_pw_admit_date_hosp) and discharge dates (rwe_pw_disch_date_hosp) ####
# Number of children
num_unique_record_id <- length(unique(df$rwe_pw_study_id))
print(num_unique_record_id)

# Create variable year
df$year <- as.numeric(format(df$rwe_pw_admit_date_hosp, "%Y"))
table(df$year)

### Rename rwe_pw_study_id to record_id for consistency with other databases ############
df$record_id <- df$rwe_pw_study_id
### Length of pediatric ward stay (rwe_pw_admit_hosp_days) #####################
summary(df$rwe_pw_admit_hosp_days)

### Create variable for month (rwe_adm_month) ##################################
# Create a new variable 'rwe_adm_month' that extracts the month from rwe_pw_admit_hosp_days
df <- df %>%
  mutate(rwe_adm_month = month(rwe_pw_admit_date_hosp))
table(df$rwe_adm_month)
sum(is.na(df$rwe_adm_month))
### TABLE 1 collected diagnosis categories (raw) #####################################
table_1_raw <- matrix(ncol = 3, nrow = 20, dimnames = list(
  c("Age in months, mean (SD)", "Sex, female n (%)", "HIV reactive, n (%)",
    "Admission diagnosis, n (%)", "Malaria", "URTI", "Fracture", "Wound", "Sepsis",
    "Anaemia", "Pneumonia", "Diarrhoea", "Bronchitis", "ARI", "Asthma", 
    "Malnutrition", "Other", "Epilepsy", "Meningitis", "Multimorbidity (>=2)"),
  c("Pre-IMPALA n=930", "Post-IMPALA n=1126", "p-value")
))

# id
df$id <- df$rwe_pw_study_id

# age
age.m  <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, mean),1)
na_subset <- df[is.na(df$rwe_pw_calc_admit_age), ]
summary(df$rwe_pw_calc_admit_age)
hist(df$rwe_pw_calc_admit_age)

# Calculate the mean of age, excluding NA values
mean_age <- mean(df$rwe_pw_calc_admit_age, na.rm = TRUE)
# Replace NA values in age with the mean age
df$rwe_pw_calc_admit_age[is.na(df$rwe_pw_calc_admit_age)] <- mean_age
age.m  <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, mean), 1)
age.sd <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, sd), 1)
age.p  <- t.test(df$rwe_pw_calc_admit_age, df$trt)$p.value
table_1_raw["Age in months, mean (SD)","Pre-IMPALA n=930"] <- paste(age.m[1]," (",age.sd[1],")",sep = "")
table_1_raw["Age in months, mean (SD)","Post-IMPALA n=1126"] <- paste(age.m[2]," (",age.sd[2],")",sep = "")
table_1_raw["Age in months, mean (SD)","p-value"] <- age.p
df$agemons <- df$rwe_pw_calc_admit_age

# sex
sum(is.na(df$rwe_pw_sex_parent))
tb.sex.trt <- table(df$rwe_pw_sex_parent, df$trt) 
prop.tb.sex.trt <- round(prop.table(table(df$rwe_pw_sex_parent, df$trt), margin = 2)*100, 1)
sex.p <- round(chisq.test(tb.sex.trt)$p.value, 2)
table_1_raw["Sex, female n (%)","Pre-IMPALA n=930"] <- paste(tb.sex.trt[1,1]," (",prop.tb.sex.trt[1,1],")",sep = "") 
table_1_raw["Sex, female n (%)","Post-IMPALA n=1126"] <- paste(tb.sex.trt[1,2]," (",prop.tb.sex.trt[1,2],")",sep = "") 
table_1_raw["Sex, female n (%)","p-value"] <- sex.p 
df$sex <- df$rwe_pw_sex_parent

# hiv status
sum(is.na(df$rwe_pw_hiv_status)) # no data
sum(is.na(df$rwe_pw_hiv_child)) # only two missing obs
sum(is.na(df$rwe_pw_hiv_status_2))
df$rwe_pw_hiv_status_2 <- ifelse(df$rwe_pw_hiv_child == 0 & is.na(df$rwe_pw_hiv_status_2), 3, df$rwe_pw_hiv_status_2)
tb.hiv.trt <- table(df$rwe_pw_hiv_status_2, df$trt)
trt_sums <- colSums(tb.hiv.trt)
df$r_hiv <- df$rwe_pw_hiv_status_2

prop.tb.hiv.trt <- round(prop.table(table(df$rwe_pw_hiv_status_2, df$trt), margin = 2)*100,1)
hiv.p <- round(chisq.test(tb.hiv.trt)$p.value, 2)
table_1_raw["HIV reactive, n (%)","Pre-IMPALA n=930"] <- paste(tb.hiv.trt[2,1]," (",prop.tb.hiv.trt[2,1],") n=929",sep = "") 
table_1_raw["HIV reactive, n (%)","Post-IMPALA n=1126"] <- paste(tb.hiv.trt[2,2]," (",prop.tb.hiv.trt[2,2],") n=1125",sep = "") 
table_1_raw["HIV reactive, n (%)","p-value"] <- hiv.p 

# Loop through rwe_pw_admit_diagnosis___ and count NA values
for (i in 1:15) {
  column_name <- paste0("df$rwe_pw_admit_diagnosis___", i)  
  na_count <- sum(is.na(df$rwe_pw_admit_diagnosis___[[column_name]]))  # Count NA values
  cat("Number of NAs in", column_name, ":", na_count, "\n") 
}

# Admission diagnosis: use loop to populate Table 1 rows
diagnosis_vars <- paste0("df$rwe_pw_admit_diagnosis___", 1:15, ".factor")
diagnosis_labels <- c("Malaria", "URTI", "Fracture", "Wound", "Sepsis",
                      "Anaemia", "Pneumonia", "Diarrhoea", "Bronchitis", 
                      "ARI", "Asthma", "Malnutrition", "Other", "Epilepsy", "Meningitis")

for (i in seq_along(diagnosis_vars)) {
  var <- eval(parse(text = diagnosis_vars[i]))
  tb_diag <- table(var, df$trt)
  prop_tb_diag <- round(prop.table(tb_diag, 2) * 100, 1)
  diag_p <- round(chisq.test(tb_diag)$p.value, 3)
  
  table_1_raw[diagnosis_labels[i], "Pre-IMPALA n=930"] <- paste0(tb_diag[2, 1], " (", prop_tb_diag[2, 1], ")")
  table_1_raw[diagnosis_labels[i], "Post-IMPALA n=1126"] <- paste0(tb_diag[2, 2], " (", prop_tb_diag[2, 2], ")")
  table_1_raw[diagnosis_labels[i], "p-value"] <- diag_p
}

diagnosis_vars <- paste0("rwe_pw_admit_diagnosis___", 1:15)
df$multimorb <- rowSums(df[, diagnosis_vars], na.rm = TRUE) 
table(df$multimorb)
df$multimorb <- ifelse(df$multimorb >=2, 1, 0)
table(df$multimorb)


# multimorb
sum(is.na(df$multimorb))
tb.multimorb.trt <- table(df$multimorb, df$trt) 
prop.tb.multimorb.trt <- round(prop.table(table(df$multimorb, df$trt), margin = 2)*100, 1)
multimorb.p <- round(chisq.test(tb.multimorb.trt)$p.value, 2)
table_1_raw["Multimorbidity (>=2)","Pre-IMPALA n=930"] <- paste(tb.multimorb.trt[2,1]," (",prop.tb.multimorb.trt[2,1],")",sep = "") 
table_1_raw["Multimorbidity (>=2)","Post-IMPALA n=1126"] <- paste(tb.multimorb.trt[2,2]," (",prop.tb.multimorb.trt[2,2],")",sep = "") 
table_1_raw["Multimorbidity (>=2)","p-value"] <- multimorb.p

table_1_raw <- as.data.frame(table_1_raw)
write.csv(table_1_raw, "~/2023-IMPALA/cea/RWE/output/Table 1_RWE_rawdiagclass.csv", row.names = TRUE)

### TABLE 1 diagnosis categories (IMPALA) ######################################
# Generate a matrix to display Table 1 results
diagnosis_categories <- c(
  "Hemato/onco", "Respiratory", "Gastro-enteral diseases", "CNS", 
  "Renal-cardiovascular", "Other", "Systemic/Severe infection-inflammation", 
  "Malaria", "SAM/Malnutrition"
)
table_1 <- matrix(ncol = 3, nrow = 14, dimnames = list(
  c("Age in months, mean (SD)", "Sex, female n (%)", "HIV reactive, n (%)",
    "Diagnosis, n (%)", diagnosis_categories, "Multimorbidity (>=2)"),
  c("Pre-IMPALA n=930", "Post-IMPALA n=1126", "p-value")
))

# id
df$id <- df$rwe_pw_study_id

# age
age.m  <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, mean),1)
na_subset <- df[is.na(df$rwe_pw_calc_admit_age), ]
summary(df$rwe_pw_calc_admit_age)
hist(df$rwe_pw_calc_admit_age)

# Calculate the mean of age, excluding NA values
mean_age <- mean(df$rwe_pw_calc_admit_age, na.rm = TRUE)
# Replace NA values in age with the mean age
df$rwe_pw_calc_admit_age[is.na(df$rwe_pw_calc_admit_age)] <- mean_age
age.m  <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, mean), 1)
age.sd <- round(tapply(df$rwe_pw_calc_admit_age, df$trt, sd), 1)
age.p  <- t.test(df$rwe_pw_calc_admit_age, df$trt)$p.value
table_1["Age in months, mean (SD)","Pre-IMPALA n=930"] <- paste(age.m[1]," (",age.sd[1],")",sep = "")
table_1["Age in months, mean (SD)","Post-IMPALA n=1126"] <- paste(age.m[2]," (",age.sd[2],")",sep = "")
table_1["Age in months, mean (SD)","p-value"] <- age.p
df$agemons <- df$rwe_pw_calc_admit_age

# sex
sum(is.na(df$rwe_pw_sex_parent))
tb.sex.trt <- table(df$rwe_pw_sex_parent, df$trt) 
prop.tb.sex.trt <- round(prop.table(table(df$rwe_pw_sex_parent, df$trt), margin = 2)*100, 1)
sex.p <- round(chisq.test(tb.sex.trt)$p.value, 2)
table_1["Sex, female n (%)","Pre-IMPALA n=930"] <- paste(tb.sex.trt[1,1]," (",prop.tb.sex.trt[1,1],")",sep = "") 
table_1["Sex, female n (%)","Post-IMPALA n=1126"] <- paste(tb.sex.trt[1,2]," (",prop.tb.sex.trt[1,2],")",sep = "") 
table_1["Sex, female n (%)","p-value"] <- sex.p 
df$sex <- df$rwe_pw_sex_parent

# hiv status
sum(is.na(df$rwe_pw_hiv_status)) # no data
sum(is.na(df$rwe_pw_hiv_child)) # only two missing obs
sum(is.na(df$rwe_pw_hiv_status_2))
df$rwe_pw_hiv_status_2 <- ifelse(df$rwe_pw_hiv_child == 0 & is.na(df$rwe_pw_hiv_status_2), 3, df$rwe_pw_hiv_status_2)
tb.hiv.trt <- table(df$rwe_pw_hiv_status_2, df$trt)
trt_sums <- colSums(tb.hiv.trt)
df$r_hiv <- df$rwe_pw_hiv_status_2

prop.tb.hiv.trt <- round(prop.table(table(df$rwe_pw_hiv_status_2, df$trt), margin = 2)*100,1)
hiv.p <- round(chisq.test(tb.hiv.trt)$p.value, 2)
table_1["HIV reactive, n (%)","Pre-IMPALA n=930"] <- paste(tb.hiv.trt[2,1]," (",prop.tb.hiv.trt[2,1],") n=929",sep = "") 
table_1["HIV reactive, n (%)","Post-IMPALA n=1126"] <- paste(tb.hiv.trt[2,2]," (",prop.tb.hiv.trt[2,2],") n=1125",sep = "") 
table_1["HIV reactive, n (%)","p-value"] <- hiv.p 

### Diagnosis classification (rwe_pw_admit_diagnosis___) #######################
# Define diagnostic categories consistent with IMPALA categories
# "diag_Hemato_onco"                            "diag_Respiratory"                            
# "diag_Gastro_enteral_diseases"                "diag_CNS"                                    
# "diag_Renal_cardiovascular"                   "diag_Other"                                 
# "diag_Systemic_Severe_infection_inflammation" "diag_Malaria"                               
# "diag_SAM_Malnutrition"  

df$diag_Hemato_onco <- NA
df$diag_Respiratory <- NA
df$diag_Gastro_enteral_diseases <- NA
df$diag_CNS <- NA
df$diag_Renal_cardiovascular <- 0 # not collected in the RWE
df$diag_Other <- NA
df$diag_Systemic_Severe_infection_inflammation <- NA
df$diag_Malaria <- NA
df$diag_SAM_Malnutrition <- NA

# Categorize admission diagnosis based on the dummy variables
df$diag_Hemato_onco[df$rwe_pw_admit_diagnosis___6 == 1] <- 1
df$diag_Hemato_onco <- ifelse(is.na(df$diag_Hemato_onco), 0, df$diag_Hemato_onco)

df$diag_Respiratory[df$rwe_pw_admit_diagnosis___2 == 1 | 
                      df$rwe_pw_admit_diagnosis___7 == 1 |
                      df$rwe_pw_admit_diagnosis___9 == 1 |
                      df$rwe_pw_admit_diagnosis___10 == 1 |
                      df$rwe_pw_admit_diagnosis___11 == 1] <- 1
df$diag_Respiratory <- ifelse(is.na(df$diag_Respiratory), 0, df$diag_Respiratory)

df$diag_Gastro_enteral_diseases[df$rwe_pw_admit_diagnosis___8 == 1] <- 1
df$diag_Gastro_enteral_diseases <- ifelse(is.na(df$diag_Gastro_enteral_diseases), 0, df$diag_Gastro_enteral_diseases)

df$diag_CNS[df$rwe_pw_admit_diagnosis___15 == 1 | 
              df$rwe_pw_admit_diagnosis___14 == 1] <- 1
df$diag_CNS <- ifelse(is.na(df$diag_CNS), 0, df$diag_CNS)

df$diag_Other[df$rwe_pw_admit_diagnosis___3 == 1 | 
                df$rwe_pw_admit_diagnosis___4 == 1 |
                df$rwe_pw_admit_diagnosis___13 == 1] <- 1
df$diag_Other <- ifelse(is.na(df$diag_Other), 0, df$diag_Other)

df$diag_Malaria[df$rwe_pw_admit_diagnosis___1 == 1] <- 1
df$diag_Malaria <- ifelse(is.na(df$diag_Malaria), 0, df$diag_Malaria)

df$diag_Systemic_Severe_infection_inflammation[df$rwe_pw_admit_diagnosis___5 == 1] <- 1
df$diag_Systemic_Severe_infection_inflammation <- ifelse(is.na(df$diag_Systemic_Severe_infection_inflammation), 0, df$diag_Systemic_Severe_infection_inflammation)

df$diag_SAM_Malnutrition[df$rwe_pw_admit_diagnosis___12 == 1] <- 1
df$diag_SAM_Malnutrition <- ifelse(is.na(df$diag_SAM_Malnutrition), 0, df$diag_SAM_Malnutrition)

# Function to compute counts (%) for categorical variables
count_percent <- function(var, df) {
  tb.trt <- table(df[[var]], df$trt) 
  prop.tb.trt <- round(prop.table(tb.trt, margin = 2) * 100, 1)
  paste0(tb.trt[2,1], " (", prop.tb.trt[2,1], ")")
}

for (category in diagnosis_categories) {
  diagnosis_col <- paste0("diag_", gsub("[^A-Za-z0-9]", "_", category))  # Ensure correct column name formatting
  
  # Ensure that the column exists
  if (diagnosis_col %in% colnames(df)) {
    # Check if the variable has more than one unique value
    if (length(unique(df[[diagnosis_col]])) > 1) {
      table_1[category, "Pre-IMPALA n=930"] <- count_percent(diagnosis_col, df %>% filter(trt == 0))
      table_1[category, "Post-IMPALA n=1126"] <- count_percent(diagnosis_col, df %>% filter(trt == 1))
    } else {
      message("Skipping '", diagnosis_col, "' as it has only one category.")
    }
  } else {
    message("Warning: Diagnosis category '", category, "' not found in the dataset.")
  }
}

# Compute p-values for categorical variables using chi-square test
p_value_cat <- function(var, df) {
  # Create contingency table
  tb <- table(df[[var]], df$trt)
  
  # Check for small sample sizes or lack of variation in data
  if (any(dim(tb) == 1)) {
    return(NA)  # Return NA if the table has only one level in either dimension
  }
  
  # Perform chi-square test
  chi_sq <- chisq.test(tb)
  
  # Extract the p-value
  p_value <- chi_sq$p.value
  
  # If p-value is below a threshold (e.g., 0.001), set it to 0.001
  if (p_value < 0.001) {
    return(0.001)
  } else {
    return(round(p_value, 3))  # Round the p-value to 3 decimal places
  }
}

# Assign p-values to the table
table_1["Hemato/onco", "p-value"] <- p_value_cat("diag_Hemato_onco", df)
table_1["Respiratory", "p-value"] <- p_value_cat("diag_Respiratory", df)
table_1["Gastro-enteral diseases", "p-value"] <- p_value_cat("diag_Gastro_enteral_diseases", df)
table_1["CNS", "p-value"] <- p_value_cat("diag_CNS", df)
table_1["Renal-cardiovascular", "p-value"] <- p_value_cat("diag_Renal_cardiovascular", df)
table_1["Other", "p-value"] <- p_value_cat("diag_Other", df)
table_1["Systemic/Severe infection-inflammation", "p-value"] <- p_value_cat("diag_Systemic_Severe_infection_inflammation", df)
table_1["Malaria", "p-value"] <- p_value_cat("diag_Malaria", df)
table_1["SAM/Malnutrition", "p-value"] <- p_value_cat("diag_SAM_Malnutrition", df)

### Multi-morbidity variable ###################################################
# the number of diagnosis per patient
df$multimorb <- rowSums(df[, c("diag_Hemato_onco", "diag_Respiratory", "diag_Gastro_enteral_diseases", 
                               "diag_CNS", "diag_Systemic_Severe_infection_inflammation", 
                               "diag_Malaria", "diag_SAM_Malnutrition", "diag_Other")], na.rm = TRUE) 
table(df$multimorb)
# Check children with multimorbidity == zero
df_subset <- df[df$multimorb == 0, c("record_id", "rwe_pw_admit_other_diagno","diag_Hemato_onco", "diag_Respiratory", "diag_Gastro_enteral_diseases", 
                                     "diag_CNS", "diag_Systemic_Severe_infection_inflammation", 
                                     "diag_Malaria", "diag_SAM_Malnutrition", "diag_Other")] 
table(df$multimorb)
df$multimorb <- ifelse(df$multimorb >=2, 1, 0)
table(df$multimorb)

# multimorb
sum(is.na(df$multimorb))
tb.multimorb.trt <- table(df$multimorb, df$trt) 
prop.tb.multimorb.trt <- round(prop.table(table(df$multimorb, df$trt), margin = 2)*100, 1)
multimorb.p <- round(chisq.test(tb.multimorb.trt)$p.value, 2)
table_1["Multimorbidity (>=2)","Pre-IMPALA n=930"] <- paste(tb.multimorb.trt[2,1]," (",prop.tb.multimorb.trt[2,1],")",sep = "") 
table_1["Multimorbidity (>=2)","Post-IMPALA n=1126"] <- paste(tb.multimorb.trt[2,2]," (",prop.tb.multimorb.trt[2,2],")",sep = "") 
table_1["Multimorbidity (>=2)","p-value"] <- multimorb.p
table_1
table_1 <- as.data.frame(table_1)
write.csv(table_1, "~/2023-IMPALA/cea/RWE/output/Table 1_RWE_newdiagclass.csv", row.names = TRUE)

### Effect outcomes (death, cie, DALY) #########################################
##### Mortality
na_subset <- df[, c("rwe_pw_study_id", "trt","rwe_pw_outcome_disch_hosp", 
                    "dly_cie_other.factor",
                    paste0("dly_cie_other_", 2:8,".factor"))]

table(df$rwe_pw_outcome_disch_hosp, df$trt)
table(df$dly_cie_other.factor)
table(df$dly_cie_other_2.factor)
table(df$dly_cie_other_3.factor)
table(df$dly_cie_other_4.factor)
table(df$dly_cie_other_5.factor)
table(df$dly_cie_other_6.factor)
table(df$dly_cie_other_7.factor)
table(df$dly_cie_other_8.factor)

df$death <- ifelse(df$rwe_pw_outcome_disch_hosp == 2, 1, 0)
table(df$death, df$trt)
table(df$death)

##### CIE
# Number of CIE during hospitalization
table(df$dly_new_critical1.factor, df$trt)
table(df$dly_new_critical1, df$trt)
tapply(df$dly_new_critical_num, df$trt, mean, na.rm = TRUE)
table(df$dly_new_critical_num)
table(df$dly_new_critical_num, df$trt)
sum(is.na(df$dly_new_critical_num)) # missing data
sum(is.na(df$dly_new_critical1.factor)) # missing data

# Replace missing values in dly_new_critical_num if dly_new_critical1 is 'No'
df$dly_new_critical_num <- ifelse(df$dly_new_critical1 == 0, 0, df$dly_new_critical_num)
table(df$dly_new_critical_num, df$trt)
sum(is.na(df$dly_new_critical_num)) # remaining missing data = 603 observations
df$dly_new_critical_num_missing <- ifelse(is.na(df$dly_new_critical_num), 1, 0)
table(df$dly_new_critical_num_missing, df$trt) # still  1 249 354

# Check whether there is complete data in dly_type_cie to fill in 603 missing values.
na_subset <- df[, c("rwe_pw_study_id", "trt","dly_new_critical1.factor", 
                    "dly_new_critical1", "dly_new_critical_num", 
                    paste0("dly_type_cie", 1:8), "dly_respiratory", 
                    "dly_circulatory", "dly_neurological",
                    "dly_infectious",  "dly_cie_other",
                    paste0("dly_respiratory_",2:8),
                    paste0("dly_circulatory_",2:8),
                    paste0("dly_neurological",2:8),
                    paste0("dly_infectious_",2:8),
                    paste0("dly_cie_other_",2:8))]

# Filter rows with NA in dly_new_critical1.factor to check whether there is any
# complete information in dly_* variables that could be used to fill in 
# missing values in the dly_new_critical1.factor
na_cie <- na_subset[is.na(na_subset$dly_new_critical1.factor), ]
# no complete information exists

df$cie <- df$dly_new_critical1


##### DALY
# Calculate YLL (Years of Life Lost)
# The YLL was calculated by subtracting the life expectancy at birth in Malawy (i.e., 62.9 years)
# from the age of death.
df$life_expectancy <- 62.9
df$age_death <- ifelse(df$death == 1, df$rwe_pw_calc_admit_age / 12, NA)
df$YLL <- df$life_expectancy - df$age_death
table(df$YLL)
# Children who did not died had, therefore, zero YLL.
# Replace YLL with 0 if a child hasn't died.
df$YLL <- ifelse(is.na(df$YLL), 0, df$YLL)

# Clean variables to calculate disability weights
# Subset dly_type_cie variables to visually check missing data
dly_type_cie_subset <- df[, c("rwe_pw_study_id", "trt","dly_new_critical1.factor", 
                              "dly_new_critical1", "dly_new_critical_num", 
                              paste0("dly_type_cie", 1:8))]

# Loop through dly_type_cie1 to dly_type_cie8 and count NA values
for (i in 1:8) {
  column_name <- paste0("dly_type_cie", i)  
  na_count <- sum(is.na(dly_type_cie_subset[[column_name]]))  # Count NA values
  cat("Number of NAs in", column_name, ":", na_count, "\n") 
}


# Replace NA in dly_type_cie variables if no cie reported
df$dly_type_cie1 <- ifelse(df$dly_new_critical1 == 0 & is.na(df$dly_type_cie1), 0, df$dly_type_cie1)

df$dly_type_cie2 <- ifelse((df$dly_new_critical1 == 0 |
                                              df$dly_new_critical_num <= 1) & 
                                                is.na(df$dly_type_cie2), 0, 
                                                    df$dly_type_cie2)

df$dly_type_cie3 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 2) & 
                                              is.na(df$dly_type_cie3), 0, 
                                            df$dly_type_cie3)

df$dly_type_cie4 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 3) & 
                                              is.na(df$dly_type_cie4), 0, 
                                            df$dly_type_cie4)

df$dly_type_cie5 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 4) & 
                                              is.na(df$dly_type_cie5), 0, 
                                            df$dly_type_cie5)

df$dly_type_cie6 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 5) & 
                                              is.na(df$dly_type_cie6), 0, 
                                            df$dly_type_cie6)

df$dly_type_cie7 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 6) & 
                                              is.na(df$dly_type_cie7), 0, 
                                            df$dly_type_cie7)

df$dly_type_cie8 <- ifelse((df$dly_new_critical1 == 0 |
                                               df$dly_new_critical_num <= 7) & 
                                              is.na(df$dly_type_cie8), 0, 
                                            df$dly_type_cie8)

# Loop through dly_type_cie1 to dly_type_cie8 and count NA values after cleaning
for (i in 1:8) {
  column_name <- paste0("dly_type_cie", i)  
  na_count <- sum(is.na(df[[column_name]]))  # Count NA values
  cat("Number of NAs in", column_name, ":", na_count, "\n")  
}

# Loop through dly_type_cie1 to dly_type_cie8 and create corresponding 
# d_type_cie1 to d_type_cie8 to be consistent to IMPALA data
for (i in 1:8) {
  old_column <- paste0("dly_type_cie", i)  # Existing column name
  new_column <- paste0("d_type_cie", i)    # New column name
  df[[new_column]] <- df[[old_column]]  # Copy values
}

# Replace 98 with 5 in d_type_cie1 to d_type_cie8 columns
# To be consistent to IMPALA data
for (i in 1:8) {
  column_name <- paste0("d_type_cie", i)  
  df[[column_name]][df[[column_name]] == 98] <- 5  # Replace 98 with 5
}

# Loop through d_type_cie1 to d_type_cie8 and print frequency tables
for (i in 1:8) {
  column_name <- paste0("d_type_cie", i)  
  cat("Frequency table for", column_name, ":\n") 
  print(table(df[[column_name]])) 
  cat("\n")  
}

# Loop through d_type_cie1 to d_type_cie8 to generate a variable including
# disability weights for each Admission diagnosis categorized:
# Respiratory  == 1
# Circulatory  == 2
# Neurological == 3
# Infectious   == 4
# Other        == 5

for (i in 1:8) {
  # Generate the variable name dynamically
  d_type_var <- paste0("d_type_cie", i)
  dw_var <- paste0("dw_", i)
  
  # Initialize the dw_i variable as NA, then set values based on conditions
  df[[dw_var]] <- NA
  df[[dw_var]][df[[d_type_var]] == 1 | df[[d_type_var]] == 4] <- 0.133
  df[[dw_var]][df[[d_type_var]] == 2] <- 0.224
  df[[dw_var]][df[[d_type_var]] == 3] <- 0.552
  df[[dw_var]][df[[d_type_var]] == 4] <- 0.133
  df[[dw_var]][df[[d_type_var]] == 5] <- 0.128
}

# Loop through d_type_cie1 to d_type_cie8 and print frequency tables
for (i in 1:8) {
  column_name <- paste0("dw_", i) 
  cat("Frequency table for", column_name, ":\n")  
  print(table(df[[column_name]])) 
  cat("\n")  
}
# Generate the total disability weight variable as the sum of dw_1 to dw_8
df$t_dw <- rowSums(df[paste0("dw_", 1:8)], na.rm = TRUE)
sum(is.na(df$t_dw))

# Calculate YLD (Years Lived with Disability)
# YLD is the product of the total disability weights (t_dw) and the duration in years (rwe_pw_admit_hosp_days / 365)
df$YLD <- df$t_dw * (df$rwe_pw_admit_hosp_days / 365)

# Calculate DALY (Disability-Adjusted Life Years)
df$DALY <- df$YLL + df$YLD

# Optionally, add a label to the DALY column (not required for calculations, just for documentation purposes)
attr(df$DALY, "label") <- "Disability-Adjusted Life Years"

# View the resulting columns
head(df[c("YLD", "DALY")])
sum(is.na(df$DALY))
sum(is.na(df$dly_new_critical1))
tapply(df$DALY, df$trt, mean)

# if dly_new_critical1 is NA, then DALY should be NA as there is no information on cies during hospitalization
df$DALY <- ifelse(is.na(df$dly_new_critical1), NA, df$DALY)
sum(is.na(df$DALY))

### Select variables of interest and then arrange (df) #########################
df <- df %>%
  select(record_id, trt, agemons, sex, r_hiv,  
         diag_Hemato_onco, diag_Respiratory, diag_Gastro_enteral_diseases,               
         diag_CNS, diag_Renal_cardiovascular, diag_Other, multimorb,      
         diag_Systemic_Severe_infection_inflammation, diag_Malaria, diag_SAM_Malnutrition,
         rwe_pw_admit_date_hosp, rwe_pw_disch_date_hosp, rwe_adm_month, year, rwe_pw_admit_hosp_days, 
         death, cie, DALY, rwe_pw_bill) %>%
  arrange(record_id, trt, agemons, sex, r_hiv, diag_Hemato_onco, diag_Respiratory, 
          diag_Gastro_enteral_diseases, multimorb,              
          diag_CNS, diag_Renal_cardiovascular, diag_Other,      
          diag_Systemic_Severe_infection_inflammation, diag_Malaria, diag_SAM_Malnutrition,
          rwe_pw_admit_date_hosp, rwe_pw_disch_date_hosp, rwe_adm_month, year, rwe_pw_admit_hosp_days, 
          death, cie, DALY, rwe_pw_bill)

### Describe missing data  #####################################################
# Visualize missing data pattern
md.pattern(df, rotate.names = TRUE)

# Select data on observations with missing data in r_hiv (only 2 obs)
selected_data <- df[is.na(df$r_hiv), ]

# View the selected data
head(selected_data)

# Calculate the mean (proportion of 1s) in r_hiv
# Since r_hiv is binary, the mean will represent the proportion of 1s.
mean_r_hiv <- mean(df$r_hiv, na.rm = TRUE)

# Impute missing values with the mean (proportion of 1s)
df$r_hiv[is.na(df$r_hiv)] <- mean_r_hiv
table(df$r_hiv)
df$r_hiv <- round(df$r_hiv)

# Visualize missing data pattern
md.pattern(df, rotate.names = TRUE)

# missing in CIE and DALY
missing_indicator <- lapply(df, function(x) ifelse(is.na(x), 1, 0)) # 1 == missing, 0 == observed
missing_indicator_df <- do.call(cbind,missing_indicator) # convert the list to a data frame
colnames(missing_indicator_df) <- paste0(colnames(df), "_M")
dataset <- cbind(df, missing_indicator_df)
attach(dataset)
t.test(agemons ~ cie_M) # younger children had less missing data
tapply(agemons, cie_M, mean)
chisq.test(table(sex, cie_M)) 
chisq.test(table(multimorb, cie_M)) # more morbidity less missing data
tb <- table(multimorb, cie_M)
prop.table(tb, margin = 2)
chisq.test(table(r_hiv, cie_M)) 
tb <- table(r_hiv, cie_M)
prop.table(tb, margin = 2)
detach(dataset)

### Cost outcomes ##############################################################
### IMPALA COSTS
# Cost Per Year for the whole installation St. Luke's (6 monitors): $4,483.57
# Average children admitted to the paediatric ward yearly at St.Luke's: 1,000
# Cost Per Year for the whole installation Zomba Central (9 monitors): $6,221.43
# Average children admitted to the paediatric ward yearly at Zomba Central: 5,000
# Average costs of the IMPALA monitoring system per child = ($4.48 + $1.24)/ 2 = $2.86
df$c_interv <- 2.86
df$c_interv <- ifelse(df$trt == 0, 0, df$c_interv)
attr(df$c_interv, "label") <- "Intervention costs"

##### DIRECT MEDICAL COSTS BASED ON ST. LUKE'S PATIENT-LEVEL BILL
# Summary days spent at St. Luke's Hospital
summary(df$rwe_pw_admit_hosp_days)
tapply(df$rwe_pw_admit_hosp_days, df$trt, mean)
tapply(df$rwe_pw_admit_hosp_days, df$trt, median)
t.test(df$rwe_pw_admit_hosp_days, df$trt)

# Bill per patient in MWK
summary(df$rwe_pw_bill)
tapply(df$rwe_pw_bill, df$trt, mean)
df$rwe_pw_bill <- ifelse(df$rwe_pw_bill == 9999999, NA, df$rwe_pw_bill)
summary(df$rwe_pw_bill)
tapply(df$rwe_pw_bill, df$trt, mean, na.rm = TRUE)

# Average cost per hospital day in MWK
c_one_hday_mwk <- df$rwe_pw_bill / pmax(df$rwe_pw_admit_hosp_days, 1)  # Prevent division by zero
summary(c_one_hday_mwk)
sd(c_one_hday_mwk, na.rm = TRUE)

# Convert MWK to $ (conversion PPP, 1$ = 382 MWK (2024))
df$bill_dollar <- df$rwe_pw_bill/ 382
summary(df$bill_dollar, na.rm = TRUE)
tapply(df$bill_dollar, df$trt, mean, na.rm = TRUE)

# Average cost per hospital day in $ == $382
c_one_hday_dollar <- df$bill_dollar / pmax(df$rwe_pw_admit_hosp_days, 1)  # Prevent division by zero
summary(c_one_hday_dollar)
sd(c_one_hday_dollar, na.rm = TRUE)

# Cost of one day at St. Luke's hospital
df$c_onedayHDU <- round(mean(c_one_hday_dollar, na.rm = TRUE),0)
df$c_direct <- df$rwe_pw_admit_hosp_days * df$c_onedayHDU
attr(df$c_direct, "label") <- "Direct medical costs $"

##### DIRECT NON-MEDICAL COSTS in $ BASED ON THE CAREGIVERS SURVEY PPP, 1$ = 382 MWK (2024)
df$c_trans <- df$rwe_pw_admit_hosp_days * (22154/ 382)
df$c_food  <- df$rwe_pw_admit_hosp_days * (1181/ 382)
df$c_accom <- df$rwe_pw_admit_hosp_days * (4/ 382)
df$c_other <- df$rwe_pw_admit_hosp_days * (1018/ 382)
df$c_direct_nonm <- df$c_trans + df$c_food + df$c_accom + df$c_other
attr(df$c_direct_nonm, "label") <- "Direct non-medical costs (transp, food, accomodation, other)"

##### INDIRECT COSTS in $ BASED ON THE CAREGIVERS SURVEY PPP, 1$ = 382 MWK (2024)
df$c_absent = df$rwe_pw_admit_hosp_days * (294/ 382)
attr(df$c_absent, "label") <- "Absenteeism costs or indirect costs"

##### TOTAL HOUSEHOLD COSTS
df$c_househ <- df$c_direct_nonm + df$c_absent
attr(df$c_househ, "label") <- "Household costs"

##### TOTAL HEALTHCARE COSTS (healthcare perspective)
df$c_healthcare <- df$c_interv + df$c_direct
attr(df$c_healthcare, "label") <- "Total healthcare costs (healtchare perspective)"

##### TOTAL SOCIETAL COSTS (societal perspective)
df$c_soc <- df$c_healthcare + df$c_househ
attr(df$c_soc, "label") <- "Total societal costs (societal perspective)"

# Save data
saveRDS(df,file="~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300_clean.rds")

### Save data ##################################################################
saveRDS(df,file="~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300_clean.rds")

##### TABLE 2 ##################################################################
table_2 <- matrix(ncol = 4, nrow = 11, dimnames = list(
  c("Effect outcomes", "Mortality, n (%)", "Occurrence of a CIE, n (%)",
    "DALY, mean (SD)", "Cost outcomes, $ mean (SD)", "Intervention costs", 
    "Direct medical costs", "Direct non-medical costs", "Indirect costs",
    "Total healthcare costs", "Total societal costs"),
  c("Pre-IMPALA n=930", "Post-IMPALA n=1126", "Unadjusted difference (95% CI)", "Adjusted difference (95% CI)")
))

# Mortality
t.test(df$death ~ df$trt)
tb.trt <- table(df$trt, df$death) 
prop.tb.trt <- round(prop.table(table(df$trt, df$death), margin = 1)*100, 2)
pr.test <- prop.test(tb.trt)
diff <- round((pr.test$estimate[1] - pr.test$estimate[2])*100, 2)
ci <- round((pr.test$conf.int)*100, 1)
table_2["Mortality, n (%)","Pre-IMPALA n=930"] <- paste(tb.trt[1,2]," (",prop.tb.trt[1,2],")", sep = "") 
table_2["Mortality, n (%)","Post-IMPALA n=1126"] <- paste(tb.trt[2,2]," (",prop.tb.trt[2,2],")", sep = "") 
table_2["Mortality, n (%)","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# CIE
t.test(df$cie ~ df$trt)
tb.trt <- table(df$trt, df$cie) 
prop.tb.trt <- round(prop.table(table(df$trt, df$cie), margin = 1)*100, 1)
pr.test <- prop.test(tb.trt)
diff <- round((pr.test$estimate[1] - pr.test$estimate[2])*100, 1)
ci <- round((pr.test$conf.int)*100, 1)
table_2["Occurrence of a CIE, n (%)","Pre-IMPALA n=930"] <- paste(tb.trt[1,2]," (",prop.tb.trt[1,2],") n=681", sep = "") 
table_2["Occurrence of a CIE, n (%)","Post-IMPALA n=1126"] <- paste(tb.trt[2,2]," (",prop.tb.trt[2,2],") n=772", sep = "") 
table_2["Occurrence of a CIE, n (%)","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# DALY
t.test(df$DALY ~ df$trt)
daly.m  <- round(tapply(df$DALY, df$trt, mean, na.rm = TRUE), 1)
daly.sd <- round(tapply(df$DALY, df$trt, sd, na.rm = TRUE), 1)
test <- t.test(df$DALY ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 1)
ci <- -round(test$conf.int, 1)
table_2["DALY, mean (SD)","Pre-IMPALA n=930"] <- paste(daly.m[1]," (",daly.sd[1],") n=681",sep = "")
table_2["DALY, mean (SD)","Post-IMPALA n=1126"] <- paste(daly.m[2]," (",daly.sd[2],") n=772",sep = "")
table_2["DALY, mean (SD)","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# Count complete observations for each treatment group
complete_obs <- tapply(df$DALY, df$trt, function(x) sum(complete.cases(x)))

# Output the result
complete_obs

# Intervention costs
c.m  <- round(tapply(df$c_interv, df$trt, mean), 0)
c.sd <- round(tapply(df$c_interv, df$trt, sd), 0)
table_2["Intervention costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Intervention costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Intervention costs","Unadjusted difference (95% CI)"] <- paste(c.m[2]," (NA)", sep = "") 

# Direct medical costs
c.m  <- round(tapply(df$c_direct, df$trt, mean), 0)
c.sd <- round(tapply(df$c_direct, df$trt, sd), 0)
test <- t.test(df$c_direct ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 0)
ci <- round(test$conf.int, 0)
table_2["Direct medical costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Direct medical costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Direct medical costs","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# Direct non-medical costs
c.m  <- round(tapply(df$c_direct_nonm, df$trt, mean), 0)
c.sd <- round(tapply(df$c_direct_nonm, df$trt, sd), 0)
test <- t.test(df$c_direct_nonm ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 0)
ci <- round(test$conf.int, 0)
table_2["Direct non-medical costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Direct non-medical costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Direct non-medical costs","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# Indirect costs
c.m  <- round(tapply(df$c_absent, df$trt, mean), 2)
c.sd <- round(tapply(df$c_absent, df$trt, sd), 2)
test <- t.test(df$c_absent ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 2)
ci <- round(test$conf.int, 2)
table_2["Indirect costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Indirect costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Indirect costs","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

# Total healthcare costs
c.m  <- round(tapply(df$c_healthcare, df$trt, mean), 1)
c.sd <- round(tapply(df$c_healthcare, df$trt, sd), 1)
test <- t.test(df$c_healthcare ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 1)
ci <- -round(test$conf.int, 1)
table_2["Total healthcare costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Total healthcare costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Total healthcare costs","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[2],"; ", ci[1],")", sep = "") 

# Total societal costs
c.m  <- round(tapply(df$c_soc, df$trt, mean), 0)
c.sd <- round(tapply(df$c_soc, df$trt, sd), 0)
test <- t.test(df$c_soc ~ df$trt)
diff <- round((test$estimate[2] - test$estimate[1]), 0)
ci <- round(test$conf.int, 0)
table_2["Total societal costs","Pre-IMPALA n=930"] <- paste(c.m[1]," (",c.sd[1],")",sep = "")
table_2["Total societal costs","Post-IMPALA n=1126"] <- paste(c.m[2]," (",c.sd[2],")",sep = "")
table_2["Total societal costs","Unadjusted difference (95% CI)"] <- paste(diff," (",ci[1],"; ", ci[2],")", sep = "") 

table_2 <- as.data.frame(table_2)
write.csv(table_2, "~/2023-IMPALA/cea/RWE/output/Table 2_RWE.csv", row.names = TRUE)

##### TABLE 2: TLME to get adjusted 95% CI cost categories######################
# Load data
dataset <- readRDS("~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300_clean.rds")
dataset <- dataset[, c("record_id", "trt", "agemons", "sex", "r_hiv", "rwe_adm_month",
                       "diag_Hemato_onco", "diag_Respiratory", 
                       "diag_Gastro_enteral_diseases", "diag_CNS", 
                       "diag_Other",      
                       "diag_Systemic_Severe_infection_inflammation", 
                       "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
                       "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
                       "c_absent", "c_househ", "c_healthcare", "c_soc")]


# transform variables as numeric otherwise, models don't converge
numeric <- c("trt", "agemons", "sex", "r_hiv", "rwe_adm_month",
             "diag_Hemato_onco", "diag_Respiratory", 
             "diag_Gastro_enteral_diseases", "diag_CNS", 
             "diag_Other",      
             "diag_Systemic_Severe_infection_inflammation", 
             "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
             "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
             "c_absent", "c_househ", "c_healthcare", "c_soc")

dataset[numeric] <- lapply(dataset[numeric], as.numeric)
dataset <- na.omit(dataset)
dataset <- as.data.frame(dataset)
str(dataset)
table(dataset$trt)
names(dataset)

##### TMLE: Direct medical costs (c_direct) ####################################
# Create a function to run TMLE for costs
TMLE_c <- function(x, i) {
  dataset <- x[i,]
  # COSTS
  W_A <- dataset[,c(2:5,15)] # COVARIATES 
  min.Y <- min(dataset$c_direct)
  max.Y <- max(dataset$c_direct)
  Y.bounded <- (dataset$c_direct - min.Y)/(max.Y - min.Y)
  Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
  Q_A <- as.vector(predict(Q0)$pred)
  W_A1 <- W_A %>% mutate(trt = 1)
  Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
  W_A0 <- W_A %>% mutate(trt = 0)
  Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
  ate_gcomp <- mean(Q_1 - Q_0)
  dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
  A <- as.numeric(dataset$trt)
  W <- dataset[,c(3:6,15)] # COVARIATES
  g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
  g_W <- as.vector(predict(g)$pred)
  H_1 <- 1/g_W
  H_0 <- -1/(1-g_W)
  dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
  glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
  eps <- coef(glm_fit)
  H_A <- dat_tmle$H_A
  Q_A_update <- (Q_A) + eps*H_A
  Q_1_update <- (Q_1) + eps*H_1
  Q_0_update <- (Q_0) + eps*H_0
  tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
  tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
 
  return(c(tmle_atec))
}

# Check if the TMLE_ce function works
initial.time <- Sys.time()
tmle_estimates <- TMLE_c(df)
Sys.time() - initial.time

# Apply boot function
initial.time <- Sys.time()
bootce <- boot(data=df, statistic = TMLE_c, R=1000)
Sys.time() - initial.time

# Extract statistics of interest from the bootce list
cost_diff <- round(bootce[["t0"]], 2)

# Extract the bootstrapped statistics of interest from the bootce list
bootcost_diff <- as.data.frame(bootce[["t"]])
bootcost_diff <- setNames(bootcost_diff, c("bootcost_diff"))

# Extract lower- and upper-level confidence interval limits from the boot function
ci_c <- boot.ci(bootce, type = "perc", index = 1)

LL_cost <- round(ci_c$percent[4], 0)
UL_cost <- round(ci_c$percent[5], 0)

table_2["Direct medical costs","Adjusted difference (95% CI)"] <- paste(cost_diff," (",LL_cost,"; ", UL_cost,")", sep = "") 

save.image(file = "~/2023-IMPALA/cea/RWE/output/95ci_c_direct.RData")

##### TMLE: Direct non-medical costs (c_direct_nonm) ###########################
# Create a function to run TMLE for costs
TMLE_c <- function(x, i) {
  dataset <- x[i,]
  # COSTS
  W_A <- dataset[,c(2:5,15)] # COVARIATES 
  min.Y <- min(dataset$c_direct_nonm)
  max.Y <- max(dataset$c_direct_nonm)
  Y.bounded <- (dataset$c_direct_nonm - min.Y)/(max.Y - min.Y)
  Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
  Q_A <- as.vector(predict(Q0)$pred)
  W_A1 <- W_A %>% mutate(trt = 1)
  Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
  W_A0 <- W_A %>% mutate(trt = 0)
  Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
  ate_gcomp <- mean(Q_1 - Q_0)
  dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
  A <- as.numeric(dataset$trt)
  W <- dataset[,c(3:6,15)] # COVARIATES
  g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
  g_W <- as.vector(predict(g)$pred)
  H_1 <- 1/g_W
  H_0 <- -1/(1-g_W)
  dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
  glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
  eps <- coef(glm_fit)
  H_A <- dat_tmle$H_A
  Q_A_update <- (Q_A) + eps*H_A
  Q_1_update <- (Q_1) + eps*H_1
  Q_0_update <- (Q_0) + eps*H_0
  tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
  tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
  
  return(c(tmle_atec))
}

# Check if the TMLE_ce function works
initial.time <- Sys.time()
tmle_estimates <- TMLE_c(df)
Sys.time() - initial.time

# Apply boot function
initial.time <- Sys.time()
bootce <- boot(data=df, statistic = TMLE_c, R=1000)
Sys.time() - initial.time

# Extract statistics of interest from the bootce list
cost_diff <- round(bootce[["t0"]], 0)

# Extract the bootstrapped statistics of interest from the bootce list
bootcost_diff <- as.data.frame(bootce[["t"]])
bootcost_diff <- setNames(bootcost_diff, c("bootcost_diff"))

# Extract lower- and upper-level confidence interval limits from the boot function
ci_c <- boot.ci(bootce, type = "perc", index = 1)

LL_cost <- round(ci_c$percent[4], 0)
UL_cost <- round(ci_c$percent[5], 0)

table_2["Direct non-medical costs","Adjusted difference (95% CI)"] <- paste(cost_diff," (",LL_cost,"; ", UL_cost,")", sep = "") 

save.image(file = "~/2023-IMPALA/cea/RWE/output/95ci_c_direct_nonm.RData")

##### TMLE: Indirect costs (c_absent) ##########################################
# Create a function to run TMLE for costs
TMLE_c <- function(x, i) {
  dataset <- x[i,]
  # COSTS
  W_A <- dataset[,c(2:5,15)] # COVARIATES 
  min.Y <- min(dataset$c_absent)
  max.Y <- max(dataset$c_absent)
  Y.bounded <- (dataset$c_absent - min.Y)/(max.Y - min.Y)
  Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
  Q_A <- as.vector(predict(Q0)$pred)
  W_A1 <- W_A %>% mutate(trt = 1)
  Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
  W_A0 <- W_A %>% mutate(trt = 0)
  Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
  ate_gcomp <- mean(Q_1 - Q_0)
  dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
  A <- as.numeric(dataset$trt)
  W <- dataset[,c(3:6,15)] # COVARIATES
  g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
  g_W <- as.vector(predict(g)$pred)
  H_1 <- 1/g_W
  H_0 <- -1/(1-g_W)
  dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
  glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
  eps <- coef(glm_fit)
  H_A <- dat_tmle$H_A
  Q_A_update <- (Q_A) + eps*H_A
  Q_1_update <- (Q_1) + eps*H_1
  Q_0_update <- (Q_0) + eps*H_0
  tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
  tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
  
  return(c(tmle_atec))
}

# Check if the TMLE_ce function works
initial.time <- Sys.time()
tmle_estimates <- TMLE_c(df)
Sys.time() - initial.time

# Apply boot function
initial.time <- Sys.time()
bootce <- boot(data=df, statistic = TMLE_c, R=1000)
Sys.time() - initial.time

# Extract statistics of interest from the bootce list
cost_diff <- round(bootce[["t0"]], 2)

# Extract the bootstrapped statistics of interest from the bootce list
bootcost_diff <- as.data.frame(bootce[["t"]])
bootcost_diff <- setNames(bootcost_diff, c("bootcost_diff"))

# Extract lower- and upper-level confidence interval limits from the boot function
ci_c <- boot.ci(bootce, type = "perc", index = 1)

LL_cost <- round(ci_c$percent[4], 2)
UL_cost <- round(ci_c$percent[5], 2)

table_2["Indirect costs","Adjusted difference (95% CI)"] <- paste(cost_diff," (",LL_cost,"; ", UL_cost,")", sep = "") 

save.image(file = "~/2023-IMPALA/cea/RWE/output/95ci_c_absent.RData")

write.csv(table_2, "~/2023-IMPALA/cea/RWE/output/Table 2_RWE.csv", row.names = TRUE)

##### TABLE 3 ##################################################################
table_3 <- matrix(ncol = 10, nrow = 14, dimnames = list(
  c("Main analysis", "Mortality, n(%), HP", "Mortality, n(%), SP", 
    "CIE, n(%), HP", "CIE, n(%), SP", "DALY, mean(SD), HP", "DALY, mean(SD), SP",
    "SA1, COEM", "Mortality, n(%), COEM-HP", "Mortality, n(%), COEM-SP",
    "CIE, n(%), COEM-HP", "CIE, n(%), COEM-SP", "DALY, mean(SD), COEM-HP", "DALY, mean(SD), COEM-SP"), # Fixed row names
  
  c("Effect difference (95% CI)", "Cost difference (95% CI)", "ICER", 
    "NE", "SE", "SW", "NW", "p0", "p224", "p448") # Column names
))

##### TMLE #####################################################################
# Load data
dataset <- readRDS("~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300_clean.rds")
dataset <- dataset[, c("record_id", "trt", "agemons", "sex", "r_hiv", "rwe_adm_month",
                       "diag_Hemato_onco", "diag_Respiratory", 
                       "diag_Gastro_enteral_diseases", "diag_CNS", 
                       "diag_Other",      
                       "diag_Systemic_Severe_infection_inflammation", 
                       "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
                       "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
                       "c_absent", "c_healthcare", "c_soc")]


# transform variables as numeric otherwise, models don't converge
numeric <- c("trt", "agemons", "sex", "r_hiv", "rwe_adm_month",
             "diag_Hemato_onco", "diag_Respiratory", 
             "diag_Gastro_enteral_diseases", "diag_CNS", 
             "diag_Other",      
             "diag_Systemic_Severe_infection_inflammation", 
             "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
             "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
             "c_absent", "c_healthcare", "c_soc")

dataset[numeric] <- lapply(dataset[numeric], as.numeric)
dataset <- na.omit(dataset)
dataset <- as.data.frame(dataset)
str(dataset)
table(dataset$trt)
names(dataset)


##### Main analysis: CEA MORTALITY HEALTHCARE PERSPECTIVE ######################
# Create a function to run TMLE for costs and effects
TMLE_ce <- function(x, i) {
  dataset <- x[i,]
  # COSTS
  W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
  min.Y <- min(dataset$c_healthcare)
  max.Y <- max(dataset$c_healthcare)
  Y.bounded <- (dataset$c_healthcare - min.Y)/(max.Y - min.Y)
  Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
  Q_A <- as.vector(predict(Q0)$pred)
  W_A1 <- W_A %>% mutate(trt = 1)
  Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
  W_A0 <- W_A %>% mutate(trt = 0)
  Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
  ate_gcomp <- mean(Q_1 - Q_0)
  dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
  A <- as.numeric(dataset$trt)
  W <- dataset[,c(3:6,15)] # COVARIATES age, sex, rwe_adm_month, multimorb
  g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
  g_W <- as.vector(predict(g)$pred)
  H_1 <- 1/g_W
  H_0 <- -1/(1-g_W)
  dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
  glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
  eps <- coef(glm_fit)
  H_A <- dat_tmle$H_A
  Q_A_update <- (Q_A) + eps*H_A
  Q_1_update <- (Q_1) + eps*H_1
  Q_0_update <- (Q_0) + eps*H_0
  tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
  tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
  
  # EFFECTS
  W_A <- dataset[,c(2:5,15)] # COVARIATES  
  Y <- (dataset$death)
  Q0 <- SuperLearner(Y = Y, X = W_A, family = binomial(),SL.library = 'SL.glm')
  Q_A <- as.vector(predict(Q0)$pred)
  W_A1 <- W_A %>% mutate(trt = 1)
  Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
  W_A0 <- W_A %>% mutate(trt = 0)
  Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
  ate_gcomp <- mean(Q_1 - Q_0)
  dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
  A <- dataset$trt
  W <- dataset[,c(3:6,15)] # COVARIATES
  g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
  g_W <- as.vector(predict(g)$pred)
  H_1 <- 1/g_W
  H_0 <- -1/(1-g_W)
  dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
  glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
  eps <- coef(glm_fit)
  H_A <- dat_tmle$H_A
  Q_A_update <- (Q_A) + eps*H_A
  Q_1_update <- (Q_1) + eps*H_1
  Q_0_update <- (Q_0) + eps*H_0
  tmle_atee <- mean(Q_1_update - Q_0_update)
  
  return(c(tmle_atec,tmle_atee))
}

# Check if the TMLE_ce function works
set.seed(248)
initial.time <- Sys.time()
tmle_estimates <- TMLE_ce(dataset)
Sys.time() - initial.time

 
# Extract statistics of interest from the bootce list
boot_estimates <- bootce[["t0"]]
boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
cost_diff <- round(boot_estimates["cost_diff"], 0)
effect_diff <- (round(-1 * (boot_estimates["effect_diff"]), 4))*100

# Extract the bootstrapped statistics of interest from the bootce list
postboot <- as.data.frame(bootce[["t"]])
postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100

# Calculate ICER
ICER <- round(cost_diff/effect_diff, 0)

# Covariance matrix per imputed dataset
cov <- cov(postboot)

# Extract lower- and upper-level confidence interval limits from the boot function
ci_c <- boot.ci(bootce, type = "perc", index = 1)
ci_e <- boot.ci(bootce, type = "perc", index = 2)
LL_effect <- (-1 * round(ci_e$percent[5], 4))*100
UL_effect <- (-1 * round(ci_e$percent[4], 4))*100

LL_cost <- round(ci_c$percent[4], 0)
UL_cost <- round(ci_c$percent[5], 0)

# Extract standard errors from the boot function
# in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
se_c <- sd(bootce$t[, 1])
se_e <- sd(bootce$t[, 2])

# CE-PLANE
ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
  geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
  geom_point(data = data.frame(x = effect_diff, y = cost_diff),
             aes(x, y), color = "red", size = 2) +
  labs(x = "Differences in Mortality (%) - St. Luke's pediatric ward (HP)") +
  labs(y = "Differences in costs ($)") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_minimal()
ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_Mortality_HP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)

postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                   NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                   SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                   NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                   SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)

# Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
# according to different willingness-to-pay thresholds
wtp <- seq(0, 600, 112)
INB <- wtp*effect_diff-(cost_diff)
varINB <- wtp^2*se_e + se_c
seINB <- sqrt(varINB)
z <- INB/seINB
CEAC <- as.data.frame(wtp)
CEAC$prob <- pnorm(z,0,1)
CEAC

# Plot CEAC
ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
  geom_line(color = "black", linewidth = 1) +
  ylim(0,1) +
  labs(x = "Willingness-to-pay: incremental costs/ life-saved") +
  labs(y = "Probability of cost-effectiveness") + 
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
  theme_minimal()
ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_Mortality_HP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)

  table_2["Mortality, n (%)","Adjusted difference (95% CI)"]  <- paste(-round(effect_diff, 2), " (", -round(LL_effect, 3), "; ", -round(UL_effect, 3), ")", sep = "")
  table_2["Total healthcare costs","Adjusted difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "") 

  table_3["Mortality, n(%), HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["Mortality, n(%), HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["Mortality, n(%), HP","ICER"] <- ICER 
  table_3["Mortality, n(%), HP","NE"] <- mean(postboot$pNE) 
  table_3["Mortality, n(%), HP","SE"] <- mean(postboot$pSE) 
  table_3["Mortality, n(%), HP","SW"] <- mean(postboot$pSW) 
  table_3["Mortality, n(%), HP","NW"] <- mean(postboot$pNW) 
  table_3["Mortality, n(%), HP","p0"] <- round(CEAC[1,2], 3)
  table_3["Mortality, n(%), HP","p224"] <- round(CEAC[3,2], 3)
  table_3["Mortality, n(%), HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/1-TMLE_CEA_RWE_Mortality_HP.RData")

##### Main analysis: CEA MORTALITY SOCIETAL PERSPECTIVE ########################
  # Create a function to run TMLE for costs and effects
  TMLE_ce <- function(x, i) {
    dataset <- x[i,]
    # COSTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
    min.Y <- min(dataset$c_soc)
    max.Y <- max(dataset$c_soc)
    Y.bounded <- (dataset$c_soc - min.Y)/(max.Y - min.Y)
    Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- as.numeric(dataset$trt)
    W <- dataset[,c(3:6,15)] # COVARIATES age, sex, rwe_adm_month, multimorb
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
    tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
    
    # EFFECTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES  
    Y <- (dataset$death)
    Q0 <- SuperLearner(Y = Y, X = W_A, family = binomial(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- dataset$trt
    W <- dataset[,c(3:6,15)] # COVARIATES
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atee <- mean(Q_1_update - Q_0_update)
    
    return(c(tmle_atec,tmle_atee))
  }
  
  # Check if the TMLE_ce function works
  set.seed(248)
  initial.time <- Sys.time()
  tmle_estimates <- TMLE_ce(dataset)
  Sys.time() - initial.time
  
  # Apply boot function
  initial.time <- Sys.time()
  bootce <- boot(data=dataset, statistic = TMLE_ce, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 0)
  effect_diff <- (round(-1 * (boot_estimates["effect_diff"]), 2))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff, 0)
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 2))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 2))*100
  
  LL_cost <- round(ci_c$percent[4], 0)
  UL_cost <- round(ci_c$percent[5], 0)
  
  # Extract standard errors from the boot function
  # in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x, y), color = "red", size = 2) +
    labs(x = "Differences in Mortality (%) - St. Luke's pediatric ward (SP)") +
    labs(y = "Differences in costs ($)") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_Mortality_SP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
  # according to different willingness-to-pay thresholds
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ life-saved") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_Mortality_SP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)
  
  table_2["Total societal costs","Adjusted difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  
  table_3["Mortality, n(%), SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["Mortality, n(%), SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["Mortality, n(%), SP","ICER"] <- ICER 
  table_3["Mortality, n(%), SP","NE"] <- mean(postboot$pNE) 
  table_3["Mortality, n(%), SP","SE"] <- mean(postboot$pSE) 
  table_3["Mortality, n(%), SP","SW"] <- mean(postboot$pSW) 
  table_3["Mortality, n(%), SP","NW"] <- mean(postboot$pNW) 
  table_3["Mortality, n(%), SP","p0"] <- round(CEAC[1,2], 3)
  table_3["Mortality, n(%), SP","p224"] <- round(CEAC[3,2], 3)
  table_3["Mortality, n(%), SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/2-TMLE_CEA_RWE_Mortality_SP.RData")
  
##### Main analysis: CEA CIE HEALTCHARE PERSPECTIVE ############################
  # Create a function to run TMLE for costs and effects
  TMLE_ce <- function(x, i) {
    dataset <- x[i,]
    # COSTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
    min.Y <- min(dataset$c_healthcare)
    max.Y <- max(dataset$c_healthcare)
    Y.bounded <- (dataset$c_healthcare - min.Y)/(max.Y - min.Y)
    Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- as.numeric(dataset$trt)
    W <- dataset[,c(3:6,15)] # COVARIATES age, sex, rwe_adm_month, multimorb
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
    tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
    
    # EFFECTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES  
    Y <- (dataset$cie)
    Q0 <- SuperLearner(Y = Y, X = W_A, family = binomial(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- dataset$trt
    W <- dataset[,c(3:6,15)] # COVARIATES
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atee <- mean(Q_1_update - Q_0_update)
    
    return(c(tmle_atec,tmle_atee))
  }
  
  # Check if the TMLE_ce function works
  set.seed(248)
  initial.time <- Sys.time()
  tmle_estimates <- TMLE_ce(dataset)
  Sys.time() - initial.time
  
  # Apply boot function
  initial.time <- Sys.time()
  bootce <- boot(data=dataset, statistic = TMLE_ce, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 0)
  effect_diff <- (round(-1 * (boot_estimates["effect_diff"]), 3))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff, 0)
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 3))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 3))*100
  
  LL_cost <- round(ci_c$percent[4], 0)
  UL_cost <- round(ci_c$percent[5], 0)
  
  # Extract standard errors from the boot function
  # in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x, y), color = "red", size = 2) +
    labs(x = "Differences in CIE (%) - St. Luke's pediatric ward (HP) ") +
    labs(y = "Differences in costs ($)") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_CIE_HP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
  # according to different willingness-to-pay thresholds
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ less CIE") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_CIE_HP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)
  
  table_2["Occurrence of a CIE, n (%)","Adjusted difference (95% CI)"] <- paste(-effect_diff, " (", -LL_effect, "; ", -UL_effect, ")", sep = "") 
  
  table_3["CIE, n(%), HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["CIE, n(%), HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["CIE, n(%), HP","ICER"] <- ICER 
  table_3["CIE, n(%), HP","NE"] <- mean(postboot$pNE) 
  table_3["CIE, n(%), HP","SE"] <- mean(postboot$pSE) 
  table_3["CIE, n(%), HP","SW"] <- mean(postboot$pSW) 
  table_3["CIE, n(%), HP","NW"] <- mean(postboot$pNW) 
  table_3["CIE, n(%), HP","p0"] <- round(CEAC[1,2], 3)
  table_3["CIE, n(%), HP","p224"] <- round(CEAC[3,2], 3)
  table_3["CIE, n(%), HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/3-TMLE_CEA_RWE_CIE_HP.RData")
  
##### Main analysis: CEA CIE SOCIETAL PERSPECTIVE ##############################
  # Create a function to run TMLE for costs and effects
  TMLE_ce <- function(x, i) {
    dataset <- x[i,]
    # COSTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
    min.Y <- min(dataset$c_soc)
    max.Y <- max(dataset$c_soc)
    Y.bounded <- (dataset$c_soc - min.Y)/(max.Y - min.Y)
    Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- as.numeric(dataset$trt)
    W <- dataset[,c(3:6,15)] # COVARIATES age, sex, rwe_adm_month, multimorb
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
    tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
    
    # EFFECTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES  
    Y <- (dataset$cie)
    Q0 <- SuperLearner(Y = Y, X = W_A, family = binomial(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- dataset$trt
    W <- dataset[,c(3:6,15)] # COVARIATES
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atee <- mean(Q_1_update - Q_0_update)
    
    return(c(tmle_atec,tmle_atee))
  }
  
  # Check if the TMLE_ce function works
  set.seed(248)
  initial.time <- Sys.time()
  tmle_estimates <- TMLE_ce(dataset)
  Sys.time() - initial.time
  
  # Apply boot function
  initial.time <- Sys.time()
  bootce <- boot(data=dataset, statistic = TMLE_ce, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 0)
  effect_diff <- (round(-1 * (boot_estimates["effect_diff"]), 3))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff, 0)
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 3))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 3))*100
  
  LL_cost <- round(ci_c$percent[4], 0)
  UL_cost <- round(ci_c$percent[5], 0)
  
  # Extract standard errors from the boot function
  # in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x, y), color = "red", size = 2) +
    labs(x = "Differences in CIE (rate)%) - St. Luke's pediatric ward (SP)") +
    labs(y = "Differences in costs ($)") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_CIE_SP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
  # according to different willingness-to-pay thresholds
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ less CIE") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_CIE_SP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)
  
  table_3["CIE, n(%), SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["CIE, n(%), SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["CIE, n(%), SP","ICER"] <- ICER 
  table_3["CIE, n(%), SP","NE"] <- mean(postboot$pNE) 
  table_3["CIE, n(%), SP","SE"] <- mean(postboot$pSE) 
  table_3["CIE, n(%), SP","SW"] <- mean(postboot$pSW) 
  table_3["CIE, n(%), SP","NW"] <- mean(postboot$pNW) 
  table_3["CIE, n(%), SP","p0"] <- round(CEAC[1,2], 3)
  table_3["CIE, n(%), SP","p224"] <- round(CEAC[3,2], 3)
  table_3["CIE, n(%), SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/4-TMLE_CEA_RWE_CIE_SP.RData")
  
##### Main analysis: CEA DALY HEALTHCARE PERSPECTIVE ###########################
  # Create a function to run TMLE for costs and effects
  TMLE_ce <- function(x, i) {
    dataset <- x[i,]
    # COSTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
    min.Y <- min(dataset$c_healthcare)
    max.Y <- max(dataset$c_healthcare)
    Y.bounded <- (dataset$c_healthcare - min.Y)/(max.Y - min.Y)
    Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- as.numeric(dataset$trt)
    W <- dataset[,c(3:6,15)] # COVARIATES age, sex, rwe_adm_month, multimorb
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
    tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
    
    # EFFECTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES  
    Y <- (dataset$DALY)
    Q0 <- SuperLearner(Y = Y, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- dataset$trt
    W <- dataset[,c(3:6,15)] # COVARIATES
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atee <- mean(Q_1_update - Q_0_update)
    
    return(c(tmle_atec,tmle_atee))
  }
  
  # Check if the TMLE_ce function works
  set.seed(248)
  initial.time <- Sys.time()
  tmle_estimates <- TMLE_ce(dataset)
  Sys.time() - initial.time
  
  # Apply boot function
  initial.time <- Sys.time()
  bootce <- boot(data=dataset, statistic = TMLE_ce, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 0)
  effect_diff <- round(-1 * (boot_estimates["effect_diff"]), 2)
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- -1 * (postboot$booteffect_diff)
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff, 0)
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- -1 * round(ci_e$percent[5], 2)
  UL_effect <- -1 * round(ci_e$percent[4], 2)
  
  LL_cost <- round(ci_c$percent[4], 0)
  UL_cost <- round(ci_c$percent[5], 0)
  
  # Extract standard errors from the boot function
  # in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x, y), color = "red", size = 2) +
    labs(x = "DALYs averted - St. Luke's pediatric ward (HP)") +
    labs(y = "Differences in costs ($)") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_DALY_HP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
  # according to different willingness-to-pay thresholds
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ DALYs averted") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_DALY_HP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)
  
  table_2["DALY, mean (SD)","Adjusted difference (95% CI)"] <- paste(-effect_diff, " (", -LL_effect, "; ", -UL_effect, ")", sep = "") 
  
  table_3["DALY, mean(SD), HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["DALY, mean(SD), HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["DALY, mean(SD), HP","ICER"] <- ICER 
  table_3["DALY, mean(SD), HP","NE"] <- mean(postboot$pNE) 
  table_3["DALY, mean(SD), HP","SE"] <- mean(postboot$pSE) 
  table_3["DALY, mean(SD), HP","SW"] <- mean(postboot$pSW) 
  table_3["DALY, mean(SD), HP","NW"] <- mean(postboot$pNW) 
  table_3["DALY, mean(SD), HP","p0"] <- round(CEAC[1,2], 3)
  table_3["DALY, mean(SD), HP","p224"] <- round(CEAC[3,2], 3)
  table_3["DALY, mean(SD), HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/5-TMLE_CEA_RWE_DALY_HP.RData")
  
##### Main analysis: CEA DALY SOCIETAL PERSPECTIVE #############################
  # Create a function to run TMLE for costs and effects
  TMLE_ce <- function(x, i) {
    dataset <- x[i,]
    # COSTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES age, sex, r_hiv, multimorb
    min.Y <- min(dataset$c_soc)
    max.Y <- max(dataset$c_soc)
    Y.bounded <- (dataset$c_soc - min.Y)/(max.Y - min.Y)
    Q0 <- SuperLearner(Y = Y.bounded, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(cost = Y.bounded, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- as.numeric(dataset$trt)
    W <- dataset[,c(3:6,15)] # COVARIATES age, sex, r_hiv, rwe_adm_month, multimorb
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y.bounded ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atec.bounded <- mean(Q_1_update - Q_0_update)
    tmle_atec <- (max.Y - min.Y)*tmle_atec.bounded
    
    # EFFECTS
    W_A <- dataset[,c(2:5,15)] # COVARIATES  
    Y <- (dataset$DALY)
    Q0 <- SuperLearner(Y = Y, X = W_A, family = gaussian(),SL.library = 'SL.glm')
    Q_A <- as.vector(predict(Q0)$pred)
    W_A1 <- W_A %>% mutate(trt = 1)
    Q_1 <- as.vector(predict(Q0,newdata = W_A1)$pred)
    W_A0 <- W_A %>% mutate(trt = 0)
    Q_0 <- as.vector(predict(Q0,newdata = W_A0)$pred)
    ate_gcomp <- mean(Q_1 - Q_0)
    dat_tmle <- tibble(effect = Y, A = dataset$trt, Q_A, Q_0, Q_1)
    A <- dataset$trt
    W <- dataset[,c(3:6,15)] # COVARIATES
    g <- SuperLearner(Y = A, X = W, family = binomial(), SL.library = c("SL.mean","SL.glmnet"))
    g_W <- as.vector(predict(g)$pred)
    H_1 <- 1/g_W
    H_0 <- -1/(1-g_W)
    dat_tmle <- dat_tmle %>% bind_cols(H_1 = H_1, H_0 = H_0) %>% mutate(H_A = case_when(A == 1 ~ H_1, A == 0 ~ H_0))
    glm_fit <- glm(Y ~ -1 + offset(Q_A) + H_A, data = dat_tmle, family = gaussian)
    eps <- coef(glm_fit)
    H_A <- dat_tmle$H_A
    Q_A_update <- (Q_A) + eps*H_A
    Q_1_update <- (Q_1) + eps*H_1
    Q_0_update <- (Q_0) + eps*H_0
    tmle_atee <- mean(Q_1_update - Q_0_update)
    
    return(c(tmle_atec,tmle_atee))
  }
  
  # Check if the TMLE_ce function works
  set.seed(248)
  initial.time <- Sys.time()
  tmle_estimates <- TMLE_ce(dataset)
  Sys.time() - initial.time
  
  # Apply boot function
  initial.time <- Sys.time()
  bootce <- boot(data=dataset, statistic = TMLE_ce, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 0)
  effect_diff <- round(-1 * (boot_estimates["effect_diff"]), 2)
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- -1 * (postboot$booteffect_diff)
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff, 0)
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- -1 * round(ci_e$percent[5], 2)
  UL_effect <- -1 * round(ci_e$percent[4], 2)
  
  LL_cost <- round(ci_c$percent[4], 0)
  UL_cost <- round(ci_c$percent[5], 0)
  
  # Extract standard errors from the boot function
  # in the context of bootstrap replicates, sd is referred to as the bootstrap standard error (SE).
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ce_plane_plot <- ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(x = booteffect_diff, y = bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x, y), color = "red", size = 2) +
    labs(x = "DALYs averted - St. Luke's pediatric ward (SP)") +
    labs(y = "Differences in costs ($)") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEplane_DALY_SP.png", plot = ce_plane_plot, width = 8, height = 6, dpi = 300)
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach for calculating probabilities of cost-effectiveness
  # according to different willingness-to-pay thresholds
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ceac_plane_plot <- ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ DALYs averted") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  ggsave("~/2023-IMPALA/cea/RWE/output/CEAC_DALY_SP.png", plot = ceac_plane_plot, width = 8, height = 6, dpi = 300)
  
  table_3["DALY, mean(SD), SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["DALY, mean(SD), SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["DALY, mean(SD), SP","ICER"] <- ICER 
  table_3["DALY, mean(SD), SP","NE"] <- mean(postboot$pNE) 
  table_3["DALY, mean(SD), SP","SE"] <- mean(postboot$pSE) 
  table_3["DALY, mean(SD), SP","SW"] <- mean(postboot$pSW) 
  table_3["DALY, mean(SD), SP","NW"] <- mean(postboot$pNW) 
  table_3["DALY, mean(SD), SP","p0"] <- round(CEAC[1,2], 3)
  table_3["DALY, mean(SD), SP","p224"] <- round(CEAC[3,2], 3)
  table_3["DALY, mean(SD), SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/6-TMLE_CEA_RWE_DALY_SP.RData")
  
##### COEM #####################################################################
  # Coarsened Exact MAtching = COEM
  dataset <- readRDS("~/2023-IMPALA/cea/RWE/data/IMPALARWEMalawiPAEDI_DATA_2024-11-01_1300_clean.rds")
  dataset <- dataset[, c("record_id", "trt", "agemons", "sex", "r_hiv", "rwe_adm_month",
                         "diag_Hemato_onco", "diag_Respiratory", 
                         "diag_Gastro_enteral_diseases", "diag_CNS", 
                         "diag_Other",      
                         "diag_Systemic_Severe_infection_inflammation", 
                         "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
                         "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
                         "c_absent", "c_healthcare", "c_soc")]
  
  # Apply COEM
  # Restricting the matching solution to a k-to-k match (k2k=TRUE)
  # matching on: age, sex, r_hiv, rwe_adm_month, multimorb
  set.seed(164)
  mat <- cem(treatment = "trt", data = dataset, cutpoints = list(agemons = 3),
             drop = c("record_id", "diag_Hemato_onco", "diag_Respiratory", 
                      "diag_Gastro_enteral_diseases", "diag_CNS", 
                      "diag_Other",      
                      "diag_Systemic_Severe_infection_inflammation", 
                      "diag_Malaria", "diag_SAM_Malnutrition", "multimorb",
                      "death", "cie", "DALY", "c_interv", "c_direct", "c_direct_nonm", 
                      "c_absent", "c_healthcare", "c_soc"), k2k=TRUE)
  mat
  
  # Extract the matched data
  matched_data <- dataset[mat$matched, ]
  table(matched_data$trt)
  md.pattern(matched_data)
  write.csv(matched_data, file = "~/2023-IMPALA/cea/RWE/data/cem_matched_RWE_data.csv", row.names = FALSE)
  saveRDS(matched_data,file="~/2023-IMPALA/cea/RWE/data/cem_matched_RWE_data.rds")
  matched_data <- read_csv("~/2023-IMPALA/cea/RWE/data/cem_matched_RWE_data.csv")
  
##### SA1: CEA COEM MORTALITY HEALTHCARE PERSPECTIVE ###########################  
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_healthcare ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- death ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 2)
  effect_diff <- (-1 * round(boot_estimates["effect_diff"], 4))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 4))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 4))*100
  
  LL_cost <- round(ci_c$percent[4])
  UL_cost <- round(ci_c$percent[5])
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in Mortality rate") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ life-saved") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["Mortality, n(%), COEM-HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["Mortality, n(%), COEM-HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["Mortality, n(%), COEM-HP","ICER"] <- ICER 
  table_3["Mortality, n(%), COEM-HP","NE"] <- mean(postboot$pNE) 
  table_3["Mortality, n(%), COEM-HP","SE"] <- mean(postboot$pSE) 
  table_3["Mortality, n(%), COEM-HP","SW"] <- mean(postboot$pSW) 
  table_3["Mortality, n(%), COEM-HP","NW"] <- mean(postboot$pNW) 
  table_3["Mortality, n(%), COEM-HP","p0"] <- round(CEAC[1,2], 3)
  table_3["Mortality, n(%), COEM-HP","p224"] <- round(CEAC[3,2], 3)
  table_3["Mortality, n(%), COEM-HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/7-COEM_CEA_RWE_Mortality_HP.RData")

##### SA1: CEA COEM MORTALITY SOCIETAL PERSPECTIVE #############################
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_soc ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- death ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"])
  effect_diff <- (-1 * round(boot_estimates["effect_diff"], 4))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 4))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 4))*100
  
  LL_cost <- round(ci_c$percent[4])
  UL_cost <- round(ci_c$percent[5])
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in Mortality rate") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ life-saved") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["Mortality, n(%), COEM-SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["Mortality, n(%), COEM-SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["Mortality, n(%), COEM-SP","ICER"] <- ICER 
  table_3["Mortality, n(%), COEM-SP","NE"] <- mean(postboot$pNE) 
  table_3["Mortality, n(%), COEM-SP","SE"] <- mean(postboot$pSE) 
  table_3["Mortality, n(%), COEM-SP","SW"] <- mean(postboot$pSW) 
  table_3["Mortality, n(%), COEM-SP","NW"] <- mean(postboot$pNW) 
  table_3["Mortality, n(%), COEM-SP","p0"] <- round(CEAC[1,2], 3)
  table_3["Mortality, n(%), COEM-SP","p224"] <- round(CEAC[3,2], 3)
  table_3["Mortality, n(%), COEM-SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/8-COEM_CEA_RWE_Mortality_SP.RData")
  
##### SA1: CEA COEM CIE HEALTHCARE PERSPECTIVE #################################
  # Keep only complete data (no missing values) in matched_data
  matched_data <- matched_data[complete.cases(matched_data), ]
  
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_healthcare ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- cie ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 2)
  effect_diff <- (-1 * round(boot_estimates["effect_diff"], 3))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 3))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 3))*100
  
  LL_cost <- round(ci_c$percent[4], 2)
  UL_cost <- round(ci_c$percent[5], 2)
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in CIE rate") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*(effect_diff)-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ less CIE") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["CIE, n(%), COEM-HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["CIE, n(%), COEM-HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["CIE, n(%), COEM-HP","ICER"] <- ICER 
  table_3["CIE, n(%), COEM-HP","NE"] <- mean(postboot$pNE) 
  table_3["CIE, n(%), COEM-HP","SE"] <- mean(postboot$pSE) 
  table_3["CIE, n(%), COEM-HP","SW"] <- mean(postboot$pSW) 
  table_3["CIE, n(%), COEM-HP","NW"] <- mean(postboot$pNW) 
  table_3["CIE, n(%), COEM-HP","p0"] <- round(CEAC[1,2], 3)
  table_3["CIE, n(%), COEM-HP","p224"] <- round(CEAC[3,2], 3)
  table_3["CIE, n(%), COEM-HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/9-COEM_CEA_RWE_CIE_HP.RData")
  
##### SA1: CEA COEM CIE SOCIETAL PERSPECTIVE ###################################
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_soc ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- cie ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"])
  effect_diff <- (-1 * round(boot_estimates["effect_diff"], 3))*100
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- (-1 * (postboot$booteffect_diff))*100
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- (-1 * round(ci_e$percent[5], 3))*100
  UL_effect <- (-1 * round(ci_e$percent[4], 3))*100
  
  LL_cost <- round(ci_c$percent[4])
  UL_cost <- round(ci_c$percent[5])
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in CIE rate") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ less CIE") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["CIE, n(%), COEM-SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["CIE, n(%), COEM-SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["CIE, n(%), COEM-SP","ICER"] <- ICER 
  table_3["CIE, n(%), COEM-SP","NE"] <- mean(postboot$pNE) 
  table_3["CIE, n(%), COEM-SP","SE"] <- mean(postboot$pSE) 
  table_3["CIE, n(%), COEM-SP","SW"] <- mean(postboot$pSW) 
  table_3["CIE, n(%), COEM-SP","NW"] <- mean(postboot$pNW) 
  table_3["CIE, n(%), COEM-SP","p0"] <- round(CEAC[1,2], 3)
  table_3["CIE, n(%), COEM-SP","p224"] <- round(CEAC[3,2], 3)
  table_3["CIE, n(%), COEM-SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/10-COEM_CEA_RWE_CIE_SP.RData")
  
##### SA1: CEA COEM DALY HEALTHCARE PERSPECTIVE ################################
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_healthcare ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- DALY ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"])
  effect_diff <- -1 * round(boot_estimates["effect_diff"], 2)
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- -1 * (postboot$booteffect_diff)
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- -1 * round(ci_e$percent[5], 2)
  UL_effect <- -1 * round(ci_e$percent[4], 2)
  
  LL_cost <- round(ci_c$percent[4])
  UL_cost <- round(ci_c$percent[5])
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in DALYs") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ DALYs averted") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["DALY, mean(SD), COEM-HP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["DALY, mean(SD), COEM-HP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["DALY, mean(SD), COEM-HP","ICER"] <- ICER 
  table_3["DALY, mean(SD), COEM-HP","NE"] <- mean(postboot$pNE) 
  table_3["DALY, mean(SD), COEM-HP","SE"] <- mean(postboot$pSE) 
  table_3["DALY, mean(SD), COEM-HP","SW"] <- mean(postboot$pSW) 
  table_3["DALY, mean(SD), COEM-HP","NW"] <- mean(postboot$pNW) 
  table_3["DALY, mean(SD), COEM-HP","p0"] <- round(CEAC[1,2], 3)
  table_3["DALY, mean(SD), COEM-HP","p224"] <- round(CEAC[3,2], 3)
  table_3["DALY, mean(SD), COEM-HP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/11-COEM_CEA_RWE_DALY_HP.RData")
  
##### SA1: CEA COEM DALY SOCIETAL PERSPECTIVE ##################################
  # Run the SUR model on the matched data
  fsur <- function(x, i){
    matched_data <- x[i,]
    r1 <- c_soc ~ trt + agemons + sex + r_hiv + multimorb
    r2 <- DALY ~ trt + agemons + sex + r_hiv + multimorb
    fitsur <- systemfit(list(costreg = r1, effectreg = r2), "SUR", data=matched_data)
    betas <- fitsur$coefficients
    return(c(betas[["costreg_trt"]], betas[["effectreg_trt"]])) # please note that the position of the coefficient of interest changes with the number of covariates
  }
  
  # Check if the fsur function works
  initial.time <- Sys.time()
  coem_estimates <- fsur(matched_data)
  Sys.time() - initial.time
  
  # Apply boot function
  set.seed(715)
  initial.time <- Sys.time()
  bootce <- boot(data=matched_data, statistic=fsur, R=1000)
  Sys.time() - initial.time
  
  # Extract statistics of interest from the bootce list
  boot_estimates <- bootce[["t0"]]
  boot_estimates <- setNames(boot_estimates,c("cost_diff","effect_diff"))
  cost_diff <- round(boot_estimates["cost_diff"], 2)
  effect_diff <- -1 * round(boot_estimates["effect_diff"], 2)
  
  # Extract the bootstrapped statistics of interest from the bootce list
  postboot <- as.data.frame(bootce[["t"]])
  postboot <- setNames(postboot, c("bootcost_diff","booteffect_diff"))
  postboot$booteffect_diff <- -1 * (postboot$booteffect_diff)
  
  # Calculate ICER
  ICER <- round(cost_diff/effect_diff)
  ICER
  
  # Covariance matrix per imputed dataset
  cov <- cov(postboot)
  
  # Extract lower- and upper-level confidence interval limits from the boot function
  ci_c <- boot.ci(bootce, type = "perc", index = 1)
  ci_e <- boot.ci(bootce, type = "perc", index = 2)
  LL_effect <- -1 * round(ci_e$percent[5], 2)
  UL_effect <- -1 * round(ci_e$percent[4], 2)
  
  LL_cost <- round(ci_c$percent[4], 2)
  UL_cost <- round(ci_c$percent[5], 2)
  
  # Extract standard errors from the boot function
  se_c <- sd(bootce$t[, 1])
  se_e <- sd(bootce$t[, 2])
  
  # CE-PLANE
  ggplot(data = postboot, aes(x = booteffect_diff, y = bootcost_diff)) +
    geom_pointdensity(aes(-booteffect_diff, bootcost_diff), size = 2, alpha = 0.75, show.legend = FALSE, adjust = 0.05) +
    geom_point(data = data.frame(x = effect_diff, y = cost_diff),
               aes(x,y), color = "red", size = 2) +
    labs(x = "Differences in DALYs") +
    labs(y = "Differences in costs") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal()
  
  postboot <- mutate(postboot, bootICER = bootcost_diff/booteffect_diff,
                     NE = ifelse(bootICER > 0 & bootcost_diff > 0, 1, 0),
                     SE = ifelse(bootICER < 0 & bootcost_diff < 0, 1, 0),
                     NW = ifelse(bootICER < 0 & bootcost_diff > 0, 1, 0),
                     SW = ifelse(bootICER > 0 & bootcost_diff < 0, 1, 0))
  postboot$pNE <- round(sum(postboot$NE)/nrow(postboot)*100,) 
  postboot$pSE <- round(sum(postboot$SE)/nrow(postboot)*100,) 
  postboot$pSW <- round(sum(postboot$SW)/nrow(postboot)*100,) 
  postboot$pNW <- round(sum(postboot$NW)/nrow(postboot)*100,)
  
  # Incremental Net Benefit approach using Rubin's rules
  wtp <- seq(0, 600, 112)
  INB <- wtp*effect_diff-(cost_diff)
  varINB <- wtp^2*se_e + se_c
  seINB <- sqrt(varINB)
  z <- INB/seINB
  CEAC <- as.data.frame(wtp)
  CEAC$prob <- pnorm(z,0,1)
  CEAC
  
  # Plot CEAC
  ggplot(data = CEAC, aes(x = wtp, y = prob)) +
    geom_line(color = "black", linewidth = 1) +
    ylim(0,1) +
    labs(x = "Willingness-to-pay: incremental costs/ DALYs averted") +
    labs(y = "Probability of cost-effectiveness") + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = as.numeric(448), linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = c(seq(0, 560, by = 112), 448)) +
    theme_minimal()
  
  table_3["DALY, mean(SD), COEM-SP","Effect difference (95% CI)"] <- paste(effect_diff, " (", LL_effect, "; ", UL_effect, ")", sep = "")
  table_3["DALY, mean(SD), COEM-SP","Cost difference (95% CI)"] <- paste(cost_diff, " (", LL_cost, "; ", UL_cost, ")", sep = "")
  table_3["DALY, mean(SD), COEM-SP","ICER"] <- ICER 
  table_3["DALY, mean(SD), COEM-SP","NE"] <- mean(postboot$pNE) 
  table_3["DALY, mean(SD), COEM-SP","SE"] <- mean(postboot$pSE) 
  table_3["DALY, mean(SD), COEM-SP","SW"] <- mean(postboot$pSW) 
  table_3["DALY, mean(SD), COEM-SP","NW"] <- mean(postboot$pNW) 
  table_3["DALY, mean(SD), COEM-SP","p0"] <- round(CEAC[1,2], 3)
  table_3["DALY, mean(SD), COEM-SP","p224"] <- round(CEAC[3,2], 3)
  table_3["DALY, mean(SD), COEM-SP","p448"] <- round(CEAC[5,2], 3) 
  
  save.image(file = "~/2023-IMPALA/cea/RWE/output/12-COEM_CEA_RWE_DALY_SP.RData")

##### SAVE TABLE 3 ######   
  table_2 <- as.data.frame(table_2)
  write.csv(table_2, "~/2023-IMPALA/cea/RWE/output/Table 2_RWE.csv", row.names = TRUE)
  table_3 <- as.data.frame(table_3)
  write.csv(table_3, "~/2023-IMPALA/cea/RWE/output/Table 3_RWE.csv", row.names = TRUE)
  