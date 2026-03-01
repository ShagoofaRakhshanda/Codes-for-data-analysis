install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("writexl")
library(writexl)
install.packages("caret")
library(caret)
install.packages("epiR")
library(epiR)
install.packages("purrr")
library(purrr)
install.packages("boot")
library(boot)




sheet1 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Gold standard working_1.xlsx")
sheet2 <- read_excel("C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/v5/Demographics.xlsx")
sheet3 <- read_excel("C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/v5/Medications.xlsx")
sheet4 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Final count Gold standard.xlsx")
sheet5 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/v5.5_425only/Demographics.xlsx")
sheet6 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Age.xlsx")
sheet7 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Age.xlsx")
sheet8 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Gold standard demographics.xlsx")
sheet9 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Gold standard age data.xlsx")

merged_data <- inner_join(sheet1, sheet2, by = "Patient_UUID")
View(merged_data)

cross_tab <- table(merged_data$Final)
print(cross_tab)

num_unique_patients <- n_distinct(sheet1$Patient_UUID, sheet1$Final)
print(num_unique_patients)

num_unique_patients <- n_distinct(sheet1$Patient_UUID)
print(num_unique_patients)

unique_id_df <- sheet1 %>%
  group_by(Patient_UUID) %>%
  summarise(Final = ifelse(any(Final == "Yes"), "Yes", "No"), .groups = "drop")
print(unique_id_df)
View(unique_id_df)

write_xlsx(unique_id_df, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Final count Gold standard.xlsx")

unique_id_df %>%
  count(Final)

merged_data <- inner_join(sheet4, sheet2, by = "Patient_UUID")
View(merged_data)

merged_data2 <- inner_join(sheet4, sheet5, by = "Patient_UUID")
View(merged_data2)

Gold_standard_demographics <- rbind(merged_data, merged_data2)
View(Gold_standard_demographics)

write_xlsx(Gold_standard_demographics, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Gold standard demographics.xlsx")

cross_tab <- table (Gold_standard_demographics$Final, Gold_standard_demographics$)
print (cross_tab)


first_visits <- sheet6 %>%
  group_by(Patient_UUID) %>%
  summarise(first_visit = min(VisitDate, na.rm = TRUE))
View (first_visits)

write_xlsx(first_visits, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Age.xlsx")

merged_data3 <- inner_join(sheet7, sheet8, by = "Patient_UUID")
View(merged_data3)

write_xlsx(merged_data3, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Gold standard age.xlsx")

Age_data <- merged_data3 %>%
  mutate(
    DateOfBirth = as.Date(DateOfBirth),
    first_visit = as.Date(first_visit),
    age = as.numeric(difftime(first_visit, DateOfBirth, units = "days")) / 365
  )

View(Age_data)

write_xlsx(Age_data, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Gold standard age data.xlsx")

sheet9 %>%
  group_by(Final) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))

summary_table <- sheet9 %>%
  group_by(Final) %>%
  summarise(
    mean_age = round(mean(age, na.rm = TRUE), 1),
    sd_age = round(sd(age, na.rm = TRUE), 1),
    n = n(),
    .groups = "drop"
  )

summary_table

sheet9$age_group <- cut(
  sheet9$age,
  breaks = c(25, 35, 45, 55, 65, 75, 85, 95, Inf),
  labels = c("<35", "35–44", "45–54", "55–64", "65–74", "75–84", "85–94", "95+"),
  right = FALSE
)

table(sheet9$Final, sheet9$age_group)

View (sheet9)

#Validity measurements

## Finding TP, TN, FP, FN

sheet10 <- read_excel("C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/SI_Predicted_v2.xlsx")
View(sheet10)

common_count_TP <- length(intersect(sheet10$Final_Yes, sheet10$`Predicted -Yes-USA-SIMs-HER-27`))
common_count_TP

common_count_FP <- length(intersect(sheet10$Final_No, sheet10$`Predicted -Yes-NHS-UK-50`))
common_count_FP

common_count <- intersect(sheet10$Final_Yes, sheet10$`Predicted -Yes-NHS-UK-50`)
common_count

common_count <- intersect(sheet10$Final_No, sheet10$`Predicted -Yes-NHS-UK-50`)
common_count

FN <- length(setdiff(sheet10$Final_Yes, sheet10$`Predicted -Yes-NHS-UK-50`))
FN

TN <- length(setdiff(sheet10$Final_No, sheet10$`Predicted -Yes-NHS-UK-50`))
TN

TN <- setdiff(sheet10$Final_No, sheet10$`Predicted -Yes-Singapore EHR-B-1270`)
TN









df <- data.frame(
  Algorithm_name = c("Minnesota CRB", "USA SIMs", "Japan SIMs", "Singapore SIMs A", "Singapore SIMs B", "Singapore SIMs C", "Singapore SIMs D", "Japan SAMT", "NHS UK"),
  TP = c(140, 14, 23, 22, 738, 24, 23, 16, 27),
  FP = c(111, 13, 11, 12, 532, 21, 16, 5, 23),
  TN = c(464, 562, 564, 563, 43, 554, 559, 570, 552),
  FN = c(654, 780, 771, 772, 56, 770, 771, 778, 767)
)

df_summary <- df %>%
  mutate(
    Accuracy = (TP + TN) / (TP + FP + TN + FN),
    Sensitivity = TP / (TP + FN),
    Specificity = TN / (TN + FP),
    PPV = TP / (TP + FP),
    NPV = TN / (TN + FN)
  )

df_summary


#Sensitivity
sensitivity_df <- df %>%
  rowwise() %>%
  mutate(
    Sensitivity = TP / (TP + FN),
    CI = list(binom.test(TP, TP + FN)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    Sens_Lower = map_dbl(CI, 1),
    Sens_Upper = map_dbl(CI, 2),
    Sensitivity = round(Sensitivity * 100, 2),
    Sens_Lower = round(Sens_Lower * 100, 2),
    Sens_Upper = round(Sens_Upper * 100, 2),
    Sensitivity_CI = paste0(Sensitivity, "% (", Sens_Lower, "–", Sens_Upper, "%)")
  ) %>%
  select(Algorithm_name, Sensitivity_CI)

print(sensitivity_df)

#Specificity
specificity_df <- df %>%
  rowwise() %>%
  mutate(
    Specificity = TN / (TN + FP),
    CI = list(binom.test(TN, TN + FP)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    Spec_Lower = map_dbl(CI, 1),
    Spec_Upper = map_dbl(CI, 2),
    Specificity = round(Specificity * 100, 2),
    Spec_Lower = round(Spec_Lower * 100, 2),
    Spec_Upper = round(Spec_Upper * 100, 2),
    Specificity_CI = paste0(Specificity, "% (", Spec_Lower, "–", Spec_Upper, "%)")
  ) %>%
  select(Algorithm_name, Specificity_CI)

print(specificity_df)

#PPV
ppv_df <- df %>%
  rowwise() %>%
  mutate(
    PPV = TP / (TP + FP),
    CI = list(binom.test(TP, TP + FP)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    PPV_Lower = map_dbl(CI, 1),
    PPV_Upper = map_dbl(CI, 2),
    PPV = round(PPV * 100, 2),
    PPV_Lower = round(PPV_Lower * 100, 2),
    PPV_Upper = round(PPV_Upper * 100, 2),
    PPV_CI = paste0(PPV, "% (", PPV_Lower, "–", PPV_Upper, "%)")
  ) %>%
  select(Algorithm_name, PPV_CI)

print(ppv_df)

#NPV
npv_df <- df %>%
  rowwise() %>%
  mutate(
    NPV = TN / (TN + FN),
    CI = list(binom.test(TN, TN + FN)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    NPV_Lower = map_dbl(CI, 1),
    NPV_Upper = map_dbl(CI, 2),
    NPV = round(NPV * 100, 2),
    NPV_Lower = round(NPV_Lower * 100, 2),
    NPV_Upper = round(NPV_Upper * 100, 2),
    NPV_CI = paste0(NPV, "% (", NPV_Lower, "–", NPV_Upper, "%)")
  ) %>%
  select(Algorithm_name, NPV_CI)

print(npv_df)

#Accuracy
accuracy_df <- df %>%
  rowwise() %>%
  mutate(
    correct = TP + TN,
    total = TP + FP + TN + FN,
    Accuracy = correct / total,
    CI = list(binom.test(correct, total)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    Acc_Lower = map_dbl(CI, 1),
    Acc_Upper = map_dbl(CI, 2),
    Accuracy = round(Accuracy * 100, 2),
    Acc_Lower = round(Acc_Lower * 100, 2),
    Acc_Upper = round(Acc_Upper * 100, 2),
    Accuracy_CI = paste0(Accuracy, "% (", Acc_Lower, "–", Acc_Upper, "%)")
  ) %>%
  select(Algorithm_name, Accuracy_CI)

print(accuracy_df)

#Balanced Accuracy
ba_accuracy_df <- df %>%
  rowwise() %>%
  mutate(
    Sensitivity = TP / (TP + FN),
    Specificity = TN / (TN + FP),
    BA_Accuracy = (Sensitivity + Specificity) / 2,
    SE_BA = sqrt((Sensitivity*(1-Sensitivity) + Specificity*(1-Specificity)) / 4),
    BA_Lower = BA_Accuracy - 1.96 * SE_BA,
    BA_Upper = BA_Accuracy + 1.96 * SE_BA,
    Sensitivity = round(Sensitivity*100,2),
    Specificity = round(Specificity*100,2),
    BA_Accuracy = round(BA_Accuracy*100,2),
    BA_Lower = round(BA_Lower*100,2),
    BA_Upper = round(BA_Upper*100,2),
    BA_CI = paste0(BA_Accuracy, "% (", BA_Lower, "–", BA_Upper, "%)")
  ) %>%
  ungroup() %>%
  select(Algorithm_name, Sensitivity, Specificity, BA_CI)

print(ba_accuracy_df)

#F1 score
f1_df <- df %>%
  rowwise() %>%
  mutate(
    correct = 2*TP,
    total = 2*TP + FP + FN, #use precision and recall.
    F1 = correct / total,
    CI = list(binom.test(correct, total)$conf.int)
  ) %>%
  ungroup() %>%
  mutate(
    F1_Lower = map_dbl(CI, 1),
    F1_Upper = map_dbl(CI, 2),
    F1 = round(F1 * 100, 2),
    F1_Lower = round(F1_Lower * 100, 2),
    F1_Upper = round(F1_Upper * 100, 2),
    F1_CI = paste0(F1, "% (", F1_Lower, "–", F1_Upper, "%)")
  ) %>%
  select(Algorithm_name, F1_CI)

print(f1_df)

#MCC
mcc_df <- df %>%
  mutate(
    numerator = TP * TN - FP * FN,
    denominator = sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)),
    MCC = ifelse(denominator == 0, 0, numerator / denominator),
    MCC = round(MCC * 100, 2)  # optional as percentage
  ) %>%
  select(Algorithm_name, MCC)

print(mcc_df)

# Bootstrap function for one algorithm
mcc_boot_fun <- function(data, indices) {
  gold <- data$Goldstandard[indices]
  pred <- data$Alg1[indices]
  compute_mcc(gold, pred)
}

# Run bootstrap
set.seed(123)
boot_res <- boot(df, mcc_boot_fun, R = 1000)

# 95% CI with 5 decimal places
ci <- boot.ci(boot_res, type = "perc")$percent[4:5]
ci_rounded <- round(ci, 5)
ci_rounded



















cross_tab <- table(merged_data$Final, merged_data$GRHANITE_Site)
print(cross_tab)

merged_data2 <- inner_join(sheet1, sheet3, by = "Patient_UUID")
View(merged_data2)

medicine_count <- merged_data2 %>%
  group_by(Patient_UUID) %>%
  summarise(Num_Medicines = n_distinct(DRUGNAME))

print(medicine_count)

combined_data <- merged_data %>%
  left_join(medicine_count, by = "Patient_UUID")

cross_tab <- table(combined_data$Patient_UUID, combined_data$Num_Medicines==3)
print(cross_tab)
(cross_tab)

print(medicine_count==3)

unmatched_ids <- anti_join(unique_id_df, merged_data, by = "Patient_UUID") %>%
  distinct(Patient_UUID)
print(unmatched_ids)
View(unmatched_ids)
write_xlsx(unmatched_ids, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/List of patients not included in inner_join.xlsx")

num_unique_patients_unmatched_ids <- n_distinct(unmatched_ids$Patient_UUID)
print(num_unique_patients_unmatched_ids)

num_unique_patients_merged_data <- n_distinct(merged_data$Patient_UUID)
print(num_unique_patients_merged_data)
write_xlsx(merged_data, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Merged data.xlsx")





df2 <- Gold_standard_demographics
cross_tab <- table(df2$Final, df$Occu_recat)
print(cross_tab)



df <- Age
df2 <- Demographics
  
first_visits <- df %>%
  group_by(Patient_UUID) %>%
  summarise(first_visit = min(VisitDate, na.rm = TRUE))
View (first_visits)

write_xlsx(first_visits, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Age.xlsx")

merged_data3 <- inner_join(first_visits, df2, by = "Patient_UUID")
View(merged_data3)

write_xlsx(merged_data3, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Gold standard age.xlsx")

Age_data <- merged_data3 %>%
  mutate(
    DateOfBirth = as.Date(DateOfBirth),
    first_visit = as.Date(first_visit),
    age = as.numeric(difftime(first_visit, DateOfBirth, units = "days")) / 365
  )

View(Age_data)

write_xlsx(Age_data, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance//Manuscript/Data/Age.xlsx")

Age_data %>%
  summarise(mean_age = mean(age, na.rm = TRUE))

summary_table <- Age_data %>%
  summarise(
    mean_age = round(mean(age, na.rm = TRUE), 1),
    sd_age = round(sd(age, na.rm = TRUE), 1),
    n = n(),
    .groups = "drop"
  )

summary_table

Age_data$age_group <- cut(
  Age_data$age,
  breaks = c(25, 35, 45, 55, 65, 75, 85, 95, Inf),
  labels = c("<35", "35–44", "45–54", "55–64", "65–74", "75–84", "85–94", "95+"),
  right = FALSE
)

table(Age_data$age_group)

sum(is.na(Age_data$age_group))


str(Age_data$age)

Age_data$age <- as.numeric(Age_data$age)
range(Age_data$age, na.rm = TRUE)




y <- Medications_SI

statins <- c("%ATORVASTATIN", "%Atorvastatin%", "ATORVASTATIN", "Atorvastatin%", "ATORVASTATIN%", "ATORVACHOL", "Atorvachol%", "ATOZET", "Atozet%", "ATOZET%", "CADATIN", "Cadatin%", "CADIVAST", "Cadivast%", "CADUET", "Caduet%", "CAVSTAT", "Cavstat%", "CHOLSTAT%", "Cholstat%", "CRESTOR", "Crestor%", "CROSUVA", "Crosuva%", "EZALO%", "Ezalo%", "FLUVASTATIN", "Fluvastatin%", "LESCOL", "Lescol%", "LESCOL%", "LIPEX", "Lipex%", "LIPITOR", "Lipitor%", "LIPOSTAT", "Lipostat%", "LIPRACHOL", "Liprachol%", "LORSTAT", "Lorstat%", "%PRAVASTATIN", "PRAVACHOL", "Pravachol%", "Pravastat%", "Pravastatin%", "PRAVASTATIN%", "%ROSUVASTATIN", "%Rosuvastatin%", "ROSUVASTATIN", "Rosuvastatin%", "ROSUVASTATIN%", "ROSUZET%", "Rosuzet%", "%SIMVASTATIN", "%Simvastatin%", "SIMVABELL", "SIMVACOR", "Simvacor%", "SIMVAHEXAL", "Simvahexal%", "SIMVAR", "Simvar%", "SIMVASTATIN", "Simvastatin%", "SIMVASTATIN%", "TORVASTAT", "Torvastat%", "TROVAS", "Trovas%", "CHOLVASTIN", "VASTIN", "Vastin%", "Visacor%", "VYTORIN", "Vytorin%", "ZIMSTAT", "Zimstat%", "ZOCOR", "Zocor%", "ZEKLEN")

pattern <- paste(statins, collapse = "|")

column_name <- "DRUGNAME"

indices <- grep(pattern, y[[column_name]], ignore.case = TRUE)

result <- y[indices, ]

print(result)



df_3 <- result
df_3 <- df_3 %>%
  group_by(Patient_UUID) %>%
  arrange(PrescribedDate, .by_group = TRUE) %>%  # sort visits for each patient
  slice(1) %>%                               # keep the earliest visit per patient
  ungroup()
df_3

write_xlsx(df_3, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Other calculations/First Statin prescription.xlsx")

df <- First_Statin_prescription
df <- df %>%
  mutate(
    Statin_Intensity_Category = case_when(
      DRUGNAME == "AMLODIPINE/ATORVASTATIN" & Strength == "5mg/10mg" ~ "Moderate intensity statins",
      DRUGNAME == "APO-ATORVASTATIN" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "APO-ROSUVASTATIN" & Strength == "5mg" ~ "Moderate intensity statins",
      DRUGNAME == "APO-ROSUVASTATIN" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "APO-ROSUVASTATIN" & Strength == "20mg" ~ "High intensity statins",
      DRUGNAME == "APO-SIMVASTATIN" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATORVACHOL" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATORVACHOL" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATORVACHOL" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "ATORVACHOL" & Strength == "80mg" ~ "High intensity statins",
      DRUGNAME == "ATORVASTATIN" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATORVASTATIN" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATORVASTATIN" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "ATORVASTATIN" & Strength == "80mg" ~ "High intensity statins",
      DRUGNAME == "ATOZET" & Strength == "10mg/10mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATOZET" & Strength == "10mg/20mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATOZET" & Strength == "10mg/40mg" ~ "High intensity statins",
      DRUGNAME == "ATOZET" & Strength == "10mg/80mg" ~ "High intensity statins",
      DRUGNAME == "ATOZET COMPOSITE PACK" & Strength == "10mg + 10mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATOZET COMPOSITE PACK" & Strength == "10mg + 20mg" ~ "Moderate intensity statins",
      DRUGNAME == "ATOZET COMPOSITE PACK" & Strength == "10mg + 40mg" ~ "High intensity statins",
      DRUGNAME == "CADATIN" & Strength == "10mg/40mg" ~ "High intensity statins",
      DRUGNAME == "CADATIN" & Strength == "10mg/80mg" ~ "High intensity statins",
      DRUGNAME == "CADIVAST" & Strength == "5mg/20mg" ~ "Moderate intensity statins",
      DRUGNAME == "CADIVAST" & Strength == "5mg/80mg" ~ "High intensity statins",
      DRUGNAME == "CADUET" & Strength == "5mg/10mg" ~ "Moderate intensity statins",
      DRUGNAME == "CADUET" & Strength == "5mg/20mg" ~ "Moderate intensity statins",
      DRUGNAME == "CADUET" & Strength == "5mg/40mg" ~ "High intensity statins",
      DRUGNAME == "CADUET" & Strength == "5mg/80mg" ~ "High intensity statins",
      DRUGNAME == "CADUET" & Strength == "10mg/10mg" ~ "Moderate intensity statins",
      DRUGNAME == "CADUET" & Strength == "10mg/20mg" ~ "Moderate intensity statins",
      DRUGNAME == "CADUET" & Strength == "10mg/40mg" ~ "High intensity statins",
      DRUGNAME == "CADUET" & Strength == "10mg/80mg" ~ "High intensity statins",
      DRUGNAME == "CAVSTAT" & Strength == "5mg" ~ "Moderate intensity statins",
      DRUGNAME == "CAVSTAT" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "CAVSTAT" & Strength == "20mg" ~ "High intensity statins",
      DRUGNAME == "CRESTOR" & Strength == "5mg" ~ "Moderate intensity statins",
      DRUGNAME == "CRESTOR" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "CRESTOR" & Strength == "20mg" ~ "High intensity statins",
      DRUGNAME == "CRESTOR" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "CROSUVA" & Strength == "20mg" ~ "High intensity statins",
      DRUGNAME == "EZETIMIBE/ATORVASTATIN" & Strength == "10mg/10mg" ~ "Moderate intensity statins",
      DRUGNAME == "EZETIMIBE/SIMVASTATIN" & Strength == "10mg/40mg" ~ "Moderate intensity statins",
      DRUGNAME == "EZETIMIBE/SIMVASTATIN" & Strength == "10mg/80mg" ~ "Moderate intensity statins",
      DRUGNAME == "EZETIMIBE/ROSUVASTATIN" & Strength == "10mg/40mg" ~ "High intensity statins",      
      DRUGNAME == "FLUVASTATIN" & Strength == "40mg" ~ "Low intensity statins",
      DRUGNAME == "LESCOL" & Strength == "20mg" ~ "Low intensity statins",
      DRUGNAME == "LESCOL" & Strength == "40mg" ~ "Low intensity statins",
      DRUGNAME == "LESCOL XL" & Strength == "80mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPEX" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "LIPEX" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPEX" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPEX" & Strength == "80mg" ~ "Moderate intensity statins",      
      DRUGNAME == "LIPITOR" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPITOR" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPITOR" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "LIPITOR" & Strength == "80mg" ~ "High intensity statins",       
      DRUGNAME == "LIPOSTAT" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "LIPOSTAT" & Strength == "20mg" ~ "Low intensity statins",
      DRUGNAME == "LIPOSTAT" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "LIPOSTAT" & Strength == "80mg" ~ "Moderate intensity statins", 
      DRUGNAME == "LORSTAT" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "LORSTAT" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "LORSTAT" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "LORSTAT" & Strength == "80mg" ~ "High intensity statins", 
      DRUGNAME == "PRAVACHOL" & Strength == "5mg" ~ "Low intensity statins",
      DRUGNAME == "PRAVACHOL" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "PRAVACHOL" & Strength == "20mg" ~ "Low intensity statins",
      DRUGNAME == "PRAVACHOL" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "PRAVACHOL" & Strength == "80mg" ~ "Moderate intensity statins", 
      DRUGNAME == "ROSUVASTATIN" & Strength == "5mg" ~ "Moderate intensity statins",
      DRUGNAME == "ROSUVASTATIN" & Strength == "10mg" ~ "Moderate intensity statins",
      DRUGNAME == "ROSUVASTATIN" & Strength == "20mg" ~ "High intensity statins",
      DRUGNAME == "ROSUVASTATIN" & Strength == "40mg" ~ "High intensity statins",         
      DRUGNAME == "SIMVACOR" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVACOR" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVAHEXAL" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "SIMVAHEXAL" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVAHEXAL" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVAR" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "SIMVAR" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVAR" & Strength == "20mg" ~ "Moderate intensity statins",      
      DRUGNAME == "SIMVAR" & Strength == "80mg" ~ "Moderate intensity statins",       
      DRUGNAME == "SIMVASTATIN" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "SIMVASTATIN" & Strength == "40mg" ~ "Moderate intensity statins",
      DRUGNAME == "SIMVASTATIN" & Strength == "20mg" ~ "Moderate intensity statins",      
      DRUGNAME == "SIMVASTATIN" & Strength == "80mg" ~ "Moderate intensity statins",  
      DRUGNAME == "TORVASTAT" & Strength == "40mg" ~ "High intensity statins",
      DRUGNAME == "TORVASTAT" & Strength == "20mg" ~ "Moderate intensity statins", 
      DRUGNAME == "TROVAS" & Strength == "20mg" ~ "Moderate intensity statins",
      DRUGNAME == "VASTIN" & Strength == "20mg" ~ "Low intensity statins",
      DRUGNAME == "VASTIN" & Strength == "40mg" ~ "Low intensity statins",  
      DRUGNAME == "VYTORIN" & Strength == "10mg/10mg" ~ "Low intensity statins",
      DRUGNAME == "VYTORIN" & Strength == "10mg/20mg" ~ "Moderate intensity statins",
      DRUGNAME == "VYTORIN" & Strength == "10mg/40mg" ~ "Moderate intensity statins",      
      DRUGNAME == "VYTORIN" & Strength == "10mg/80mg" ~ "Moderate intensity statins",  
      DRUGNAME == "ZIMSTAT" & Strength == "5mg" ~ "Low intensity statins",
      DRUGNAME == "ZIMSTAT" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "ZIMSTAT" & Strength == "20mg" ~ "Moderate intensity statins",      
      DRUGNAME == "ZIMSTAT" & Strength == "40mg" ~ "Moderate intensity statins",   
      DRUGNAME == "ZIMSTAT" & Strength == "80mg" ~ "Moderate intensity statins",         
      DRUGNAME == "ZOCOR" & Strength == "5mg" ~ "Low intensity statins",
      DRUGNAME == "ZOCOR" & Strength == "10mg" ~ "Low intensity statins",
      DRUGNAME == "ZOCOR" & Strength == "20mg" ~ "Moderate intensity statins",      
      DRUGNAME == "ZOCOR" & Strength == "40mg" ~ "Moderate intensity statins",   
      DRUGNAME == "ZOCOR" & Strength == "80mg" ~ "Moderate intensity statins",          
      TRUE ~ "Other"
    )
  )

print(df)

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/Other calculations/First Statin strength.xlsx")

df <- First_Statin_strength
df2 <- Gold_standard_demographics
df3 <- df %>%
  inner_join (df2, by = "Patient_UUID")

View (df3)

tbl <- table(df3$Statin_type, df3$Final)
tbl
