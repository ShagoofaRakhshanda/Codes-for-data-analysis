install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("writexl")
library(writexl)
install.packages("stringr")
library(stringr)
install.packages("lubridate")
library(lubridate)
install.packages("car")
library(car)
install.packages("ResourceSelection")
library(ResourceSelection)
install.packages("pROC")
library(pROC)
install.packages("broom")
library(broom)
install.packages("MASS")
library(MASS)



sheet1 <- Adherence_and_comorbidities_4016
sheet2 <- Adherence_and_Demography_4016
sheet3 <- Adherence_and_first_statin_strength_4016
sheet4 <- Adherence_and_medication_count_4016
sheet5 <- Adherence_and_statin_count_4016
sheet6 <- Adherence_and_Statin_intensity_on_first_and_last_visit_in_2_years_4016

merged_sheet <- sheet1 %>%
  inner_join(sheet2, by = "Patient_UUID") %>%
  inner_join(sheet3, by = "Patient_UUID") %>%
  inner_join(sheet4, by = "Patient_UUID")%>%
  inner_join(sheet5, by = "Patient_UUID")%>%
  inner_join(sheet6, by = "Patient_UUID")

print(merged_sheet)

write_xlsx(merged_sheet, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Intolerance/OR model_4016.xlsx")

# Manually delete the excess variables of Adherence. 
## Also manually change name of UniqueMedicineCount.y to UniqueMedicineCount_statin and UniqueMedicineCount.x to UniqueMedicineCount_all


df1 <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates
df2 <- Medications

merged_df <- inner_join(df1, df2, by = c("Patient_UUID", "PrescribedDate"), relationship = "many-to-many") 
View(merged_df)

df_filtered <- merged_df %>%
  group_by(Patient_UUID) %>%
  filter(n_distinct(PrescribedDate) >= 3) %>%
  ungroup()

unique_ids <- length(unique(df_filtered$Patient_UUID))
print(unique_ids)

write_xlsx(df_filtered, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Intolerance/Visit reason in first two years without duplicates_4016.xlsx")

df1 <- Visit_reason_in_first_two_years_without_duplicates_4016
df2 <- LabResults
merged_df <- inner_join(df1, df2, by = c("Patient_UUID")) 
View(merged_df)

df3 <- merged_df
df4 <- Demographics
merged_df <- inner_join(df3, df4, by = c("Patient_UUID")) 
View(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Intolerance/CK_4016.xlsx")


df <- Visit_reason_in_first_two_years_without_duplicates_4016
df_summary <- df %>%
  group_by(Patient_UUID) %>%
  summarise(
    SI = if_else(any(Statin_intolerance == "Yes"), "Yes", "No")
  ) %>%
  ungroup()

View(df_summary)


tbl <- table(df_summary$SI)
print(tbl)

df1 <- df_summary
df2 <- OR_model_4016

merged_df <- inner_join(df1, df2, by = c("Patient_UUID")) 
View(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Intolerance/Intolerance_OR model_4016.xlsx")



data <- Intolerance_OR_model_4016
data %>%
  group_by(SI) %>%
  summarise(mean_value = mean(Age_at_Visit, na.rm = TRUE),
            sd_value   = sd(Age_at_Visit, na.rm = TRUE))

data %>%
  summarise(mean_value = mean(Age_at_Visit, na.rm = TRUE),
            sd_value   = sd(Age_at_Visit, na.rm = TRUE))

t_result <- t.test(Age_at_Visit ~ SI, data = data)
print(t_result)
t_result$p.value