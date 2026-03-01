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




y <- Medications

unique_ids <- length(unique(y$Patient_UUID))

print(unique_ids)

total_visits <- nrow(y)

print(total_visits)

total_visits <- Statins_prescription_5765_patients %>%
  distinct(Patient_UUID, PrescribedDate) %>% 
  tally(name = "Total_Visits") %>%
  pull(Total_Visits)

print(total_visits)



total_visits <- Statins_prescription_5765_patients %>%
  semi_join(PDC, by = "Patient_UUID") %>%      # keep only patients from df2
  distinct(Patient_UUID, PrescribedDate) %>%       # remove duplicates
  tally(name = "Total_Visits") %>%           # count rows
  pull(Total_Visits)                         # extract as a number

print(total_visits)


statins <- c("%ATORVASTATIN", "%Atorvastatin%", "ATORVASTATIN", "Atorvastatin%", "ATORVASTATIN%", "ATORVACHOL", "Atorvachol%", "ATOZET", "Atozet%", "ATOZET%", "CADATIN", "Cadatin%", "CADIVAST", "Cadivast%", "CADUET", "Caduet%", "CAVSTAT", "Cavstat%", "CHOLSTAT%", "Cholstat%", "CRESTOR", "Crestor%", "CROSUVA", "Crosuva%", "EZALO%", "Ezalo%", "FLUVASTATIN", "Fluvastatin%", "LESCOL", "Lescol%", "LESCOL%", "LIPEX", "Lipex%", "LIPITOR", "Lipitor%", "LIPOSTAT", "Lipostat%", "LIPRACHOL", "Liprachol%", "LORSTAT", "Lorstat%", "%PRAVASTATIN", "PRAVACHOL", "Pravachol%", "Pravastat%", "Pravastatin%", "PRAVASTATIN%", "%ROSUVASTATIN", "%Rosuvastatin%", "ROSUVASTATIN", "Rosuvastatin%", "ROSUVASTATIN%", "ROSUZET%", "Rosuzet%", "%SIMVASTATIN", "%Simvastatin%", "SIMVABELL", "SIMVACOR", "Simvacor%", "SIMVAHEXAL", "Simvahexal%", "SIMVAR", "Simvar%", "SIMVASTATIN", "Simvastatin%", "SIMVASTATIN%", "TORVASTAT", "Torvastat%", "TROVAS", "Trovas%", "CHOLVASTIN", "VASTIN", "Vastin%", "Visacor%", "VYTORIN", "Vytorin%", "ZIMSTAT", "Zimstat%", "ZOCOR", "Zocor%", "ZEKLEN")

pattern <- paste(statins, collapse = "|")
column_name <- "DRUGNAME"
indices <- grep(pattern, y[[column_name]], ignore.case = TRUE)
result <- y[indices, ]
print(result)
count <- nrow(result)
print(count)

result <- result %>%
  filter(!str_detect(DRUGNAME, regex("Avastin Injection|AVASTIN", ignore_case = TRUE)))
print(result)

write_xlsx(result, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statins prescription.xlsx")

count <- nrow(result)
print(count)
distinct_count <- result %>%
  summarise(DistinctPatients = n_distinct(Patient_UUID))
print(distinct_count)



# rows to column

data <- Statins_Dates

data_wide <- data %>%
  group_by (Patient_UUID) %>%
  mutate(Visit_number = row_number()) %>% 
  pivot_wider(
    names_from = Visit_number,
    values_from = PrescribedDate,
    names_prefix = "Visit_"
  )

write_xlsx(data_wide, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statins_Visits.xlsx")


# Finding difference between first and last visit

date_data <- Statins_Visits[, -1]  # Assuming first column is Patient_UUID
date_data <- lapply(date_data, as.Date)  # Convert to Date format

# Add a new column for the difference between first and last visit
Statins_Visits <- Statins_Visits %>%
  mutate(Date_Difference = apply(do.call(cbind, date_data), 1, function(x) {
    valid_dates <- na.omit(x)  # Remove missing dates
    if (length(valid_dates) > 0) {
      as.numeric(max(valid_dates) - min(valid_dates))  # Difference in days
    } else {
      NA
    }
  }))

print(Statins_Visits)

write_xlsx(Statins_Visits, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visit differences.xlsx")

data <- Visit_differences
filtered_data <- data %>%
  filter(Date_Difference >= 730)

print(filtered_data)

write_xlsx(filtered_data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visit differences from 730.xlsx")


# filter data of 5919 patients from main medication file

patients_to_keep <- Visit_differences_from_730$Patient_UUID

filtered_medication_data <- Medications %>%
  filter(Patient_UUID %in% patients_to_keep)

print(filtered_medication_data)

write_xlsx(filtered_medication_data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Medications data_5765 patients.xlsx")

# filter statin data from this dataset

y <- Medications_data_5765_patients

statins <- c("%ATORVASTATIN", "%Atorvastatin%", "ATORVASTATIN", "Atorvastatin%", "ATORVASTATIN%", "ATORVACHOL", "Atorvachol%", "ATOZET", "Atozet%", "ATOZET%", "CADATIN", "Cadatin%", "CADIVAST", "Cadivast%", "CADUET", "Caduet%", "CAVSTAT", "Cavstat%", "CHOLSTAT%", "Cholstat%", "CRESTOR", "Crestor%", "CROSUVA", "Crosuva%", "EZALO%", "Ezalo%", "FLUVASTATIN", "Fluvastatin%", "LESCOL", "Lescol%", "LESCOL%", "LIPEX", "Lipex%", "LIPITOR", "Lipitor%", "LIPOSTAT", "Lipostat%", "LIPRACHOL", "Liprachol%", "LORSTAT", "Lorstat%", "%PRAVASTATIN", "PRAVACHOL", "Pravachol%", "Pravastat%", "Pravastatin%", "PRAVASTATIN%", "%ROSUVASTATIN", "%Rosuvastatin%", "ROSUVASTATIN", "Rosuvastatin%", "ROSUVASTATIN%", "ROSUZET%", "Rosuzet%", "%SIMVASTATIN", "%Simvastatin%", "SIMVABELL", "SIMVACOR", "Simvacor%", "SIMVAHEXAL", "Simvahexal%", "SIMVAR", "Simvar%", "SIMVASTATIN", "Simvastatin%", "SIMVASTATIN%", "TORVASTAT", "Torvastat%", "TROVAS", "Trovas%", "CHOLVASTIN", "VASTIN", "Vastin%", "Visacor%", "VYTORIN", "Vytorin%", "ZIMSTAT", "Zimstat%", "ZOCOR", "Zocor%", "ZEKLEN")

pattern <- paste(statins, collapse = "|")

column_name <- "DRUGNAME"

indices <- grep(pattern, y[[column_name]], ignore.case = TRUE)

result <- y[indices, ]

print(result)

count <- nrow(result)

print(count)

write_xlsx(result, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statins prescription_5765 patients.xlsx")


# Filtering age

df1<-Statins_prescription_5765_patients

df1 <- df1 %>%
  mutate(PrescribedDate = as.Date(PrescribedDate))

# Get the earliest visit date for each Patient ID
earliest_visits <- df1 %>%
  group_by(Patient_UUID) %>%
  summarise(Earliest_Visit = min(PrescribedDate, na.rm = TRUE)) %>%
  ungroup()

write_xlsx(earliest_visits, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/First visits of statin patients_5765.xlsx")

df1<-First_visits_of_statin_patients_5765
df2<-Demographics

merged_df <- inner_join(df1, df2, by = "Patient_UUID")

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Demography of statin patients_5765.xlsx")

merged_df <- merged_df %>%
  mutate(
    Earliest_Visit = as.Date(Earliest_Visit),
    DateOfBirth = as.Date(DateOfBirth)
  )

# Calculate age at visit
merged_df <- merged_df %>%
  mutate(Age_at_Visit = as.integer(floor(time_length(interval(DateOfBirth, Earliest_Visit), "years"))))

# View the updated dataset
print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Demography of statin patients_5765.xlsx")




### For dates upto 730

df <- Visit_differences_from_730
  
df_long <- df %>%
  pivot_longer(cols = starts_with("Visit"),
               names_to = "Visit",
               values_to = "Visit_Date") %>%
  drop_na(Visit_Date) %>%
  mutate(Visit_Date = as.Date(Visit_Date))

# Process each patient to filter visits and calculate the difference
result <- df_long %>%
  group_by(Patient_UUID) %>%
  arrange(Visit_Date) %>%
  mutate(
    First_Visit = first(Visit_Date),  # Get the first visit date
    Visit_Difference_Days = as.numeric(Visit_Date - First_Visit)  # Difference from the first visit
  ) %>%
  filter(Visit_Difference_Days <= 730) %>%
  summarize(
    Valid_Visits = list(Visit_Date),
    Visit_Difference = max(Visit_Difference_Days, na.rm = TRUE)
  ) %>%
  mutate(Difference_Within_730 = Visit_Difference <= 730)

result <- df_long %>%
  group_by(Patient_UUID) %>%
  arrange(Visit_Date) %>%
  mutate(
    First_Visit = first(Visit_Date),
    Visit_Difference_Days = as.numeric(Visit_Date - First_Visit)
  ) %>%
  filter(Visit_Difference_Days <= 730) %>%
  mutate(Visit_Number = row_number()) %>%  # Create visit number for pivot_wider
  select(Patient_UUID, Visit_Number, Visit_Date) %>%
  pivot_wider(names_from = Visit_Number, 
              values_from = Visit_Date, 
              names_prefix = "Visit_") 

print(result)

write_xlsx(result, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visits upto 730 days.xlsx")

result_long <- result %>%
  pivot_longer(
    cols = starts_with("Visit"),        # Select all visit columns
    names_to = "Visit_Number",                  # Column for visit names
    values_to = "Visit_Date"             # Column for visit dates
  ) %>%
  drop_na(Visit_Date) %>%                # Remove rows with missing visit dates
  arrange(Patient_UUID, Visit_Date)      # Sort by Patient and Visit Date

print(result_long)

write_xlsx(result_long, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visits upto 730 days_long_5765.xlsx")

### Finding the patients with 3 or more visits in the 2 years

df<- Visits_upto_730_days_long_5765

df_filtered <- df %>%
  group_by(Patient_UUID) %>%
  filter(n() >= 3) %>%
  ungroup()

print(df_filtered)

write_xlsx(df_filtered, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visits upto 730 days_long_3 or more visits.xlsx")

df<-Visits_upto_730_days_long_3_or_more_visits

df <- df %>%
  mutate(Visit_Date = as.Date(Visit_Date))

# Keep only patients with >= 3 unique visit dates
df_filtered <- df %>%
  group_by(Patient_UUID) %>%
  filter(n_distinct(Visit_Date) >= 3) %>%
  ungroup()

unique_ids <- length(unique(df_filtered$Patient_UUID))

print(unique_ids)


write_xlsx(df_filtered, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visits upto 730 days_long_3 or more visits without duplicates.xlsx")

df<- Visits_upto_730_days_long_3_or_more_visits_without_duplicates
  
df <- df %>%
  mutate(Visit_Date = as.Date(Visit_Date))

# Pivot wider so each patient is one row, visits become separate columns
df_wide <- df %>%
  arrange(Patient_UUID, Visit_Number) %>%  # ensure visits are ordered
  pivot_wider(
    id_cols = Patient_UUID,
    names_from = Visit_Number,
    values_from = Visit_Date,
    names_prefix = "Visit_"
  )

print(df_wide)

df_wide <- df_wide %>%
  rowwise() %>%
  mutate(
    # Calculate the earliest and latest visit dates ignoring NA
    Earliest_Visit = min(c_across(starts_with("Visit_")), na.rm = TRUE),
    Latest_Visit = max(c_across(starts_with("Visit_")), na.rm = TRUE),
    Total_Days = as.integer(Latest_Visit - Earliest_Visit)
  ) %>%
  ungroup() %>%
  select(-Earliest_Visit, -Latest_Visit)

print(df_wide)

write_xlsx(df_wide, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Visits upto 730 days_wide_3 or more visits without duplicates_4016 total days.xlsx")



### Maybe not needed
print(df_diff)

write_xlsx(df_diff, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/v3/v3_Working File/Statins/Adherence measures/visits upto 730 days_total.xlsx")


# to remove the last visit date for the calculation of the numerator

df <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates
df_filtered <- df %>%
  group_by(Patient_UUID) %>%
  filter(Visit_Date != max(Visit_Date)) %>%
  ungroup()

print(df_filtered)

write_xlsx(df_filtered, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/visits before the max visit date to 730 days_long_4016 patients.xlsx")

## to first find the duplicate dates of each patient
df<-Visits_before_the_max_visit_date_to_730_days_long_4016_patients

df_clean <- df %>%
  group_by(Patient_UUID, Visit_Date) %>%
  arrange(Visit_Number) %>%
  slice(1) %>%                           # keep only first occurrence of duplicates
  ungroup() %>%
  group_by(Patient_UUID) %>%
  arrange(Patient_UUID, Visit_Date) %>%    # order visits by date
  mutate(Visit_Number = row_number()) %>%# reassign visit numbers sequentially
  ungroup()

df_clean <- df_clean %>%
  rename(PrescribedDate = Visit_Date)

print(df_clean)

write_xlsx(df_clean, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/visits before the max visit to 730 days_long_4016 patients without repeat dates.xlsx")




# for merging two datasets
df1 <- Statins_prescription_5765_patients
df2 <- Visits_before_the_max_visit_to_730_days_long_4016_patients_without_repeat_dates

merged_df <- df1 %>%
  inner_join(df2, by = c("Patient_UUID" = "Patient_UUID", 
                         "PrescribedDate" = "PrescribedDate")) %>%  # keep only patients and dates present in df2
  distinct(Patient_UUID, PrescribedDate, .keep_all = TRUE)  # remove duplicates in df1

print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Merged_statin prescription in first 2 years_4016.xlsx")


df <- visits_upto_730_days_long

df %>%
  group_by(Patient_UUID) %>%
  summarise(Visit_Count = n()) %>%
  filter(Visit_Count == 1) %>%
  count()



unique_patient_count <- df %>%
  summarise(Unique_Patients = n_distinct(Patient_UUID))

print(unique_patient_count)




## Converting rows to column for neumerator

medication_data <- Merged_statin_prescription_in_first_2_years_4016 %>%
  group_by(Patient_UUID) %>%
  arrange(PrescribedDate) %>%  # Ensure visit dates are in ascending order
  mutate(Visit_Number = row_number()) %>%
  ungroup()

# Pivot the data wider to spread visits and corresponding variables
wide_data <- medication_data %>%
  pivot_wider(
    id_cols = Patient_UUID,
    names_from = Visit_Number,
    values_from = c(PrescribedDate, DRUGNAME, Strenght, DOSE, Frequency, Repeats, Quantity, Days_Covered),
    names_glue = "{.value}_{Visit_Number}"
  )

# Print the resulting wide dataset
print(wide_data)



write_xlsx(wide_data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statins prescription rows to column_numerator.xlsx")



# Merge the nemerator and denominator in one excel sheet

df_1 <- Visit_difference_Denominator
df_2 <- Sum_of_days_covered_numerator

merged_df <- inner_join(df_1, df_2, by = c("Patient_UUID"))

print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/PDC.xlsx")


# Categorise PDC for adherent and non-adherent

PDC <- PDC

PDC$Adherence <- ifelse(PDC$PDC >= 80, "Adherent", "Not Adherent")
PDC <- PDC %>%
  mutate(
    PDC_Category = case_when(
      PDC < 10 ~ "<10",
      PDC >= 10 & PDC < 20 ~ "≥10 to <20",
      PDC >= 20 & PDC < 30 ~ "≥20 to <30",
      PDC >= 30 & PDC < 40 ~ "≥30 to <40",
      PDC >= 40 & PDC < 50 ~ "≥40 to <50",
      PDC >= 50 & PDC < 60 ~ "≥50 to <60",
      PDC >= 60 & PDC < 70 ~ "≥60 to <70",
      PDC >= 70 & PDC < 80 ~ "≥70 to <80",
      PDC >= 80 & PDC < 90 ~ "≥80 to <90",
      PDC >= 90 ~ "≥90"
    )
  )

# Print the entire dataset
print(PDC)

write_xlsx(PDC, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence levels_4016.xlsx")

summary(PDC)

PDC %>%
  count(Adherence) %>%
  arrange(desc(n))

PDC %>%
  count(PDC_Category) %>%
  arrange(desc(n))


## Analysis

## Merge with Demographics table

df_1 <- Demography_of_statin_patients_5765
df_2 <- Adherence_levels_4016
df_3 <- Statins_prescription_5765_patients

merged_df <- df_1 %>%
  inner_join(df_2, by = "Patient_UUID")
  
print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Demography_4016.xlsx")


##Mean age
data <- Demography_4016
data %>%
  group_by(Adherence) %>%
  summarise(mean_value = mean(Age_at_Visit, na.rm = TRUE),
            sd_value   = sd(Age_at_Visit, na.rm = TRUE))

data %>%
  summarise(mean_value = mean(Age_at_Visit, na.rm = TRUE),
            sd_value   = sd(Age_at_Visit, na.rm = TRUE))

t_result <- t.test(Age_at_Visit ~ Adherence, data = data)
print(t_result)
t_result$p.value

# Age category
data <- data %>%
  mutate(AgeCategory = cut(
    Age_at_Visit, 
    breaks = c(18, 30, 40, 50, 60, 70, Inf),  # Breakpoints for binning
    labels = c("18-30", "30-40", "40-50", "50-60", "60-70", "≥ 70"),
    right = FALSE  # Exclude the right endpoint (non-inclusive)
  ))
tbl <- table(data$AgeCategory, data$Adherence)
table(data$AgeCategory)

chisq.test(tbl)


#Gender

tbl <- table(data$GENDER, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$GENDER)


#Ethnicity

tbl <- table(data$ETHNICTY, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$ETHNICTY)


#Grhanite site

tbl <- table(data$GRHANITE_Site, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$GRHANITE_Site)


# Occupation

tbl <- table(data$OCCUPATION_recat, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$OCCUPATION_recat)


# first Statin prescription intensity 
  
df_3 <- df_3 %>%
  group_by(Patient_UUID) %>%
  arrange(PrescribedDate, .by_group = TRUE) %>%  # sort visits for each patient
  slice(1) %>%                               # keep the earliest visit per patient
  ungroup()

merged_df <- df_2 %>%
  inner_join (df_3, by = "Patient_UUID")

merged_df

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/First Statin prescription_4016.xlsx")

df <- First_Statin_prescription_4016

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

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and first statin strength_4016.xlsx")

data <- Adherence_and_first_statin_strength_4016
tbl <- table(data$Statin_Intensity_Category, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$Statin_Intensity_Category)




# Create manually a separate column in the Adherence and first statin strength file for Statin type

data <- Adherence_and_first_statin_strength_4016
tbl <- table(data$Statin_type, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$Statin_type)



# For finding number of statins per patient, we will have to merge the datasets

df1 <- Statins_prescription_5765_patients
df2 <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates

merged_df <- inner_join(df1, df2, by = c("Patient_UUID", "PrescribedDate"), relationship = "many-to-many") 

print(merged_df)
unique_ids <- length(unique(merged_df$Patient_UUID))

print(unique_ids)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Merged_statin prescription in first 2 years 4016.xlsx")



# Manually change all the brand names to generic names of drugs

medication_data <- Merged_statin_prescription_in_first_2_years_4016 %>%
  group_by(Patient_UUID) %>%
  arrange(PrescribedDate) %>%  # Ensure visit dates are in ascending order
  mutate(Visit_Number = row_number()) %>%
  ungroup()

print(medication_data)

# Pivot the data wider to spread visits and corresponding variables
wide_data <- medication_data %>%
  pivot_wider(
    id_cols = Patient_UUID,
    names_from = Visit_Number,
    values_from = c(PrescribedDate, Statin_type),
    names_glue = "{.value}_{Visit_Number}"
  )

# Print the resulting wide dataset
print(wide_data)

write_xlsx(wide_data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statins prescription rows to column_4016_statin count.xlsx")



# Now we will count the number of distinct statins for each patient.

# Function to count unique medicine names across a row
count_unique_medicines <- function(row) {
  # Remove NAs and empty strings
  row <- row[!is.na(row) & row != ""]
  # Get unique medicine names (ignoring order)
  unique_meds <- unique(row)
  # Return the number of distinct medicines
  length(unique_meds)
}

# Apply this function row-wise across the selected columns
df <- Statins_prescription_rows_to_column_4016_statin_count

df <- df %>%
  rowwise() %>%
  mutate(UniqueMedicineCount = count_unique_medicines(c_across(starts_with("Statin_type_")))) %>%
  ungroup()

# View the result
print(df)

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Statin count.xlsx")


# Now we will merge this with the adherence demography and strength excel sheet

df_1 <- Adherence_levels_4016
df_2 <- Statin_count

merged_df <- inner_join(df_1, df_2, by = c("Patient_UUID"))

print (merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and statin count_4016.xlsx")


#Cross-tab of Statin count with adherence

data <- Adherence_and_statin_count_4016
tbl <- table(data$UniqueMedicineCount, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$UniqueMedicineCount)



# To find the change in statin intensity over 2 years, 

# First find the last visit in 2 years of each person from the long visit data

df <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates

df_summary <- df %>%
  group_by(Patient_UUID) %>%
  summarise(
    Min_PrescribedDate = min(PrescribedDate, na.rm = TRUE),
    Max_PrescribedDate = max(PrescribedDate, na.rm = TRUE)
  ) %>%
  ungroup()

# View the result
print(df_summary)

# Now merge this with the statin drug information 

df1 <- Statins_prescription_5765_patients
df2 <- df_summary

df1 <- df1 %>%
  distinct(Patient_UUID, PrescribedDate, .keep_all = TRUE)

final_df <- df2 %>%
  # Join with df1 on Patient_ID + Min date to get Min drug
  left_join(df1, by = c("Patient_UUID" = "Patient_UUID", 
                        "Min_PrescribedDate" = "PrescribedDate")) %>%
  rename(Min_Drug = DRUGNAME,
         Min_Strength = Strenght) %>%
  
  # Join again with df1 on Patient_ID + Max date to get Max drug
  left_join(df1, by = c("Patient_UUID" = "Patient_UUID", 
                        "Max_PrescribedDate" = "PrescribedDate")) %>%
  rename(Max_Drug = DRUGNAME,
         Max_Strength = Strenght)

# Final dataset
final_df

colnames(final_df)

write_xlsx(final_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Merged_statin intensity in first and last visit in 2 years.xlsx")

# Now again define the statin intensity 

df <- Merged_statin_intensity_in_first_and_last_visit_in_2_years

df <- df %>%
  mutate(
    Min_Statin_Intensity = case_when(
      Min_Drug == "AMLODIPINE/ATORVASTATIN" & Min_Strength == "5mg/10mg" ~ "Moderate intensity statins",
      Min_Drug == "APO-ATORVASTATIN" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "APO-ROSUVASTATIN" & Min_Strength == "5mg" ~ "Moderate intensity statins",
      Min_Drug == "APO-ROSUVASTATIN" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "APO-ROSUVASTATIN" & Min_Strength == "20mg" ~ "High intensity statins",
      Min_Drug == "APO-SIMVASTATIN" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "ATORVACHOL" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "ATORVACHOL" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "ATORVACHOL" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "ATORVACHOL" & Min_Strength == "80mg" ~ "High intensity statins",
      Min_Drug == "ATORVASTATIN" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "ATORVASTATIN" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "ATORVASTATIN" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "ATORVASTATIN" & Min_Strength == "80mg" ~ "High intensity statins",
      Min_Drug == "ATOZET" & Min_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Min_Drug == "ATOZET" & Min_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Min_Drug == "ATOZET" & Min_Strength == "10mg/40mg" ~ "High intensity statins",
      Min_Drug == "ATOZET" & Min_Strength == "10mg/80mg" ~ "High intensity statins",
      Min_Drug == "ATOZET COMPOSITE PACK" & Min_Strength == "10mg + 10mg" ~ "Moderate intensity statins",
      Min_Drug == "ATOZET COMPOSITE PACK" & Min_Strength == "10mg + 20mg" ~ "Moderate intensity statins",
      Min_Drug == "ATOZET COMPOSITE PACK" & Min_Strength == "10mg + 40mg" ~ "High intensity statins",
      Min_Drug == "CADATIN" & Min_Strength == "10mg/40mg" ~ "High intensity statins",
      Min_Drug == "CADATIN" & Min_Strength == "10mg/80mg" ~ "High intensity statins",
      Min_Drug == "CADIVAST" & Min_Strength == "5mg/20mg" ~ "Moderate intensity statins",
      Min_Drug == "CADIVAST" & Min_Strength == "5mg/80mg" ~ "High intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "5mg/10mg" ~ "Moderate intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "5mg/20mg" ~ "Moderate intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "5mg/40mg" ~ "High intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "5mg/80mg" ~ "High intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "10mg/40mg" ~ "High intensity statins",
      Min_Drug == "CADUET" & Min_Strength == "10mg/80mg" ~ "High intensity statins",
      Min_Drug == "CAVSTAT" & Min_Strength == "5mg" ~ "Moderate intensity statins",
      Min_Drug == "CAVSTAT" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "CAVSTAT" & Min_Strength == "20mg" ~ "High intensity statins",
      Min_Drug == "CRESTOR" & Min_Strength == "5mg" ~ "Moderate intensity statins",
      Min_Drug == "CRESTOR" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "CRESTOR" & Min_Strength == "20mg" ~ "High intensity statins",
      Min_Drug == "CRESTOR" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "CROSUVA" & Min_Strength == "20mg" ~ "High intensity statins",
      Min_Drug == "EZETIMIBE/ATORVASTATIN" & Min_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Min_Drug == "EZETIMIBE/SIMVASTATIN" & Min_Strength == "10mg/40mg" ~ "Moderate intensity statins",
      Min_Drug == "EZETIMIBE/SIMVASTATIN" & Min_Strength == "10mg/80mg" ~ "Moderate intensity statins",
      Min_Drug == "FLUVASTATIN" & Min_Strength == "40mg" ~ "Low intensity statins",
      Min_Drug == "LESCOL" & Min_Strength == "20mg" ~ "Low intensity statins",
      Min_Drug == "LESCOL" & Min_Strength == "40mg" ~ "Low intensity statins",
      Min_Drug == "LESCOL XL" & Min_Strength == "80mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPEX" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "LIPEX" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPEX" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPEX" & Min_Strength == "80mg" ~ "Moderate intensity statins",      
      Min_Drug == "LIPITOR" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPITOR" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPITOR" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "LIPITOR" & Min_Strength == "80mg" ~ "High intensity statins",       
      Min_Drug == "LIPOSTAT" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "LIPOSTAT" & Min_Strength == "20mg" ~ "Low intensity statins",
      Min_Drug == "LIPOSTAT" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "LIPOSTAT" & Min_Strength == "80mg" ~ "Moderate intensity statins", 
      Min_Drug == "LORSTAT" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "LORSTAT" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "LORSTAT" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "LORSTAT" & Min_Strength == "80mg" ~ "High intensity statins", 
      Min_Drug == "PRAVACHOL" & Min_Strength == "5mg" ~ "Low intensity statins",
      Min_Drug == "PRAVACHOL" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "PRAVACHOL" & Min_Strength == "20mg" ~ "Low intensity statins",
      Min_Drug == "PRAVACHOL" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "PRAVACHOL" & Min_Strength == "80mg" ~ "Moderate intensity statins", 
      Min_Drug == "ROSUVASTATIN" & Min_Strength == "5mg" ~ "Moderate intensity statins",
      Min_Drug == "ROSUVASTATIN" & Min_Strength == "10mg" ~ "Moderate intensity statins",
      Min_Drug == "ROSUVASTATIN" & Min_Strength == "20mg" ~ "High intensity statins",
      Min_Drug == "ROSUVASTATIN" & Min_Strength == "40mg" ~ "High intensity statins",         
      Min_Drug == "SIMVACOR" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVACOR" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVAHEXAL" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "SIMVAHEXAL" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVAHEXAL" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVAR" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "SIMVAR" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVAR" & Min_Strength == "20mg" ~ "Moderate intensity statins",      
      Min_Drug == "SIMVAR" & Min_Strength == "80mg" ~ "Moderate intensity statins",       
      Min_Drug == "SIMVASTATIN" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "SIMVASTATIN" & Min_Strength == "40mg" ~ "Moderate intensity statins",
      Min_Drug == "SIMVASTATIN" & Min_Strength == "20mg" ~ "Moderate intensity statins",      
      Min_Drug == "SIMVASTATIN" & Min_Strength == "80mg" ~ "Moderate intensity statins",  
      Min_Drug == "TORVASTAT" & Min_Strength == "40mg" ~ "High intensity statins",
      Min_Drug == "TORVASTAT" & Min_Strength == "20mg" ~ "Moderate intensity statins", 
      Min_Drug == "TROVAS" & Min_Strength == "20mg" ~ "Moderate intensity statins",
      Min_Drug == "VASTIN" & Min_Strength == "20mg" ~ "Low intensity statins",
      Min_Drug == "VASTIN" & Min_Strength == "40mg" ~ "Low intensity statins",  
      Min_Drug == "VYTORIN" & Min_Strength == "10mg/10mg" ~ "Low intensity statins",
      Min_Drug == "VYTORIN" & Min_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Min_Drug == "VYTORIN" & Min_Strength == "10mg/40mg" ~ "Moderate intensity statins",      
      Min_Drug == "VYTORIN" & Min_Strength == "10mg/80mg" ~ "Moderate intensity statins",  
      Min_Drug == "ZIMSTAT" & Min_Strength == "5mg" ~ "Low intensity statins",
      Min_Drug == "ZIMSTAT" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "ZIMSTAT" & Min_Strength == "20mg" ~ "Moderate intensity statins",      
      Min_Drug == "ZIMSTAT" & Min_Strength == "40mg" ~ "Moderate intensity statins",   
      Min_Drug == "ZIMSTAT" & Min_Strength == "80mg" ~ "Moderate intensity statins",         
      Min_Drug == "ZOCOR" & Min_Strength == "5mg" ~ "Low intensity statins",
      Min_Drug == "ZOCOR" & Min_Strength == "10mg" ~ "Low intensity statins",
      Min_Drug == "ZOCOR" & Min_Strength == "20mg" ~ "Moderate intensity statins",      
      Min_Drug == "ZOCOR" & Min_Strength == "40mg" ~ "Moderate intensity statins",   
      Min_Drug == "ZOCOR" & Min_Strength == "80mg" ~ "Moderate intensity statins",          
      TRUE ~ "Other"
    )
  )

print(df)

table(df$Min_Statin_Intensity)

df <- df %>%
  mutate(
    Max_Statin_Intensity = case_when(
      Max_Drug == "AMLODIPINE/ATORVASTATIN" & Max_Strength == "5mg/10mg" ~ "Moderate intensity statins",
      Max_Drug == "AMLODIPINE/ATORVASTATIN" & Max_Strength == "5mg/40mg" ~ "High intensity statins",
      Max_Drug == "AMLODIPINE/ATORVASTATIN" & Max_Strength == "10mg/40mg" ~ "High intensity statins",
      Max_Drug == "APO-ATORVASTATIN" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "APO-ROSUVASTATIN" & Max_Strength == "5mg" ~ "Moderate intensity statins",
      Max_Drug == "APO-ROSUVASTATIN" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "APO-ROSUVASTATIN" & Max_Strength == "20mg" ~ "High intensity statins",
      Max_Drug == "APO-SIMVASTATIN" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "APO-SIMVASTATIN" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "ATORVACHOL" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "ATORVACHOL" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "ATORVACHOL" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "ATORVACHOL" & Max_Strength == "80mg" ~ "High intensity statins",
      Max_Drug == "ATORVASTATIN" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "ATORVASTATIN" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "ATORVASTATIN" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "ATORVASTATIN" & Max_Strength == "80mg" ~ "High intensity statins",
      Max_Drug == "ATOZET" & Max_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Max_Drug == "ATOZET" & Max_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Max_Drug == "ATOZET" & Max_Strength == "10mg/40mg" ~ "High intensity statins",
      Max_Drug == "ATOZET" & Max_Strength == "10mg/80mg" ~ "High intensity statins",
      Max_Drug == "ATOZET COMPOSITE PACK" & Max_Strength == "10mg + 10mg" ~ "Moderate intensity statins",
      Max_Drug == "ATOZET COMPOSITE PACK" & Max_Strength == "10mg + 20mg" ~ "Moderate intensity statins",
      Max_Drug == "ATOZET COMPOSITE PACK" & Max_Strength == "10mg + 40mg" ~ "High intensity statins",
      Max_Drug == "CADATIN" & Max_Strength == "10mg/40mg" ~ "High intensity statins",
      Max_Drug == "CADATIN" & Max_Strength == "10mg/80mg" ~ "High intensity statins",
      Max_Drug == "CADIVAST" & Max_Strength == "5mg/20mg" ~ "Moderate intensity statins",
      Max_Drug == "CADIVAST" & Max_Strength == "5mg/80mg" ~ "High intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "5mg/10mg" ~ "Moderate intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "5mg/20mg" ~ "Moderate intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "5mg/40mg" ~ "High intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "5mg/80mg" ~ "High intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "10mg/40mg" ~ "High intensity statins",
      Max_Drug == "CADUET" & Max_Strength == "10mg/80mg" ~ "High intensity statins",
      Max_Drug == "CAVSTAT" & Max_Strength == "5mg" ~ "Moderate intensity statins",
      Max_Drug == "CAVSTAT" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "CAVSTAT" & Max_Strength == "20mg" ~ "High intensity statins",
      Max_Drug == "CAVSTAT" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "CRESTOR" & Max_Strength == "5mg" ~ "Moderate intensity statins",
      Max_Drug == "CRESTOR" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "CRESTOR" & Max_Strength == "20mg" ~ "High intensity statins",
      Max_Drug == "CRESTOR" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "CROSUVA" & Max_Strength == "20mg" ~ "High intensity statins",
      Max_Drug == "EZETIMIBE/ATORVASTATIN" & Max_Strength == "10mg/10mg" ~ "Moderate intensity statins",
      Max_Drug == "EZETIMIBE/ATORVASTATIN" & Max_Strength == "10mg/80mg" ~ "High intensity statins",
      Max_Drug == "EZETIMIBE/SIMVASTATIN" & Max_Strength == "10mg/40mg" ~ "Moderate intensity statins",
      Max_Drug == "EZETIMIBE/SIMVASTATIN" & Max_Strength == "10mg/80mg" ~ "Moderate intensity statins",
      Max_Drug == "EZETIMIBE/SIMVASTATIN" & Max_Strength == "10mg/10mg" ~ "Low intensity statins",
      Max_Drug == "FLUVASTATIN" & Max_Strength == "40mg" ~ "Low intensity statins",
      Max_Drug == "FLUVASTATIN" & Max_Strength == "20mg" ~ "Low intensity statins",
      Max_Drug == "LESCOL" & Max_Strength == "20mg" ~ "Low intensity statins",
      Max_Drug == "LESCOL" & Max_Strength == "40mg" ~ "Low intensity statins",
      Max_Drug == "LESCOL XL" & Max_Strength == "80mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPEX" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "LIPEX" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPEX" & Max_Strength == "5mg" ~ "Low intensity statins",
      Max_Drug == "LIPEX" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPEX" & Max_Strength == "80mg" ~ "Moderate intensity statins",      
      Max_Drug == "LIPITOR" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPITOR" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPITOR" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "LIPITOR" & Max_Strength == "80mg" ~ "High intensity statins",       
      Max_Drug == "LIPOSTAT" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "LIPOSTAT" & Max_Strength == "20mg" ~ "Low intensity statins",
      Max_Drug == "LIPOSTAT" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "LIPOSTAT" & Max_Strength == "80mg" ~ "Moderate intensity statins", 
      Max_Drug == "LORSTAT" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "LORSTAT" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "LORSTAT" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "LORSTAT" & Max_Strength == "80mg" ~ "High intensity statins", 
      Max_Drug == "PRAVACHOL" & Max_Strength == "5mg" ~ "Low intensity statins",
      Max_Drug == "PRAVACHOL" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "PRAVACHOL" & Max_Strength == "20mg" ~ "Low intensity statins",
      Max_Drug == "PRAVACHOL" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "PRAVACHOL" & Max_Strength == "80mg" ~ "Moderate intensity statins", 
      Max_Drug == "ROSUVASTATIN" & Max_Strength == "5mg" ~ "Moderate intensity statins",
      Max_Drug == "ROSUVASTATIN" & Max_Strength == "10mg" ~ "Moderate intensity statins",
      Max_Drug == "ROSUVASTATIN" & Max_Strength == "20mg" ~ "High intensity statins",
      Max_Drug == "ROSUVASTATIN" & Max_Strength == "40mg" ~ "High intensity statins",         
      Max_Drug == "SIMVACOR" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVACOR" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVAHEXAL" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "SIMVAHEXAL" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVAHEXAL" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVAR" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "SIMVAR" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVAR" & Max_Strength == "20mg" ~ "Moderate intensity statins",      
      Max_Drug == "SIMVAR" & Max_Strength == "80mg" ~ "Moderate intensity statins",       
      Max_Drug == "SIMVASTATIN" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "SIMVASTATIN" & Max_Strength == "40mg" ~ "Moderate intensity statins",
      Max_Drug == "SIMVASTATIN" & Max_Strength == "20mg" ~ "Moderate intensity statins",      
      Max_Drug == "SIMVASTATIN" & Max_Strength == "80mg" ~ "Moderate intensity statins",  
      Max_Drug == "TORVASTAT" & Max_Strength == "40mg" ~ "High intensity statins",
      Max_Drug == "TORVASTAT" & Max_Strength == "20mg" ~ "Moderate intensity statins", 
      Max_Drug == "TROVAS" & Max_Strength == "20mg" ~ "Moderate intensity statins",
      Max_Drug == "VASTIN" & Max_Strength == "20mg" ~ "Low intensity statins",
      Max_Drug == "VASTIN" & Max_Strength == "40mg" ~ "Low intensity statins",  
      Max_Drug == "VYTORIN" & Max_Strength == "10mg/10mg" ~ "Low intensity statins",
      Max_Drug == "VYTORIN" & Max_Strength == "10mg/20mg" ~ "Moderate intensity statins",
      Max_Drug == "VYTORIN" & Max_Strength == "10mg/40mg" ~ "Moderate intensity statins",      
      Max_Drug == "VYTORIN" & Max_Strength == "10mg/80mg" ~ "Moderate intensity statins",  
      Max_Drug == "ZIMSTAT" & Max_Strength == "5mg" ~ "Low intensity statins",
      Max_Drug == "ZIMSTAT" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "ZIMSTAT" & Max_Strength == "20mg" ~ "Moderate intensity statins",      
      Max_Drug == "ZIMSTAT" & Max_Strength == "40mg" ~ "Moderate intensity statins",   
      Max_Drug == "ZIMSTAT" & Max_Strength == "80mg" ~ "Moderate intensity statins",         
      Max_Drug == "ZOCOR" & Max_Strength == "5mg" ~ "Low intensity statins",
      Max_Drug == "ZOCOR" & Max_Strength == "10mg" ~ "Low intensity statins",
      Max_Drug == "ZOCOR" & Max_Strength == "20mg" ~ "Moderate intensity statins",      
      Max_Drug == "ZOCOR" & Max_Strength == "40mg" ~ "Moderate intensity statins",   
      Max_Drug == "ZOCOR" & Max_Strength == "80mg" ~ "Moderate intensity statins",          
      TRUE ~ "Other"
    )
  )

print(df)
table(df$Max_Statin_Intensity)

df1 <- Adherence_levels_4016
df2 <- df


df <- df1 %>%
  inner_join (df2, by = "Patient_UUID") 
  
print(df)  

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and Statin intensity on first and last visit in 2 years_4016.xlsx")



# Include a variable to show the direction of change in statin intensity

df <- Adherence_and_Statin_intensity_on_first_and_last_visit_in_2_years_4016

df <- df %>%
  mutate(
    Change_in_statin_intensity = case_when(
      Min_Statin_Intensity == "High intensity statins" & Max_Statin_Intensity == "High intensity statins" ~ "No Change",
      Min_Statin_Intensity == "High intensity statins" & Max_Statin_Intensity == "Moderate intensity statins" ~ "High to Moderate",
      Min_Statin_Intensity == "High intensity statins" & Max_Statin_Intensity == "Low intensity statins" ~ "High to Low",
      Min_Statin_Intensity == "Moderate intensity statins" & Max_Statin_Intensity == "High intensity statins" ~ "Moderate to High",
      Min_Statin_Intensity == "Moderate intensity statins" & Max_Statin_Intensity == "Moderate intensity statins" ~ "No Change",
      Min_Statin_Intensity == "Moderate intensity statins" & Max_Statin_Intensity == "Low intensity statins" ~ "Moderate to Low",
      Min_Statin_Intensity == "Low intensity statins" & Max_Statin_Intensity == "High intensity statins" ~ "Low to High",
      Min_Statin_Intensity == "Low intensity statins" & Max_Statin_Intensity == "Moderate intensity statins" ~ "Low to Moderate",
      Min_Statin_Intensity == "Low intensity statins" & Max_Statin_Intensity == "Low intensity statins" ~ "No Change",
      TRUE ~ "Other"
    )
  )

print(df)
table(df$Change_in_statin_intensity)

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and Statin intensity on first and last visit in 2 years_4016.xlsx")

# Cross-tab change in statin intensity with adherence

data <- Adherence_and_Statin_intensity_on_first_and_last_visit_in_2_years_4016
tbl <- table(data$Change_in_statin_intensity, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$Change_in_statin_intensity)



## For finding number of concurrent poly pharmacy

df1 <- Medications_data_5765_patients
df2 <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates

merged_df <- inner_join(df1, df2, by = c("Patient_UUID", "PrescribedDate"), relationship = "many-to-many") 

print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Merged_medication prescription in first 2 years 4016.xlsx")



# Manually change all the brand names to generic names of drugs

medication_data <- Merged_medication_prescription_in_first_2_years_4016 %>%
  group_by(Patient_UUID) %>%
  arrange(PrescribedDate) %>%  # Ensure visit dates are in ascending order
  mutate(Visit_Number = row_number()) %>%
  ungroup()

print(medication_data)

# Pivot the data wider to spread visits and corresponding variables
wide_data <- medication_data %>%
  pivot_wider(
    id_cols = Patient_UUID,
    names_from = Visit_Number,
    values_from = c(PrescribedDate, Medication),
    names_glue = "{.value}_{Visit_Number}"
  )

# Print the resulting wide dataset
print(wide_data)

write_xlsx(wide_data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Medication prescription rows to column_4016_medication count.xlsx")



# Now we will count the number of distinct statins for each patient.

# Function to count unique medicine names across a row
count_unique_medicines <- function(row) {
  # Remove NAs and empty strings
  row <- row[!is.na(row) & row != ""]
  # Get unique medicine names (ignoring order)
  unique_meds <- unique(row)
  # Return the number of distinct medicines
  length(unique_meds)
}

# Apply this function row-wise across the selected columns
df <- Medication_prescription_rows_to_column_4016_medication_count

df <- df %>%
  rowwise() %>%
  mutate(UniqueMedicineCount = count_unique_medicines(c_across(starts_with("Medication_")))) %>%
  ungroup()

# View the result
print(df)

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Medication count.xlsx")


# Now we will merge this with the adherence demography and strength excel sheet

df_1 <- Adherence_levels_4016
df_2 <- Medication_count

merged_df <- inner_join(df_1, df_2, by = c("Patient_UUID"))

print (merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and medication count_4016.xlsx")


#Cross-tab of Statin count with adherence

data <- Adherence_and_medication_count_4016
tbl <- table(data$UniqueMedicineCount, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$UniqueMedicineCount)


# Multimorbidity

df1 <- Medications
df2 <- Visits_upto_730_days_long_3_or_more_visits_without_duplicates

df2_clean <- df2 %>%
  distinct(Patient_UUID, PrescribedDate, .keep_all = TRUE)

merged_df <- df1 %>%
  inner_join(df2_clean, by = c("Patient_UUID", "PrescribedDate"))

n_distinct(merged_df$Patient_UUID)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Merged_visit reason in first 2 years 4016.xlsx")

df <- Merged_visit_reason_in_first_2_years_4016

keywords_htn <- c("hypertension", "ht (hypertension)", "ht\\(hypertension\\)", "ht/diabetes", "ht/eds", "ht/gp care", "diastolic ht", "htn", "diebetes / ht / care plan review",
                  "ht / gp care plan 721")

keywords_dm <- c("dm", "dm f/u", "dm with bp not to target", "iddm", "niddm", "t2dm", "type 1 dm", "diabetes", "ht/diabetes", "diebetes / ht / care plan review")

keywords_can <- c("cancer", "carcinoma", "screen")

keywords_asth <- c("asthma", "copd", "bronchi")

keywords_lip <- c("hyperlipid", "dyslipid", "lipid", "choles")

df_screened <- df %>%
  # Step 1: Detect keywords row by row
  mutate(
    has_htn  = str_detect(tolower(REASON), str_c(keywords_htn, collapse = "|")),
    has_dm   = str_detect(tolower(REASON), str_c(keywords_dm, collapse = "|")),
    has_can  = str_detect(tolower(REASON), str_c(keywords_can, collapse = "|")),
    has_asth = str_detect(tolower(REASON), str_c(keywords_asth, collapse = "|")),
    has_lip  = str_detect(tolower(REASON), str_c(keywords_lip, collapse = "|"))  # checked but not used
  ) %>%
  # Step 2: Collapse across patients
  group_by(Patient_UUID) %>%
  summarise(
    has_htn  = any(has_htn, na.rm = TRUE),
    has_dm   = any(has_dm, na.rm = TRUE),
    has_can  = any(has_can, na.rm = TRUE),
    has_asth = any(has_asth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 3: Build the combined Condition string
  mutate(
    Condition = case_when(
      has_htn & has_dm & has_can & has_asth ~ "hypertension and diabetes and cancer and copd",
      has_htn & has_dm & has_can            ~ "hypertension and diabetes and cancer",
      has_htn & has_dm & has_asth           ~ "hypertension and diabetes and copd",
      has_htn & has_can & has_asth          ~ "hypertension and cancer and copd",
      has_dm & has_can & has_asth           ~ "diabetes and cancer and copd",
      has_htn & has_dm                      ~ "hypertension and diabetes",
      has_htn & has_can                     ~ "hypertension and cancer",
      has_htn & has_asth                    ~ "hypertension and copd",
      has_dm & has_can                      ~ "diabetes and cancer",
      has_dm & has_asth                     ~ "diabetes and copd",
      has_can & has_asth                    ~ "cancer and copd",
      has_htn                               ~ "hypertension",
      has_dm                                ~ "diabetes",
      has_can                               ~ "cancer",
      has_asth                              ~ "copd",
      TRUE                                  ~ "No"
    )
  )


table(df_screened$Condition)

write_xlsx(df_screened, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Comorbidities_4016.xlsx")

# Now we will merge this with the adherence demography and comorbidities excel sheet

df_1 <- Adherence_levels_4016
df_2 <- Comorbidities_4016



merged_df <- inner_join(df_1, df_2, by = "Patient_UUID")
print (merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and comorbidities_4016.xlsx")


#Cross-tab of comorbidities with adherence

data <- Adherence_and_comorbidities_4016
tbl <- table(data$Condition, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$Condition)

# Manually add a variable Condition_recat for the number of comorbidities in Adherence_and_comorbidities_4016


#Cross-tab of comorbidities with adherence

data <- Adherence_and_comorbidities_4016
tbl <- table(data$Condition_recat, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$Condition_recat)






## Smoking status

df1 <- Adherence_and_Demography_4016
df2 <- SmokingAlcohol

merged_df <- inner_join(df1, df2, by = c("Patient_UUID"))

print (merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and Demography_4016.xlsx")


data <- Adherence_and_Demography_4016
tbl <- table(data$SmokingStatus, data$Adherence)
print(tbl)
chisq.test(tbl)
table(data$SmokingStatus)


# Recat Age as <=65 and >65

data <- Adherence_and_Demography_3432
data <- data %>%
  mutate(
    AgeCategory = cut(
      Age_at_Visit, 
      breaks = c(-Inf, 65, Inf),   # two bins: ≤65 and >65
      labels = c("≤ 65", "> 65"),
      right = TRUE   # include the right endpoint (so 65 goes into ≤65)
    )
  )
tbl <- table(data$AgeCategory, data$Adherence)
print(tbl)
table(data$AgeCategory)

write_xlsx(data, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence and Demography_4016.xlsx")




## Checking model fit

df1 <- Adherence_and_Demography_4016
df2 <- Adherence_and_first_statin_strength_4016
df3 <- Adherence_and_statin_count_4016
df4 <- Adherence_and_Statin_intensity_on_first_and_last_visit_in_2_years_4016
df5 <- Adherence_and_comorbidities_4016
df6 <- Adherence_and_medication_count_4016

merged_df <- df1 %>%
  inner_join(df2, by = "Patient_UUID") %>%
  inner_join(df3, by = "Patient_UUID") %>%
  inner_join(df4, by = "Patient_UUID")%>%
  inner_join(df5, by = "Patient_UUID")%>%
  inner_join(df6, by = "Patient_UUID")

print(merged_df)

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Adherence_OR Model_4016.xlsx")

# Manually delete the excess variables of Adherence. 
## Also manually change name of UniqueMedicineCount.x to UniqueMedicineCount_statin and UniqueMedicineCount.y to UniqueMedicineCount_all

df <- Adherence_OR_Model_3432


# Set <=65 as reference in age

df$AgeCategory <- factor(df$AgeCategory, levels = c("≤ 65", "> 65"))
df$AgeCategory <- relevel(df$AgeCategory, ref = "≤ 65")
levels(df$AgeCategory)
table(df$AgeCategory)

# Set Gender = Male as reference
df$GENDER <- factor(df$GENDER, levels = c("Male", "Female", "Transgender/Other"))
df$GENDER <- relevel(df$GENDER, ref = "Male")
levels(df$GENDER)
table(df$GENDER)

# Set Ethnicity = Non-ATSI as reference
df$Ethnicity_recat <- factor(df$Ethnicity_recat, levels = c("Non-ATSI", "ATSI", "Not Recorded"))
df$Ethnicity_recat <- relevel(df$Ethnicity_recat, ref = "Non-ATSI")
levels(df$Ethnicity_recat)
table(df$Ethnicity_recat)

# Set Occupation_recat = unemployed as reference
df$OCCUPATION_recat <- factor(df$OCCUPATION_recat)
df$OCCUPATION_recat <- relevel(df$OCCUPATION_recat, ref = "Unemployed")
levels(df$OCCUPATION_recat)
table(df$OCCUPATION_recat)

# Grhanite site
df$GRHANITE_Site <- factor(df$GRHANITE_Site)
df$GRHANITE_Site <- relevel(df$GRHANITE_Site, ref = "NSW_Tahmoor_MC")
levels(df$GRHANITE_Site)
table(df$GRHANITE_Site)

# Smoking status
df$SmokingStatus <- factor(df$SmokingStatus)
df$SmokingStatus <- relevel(df$SmokingStatus, ref = "Non Smoker")
levels(df$SmokingStatus)
table(df$SmokingStatus)

# Statin intensity at index date
df$Statin_Intensity_Category <- factor(df$Statin_Intensity_Category)
df$Statin_Intensity_Category <- relevel(df$Statin_Intensity_Category, ref = "Moderate intensity statins")
levels(df$Statin_Intensity_Category)
table(df$Statin_Intensity_Category)

# Statin type at index date
df$Statin_type <- factor(df$Statin_type)
df$Statin_type <- relevel(df$Statin_type, ref = "Atorvastatin")
levels(df$Statin_type)
table(df$Statin_type)

# Number of statin types
df$UniqueMedicineCount_statin <- factor(df$UniqueMedicineCount_statin)
df$UniqueMedicineCount_statin <- relevel(df$UniqueMedicineCount_statin, ref = "1")
levels(df$UniqueMedicineCount_statin)
table(df$UniqueMedicineCount_statin)

# Change in statin intensity
df$Change_in_statin_intensity <- factor(df$Change_in_statin_intensity)
df$Change_in_statin_intensity <- relevel(df$Change_in_statin_intensity, ref = "No Change")
levels(df$Change_in_statin_intensity)
table(df$Change_in_statin_intensity)

# Number of polypharmacy
df$UniqueMedicineCount_all <- factor(df$UniqueMedicineCount_all)
df$UniqueMedicineCount_all <- relevel(df$UniqueMedicineCount_all, ref = "1")
levels(df$UniqueMedicineCount_all)
table(df$UniqueMedicineCount_all)

#Number of comorbidities
df$Condition_recat <- factor(df$Condition_recat)
df$Condition_recat <- relevel(df$Condition_recat, ref = "None")
levels(df$Condition_recat)
table(df$Condition_recat)



df$Adherence_binary <- ifelse(data$Adherence == "Adherent", 1, 0)
model <- glm(Adherence_binary ~ AgeCategory + GENDER + Ethnicity_recat + GRHANITE_Site + OCCUPATION_recat + SmokingStatus + Statin_Intensity_Category + Statin_type + UniqueMedicineCount_statin + Change_in_statin_intensity + UniqueMedicineCount_all + Condition_recat,
             data = df, family = binomial)

summary(model)

# Checking multicollinearity

vif(model)


# Model fit – Hosmer–Lemeshow test

hoslem.test(model$y, fitted(model), g = 10)


# Model discrimination – ROC curve & AUC

roc_obj <- roc(df$Adherence, fitted(model))
plot(roc_obj, col = "blue")
auc(roc_obj)



## Adjusted OR

model <- glm(Adherence_binary ~ AgeCategory + GENDER + Ethnicity_recat + GRHANITE_Site + OCCUPATION_recat + SmokingStatus + Statin_Intensity_Category + Statin_type + UniqueMedicineCount_statin + Change_in_statin_intensity + UniqueMedicineCount_all + Condition_recat,
             data = df, family = binomial)

summary(model)

results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
print(results)

print(results, n=44)

write_xlsx(results, "C:/UNSW/Shagoofa/Year 3/Predictors of statin adherence and intolerance/Dataset/Shagoofa_Dataset_v5/Working file/Adherence/Manuscript/AOR_4016.xlsx")

levels(df$Adherence_binary)



# Unadjusted OR

predictors <- c("AgeCategory", "GENDER", "Ethnicity_recat", "GRHANITE_Site", "OCCUPATION_recat", "SmokingStatus", "Statin_Intensity_Category", "Statin_type", "UniqueMedicineCount_statin", "Change_in_statin_intensity", "UniqueMedicineCount_all", "Condition_recat")

unadj_results <- lapply(predictors, function(var) {
  m <- glm(as.formula(paste("Adherence_binary ~", var)),
           data = df,
           family = binomial)
  
  coefs <- summary(m)$coefficients   # table with log-odds, SE, z, p-value
  ORs <- exp(coef(m))                # odds ratios
  CI <- exp(confint(m))              # confidence intervals
  
  # Combine into a clean table
  results <- cbind(
    Predictor = rownames(coefs),
    OR = ORs,
    CI_lower = CI[,1],
    CI_upper = CI[,2],
    p_value = coefs[,4]
  )
  
  results <- results[rownames(results) != "(Intercept)", ]  # drop intercept
  return(as.data.frame(results))
})

names(unadj_results) <- predictors
unadj_results


## Sensitivity analysis

model <- glm(Adherence_binary ~ AgeCategory + GENDER + Ethnicity_recat + GRHANITE_Site + OCCUPATION_recat + SmokingStatus + Statin_Intensity_Category + Statin_type + UniqueMedicineCount_statin + Change_in_statin_intensity + UniqueMedicineCount_all + Condition_recat,
             data = df, family = binomial)

summary(model)

model_step <- stepAIC(model, direction = "both")
summary(model_step)

results <- tidy(model_step, exponentiate = TRUE, conf.int = TRUE)
print(results)
print(results, n = 31)

#Checking the better model

AIC(model, model_step)
anova(model_step, model, test = "Chisq")

library(pROC)
roc_full <- roc(df$Adherence_binary, fitted(model))
roc_step <- roc(df$Adherence_binary, fitted(model_step))

auc(roc_full)
auc(roc_step)
roc.test(roc_full, roc_step)


## Sensitivity analysis 2 (without grhanite site)

model <- glm(Adherence_binary ~ AgeCategory + GENDER + Ethnicity_recat + GRHANITE_Site + OCCUPATION_recat + SmokingStatus + Statin_Intensity_Category + Statin_type + UniqueMedicineCount_statin + Change_in_statin_intensity + UniqueMedicineCount_all + Condition_recat,
             data = df, family = binomial)

summary(model)

model_reduced <- glm(Adherence_binary ~ AgeCategory + OCCUPATION_recat + SmokingStatus + Change_in_statin_intensity + UniqueMedicineCount_all + Condition_recat,
                     data = df, family = binomial)


results <- tidy(model_reduced, exponentiate = TRUE, conf.int = TRUE)
print(results)
print(results, n = 21)

#Checking the better model

AIC(model, model_reduced)
anova(model_reduced, model, test = "Chisq")

roc_full <- roc(df$Adherence_binary, fitted(model))
roc_reduced <- roc(df$Adherence_binary, fitted(model_reduced))

auc(roc_full)
auc(roc_reduced)
roc.test(roc_full, roc_reduced)


df <- df %>%
  mutate(Age_recat = cut(
    Age_at_Visit, 
    breaks = c(18, 30, 40, 50, 60, 70, Inf),  # Breakpoints for binning
    labels = c("18-30", "30-40", "40-50", "50-60", "60-70", "≥ 70"),
    right = FALSE  # Exclude the right endpoint (non-inclusive)
  ))
tbl <- table( df$Age_recat, df$GENDER, df$Adherence)
print(tbl)
table(df$Age_recat)


df1<-Patients_with_PDC_130_or_more
df2<-Visits_upto_730_days_long_5765

df3 <- df2 %>%
  semi_join(df1, by = "Patient_UUID")
nrow(df3)

