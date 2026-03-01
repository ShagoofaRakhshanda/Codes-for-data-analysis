install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
install.packages("recipes")
library(recipes)
install.packages("ModelMetrics")
library(ModelMetrics)
install.packages("caret", dependencies = TRUE)
install.packages("epiR")
library(epiR)
install.packages("pROC")
library(pROC)
install.packages("PRROC")
library(PRROC)
install.packages("irr")
library(irr)
install.packages("UpSetR")
library(UpSetR)






df <- X1271

df$Tag <- ifelse(df$Patient_UUID %in% df$Predicted_1270, 1, 0)

write_xlsx(df, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/DF.xlsx")



df <- Final_count_all
cor.test(df$Goldstandard_575, df$Singapore_EHR_B_1270, method = "pearson")
cor.test(df$Goldstandard_575, df$CRB_251, method = "pearson")
cor.test(df$Goldstandard_575, df$NHS_UK_50, method = "pearson")
cor.test(df$Goldstandard_575, df$Singapore_EHR_C_45, method = "pearson")
cor.test(df$Goldstandard_575, df$Singapore_EHR_D_39, method = "pearson")
cor.test(df$Goldstandard_575, df$Singapore_EHR_A_34, method = "pearson")
cor.test(df$Goldstandard_575, df$Japan_SIMS_34, method = "pearson")
cor.test(df$Goldstandard_575, df$USA_SIMs_EHR_27, method = "pearson")
cor.test(df$Goldstandard_575, df$Japan_EHR_21, method = "pearson")




#ROC AUC

df <- Final_count_all

algorithms <- c("CRB_251", "USA_SIMs_EHR_27", "Japan_SIMS_34", "Singapore_EHR_A_34", "Singapore_EHR_B_1270", "Singapore_EHR_C_45", "Singapore_EHR_D_39", "Japan_EHR_21", "NHS_UK_50")

auc_df <- data.frame(Algorithm = character(),
                     AUC = numeric(),
                     Lower_CI = numeric(),
                     Upper_CI = numeric(),
                     stringsAsFactors = FALSE)

roc_list <- list()

for (alg in algorithms) {
  # Logistic regression
  model <- glm(as.formula(paste("Goldstandard_575 ~", alg)),
               data = df, family = binomial)
  
  # Predicted probabilities
  pred_probs <- predict(model, type = "response")
  
  # ROC object
  roc_obj <- roc(df$Goldstandard_575, pred_probs)
  roc_list[[alg]] <- roc_obj
  
  # AUC
  auc_val <- auc(roc_obj)
  
  # 95% CI
  ci_val <- ci.auc(roc_obj)
  
  # Add to data frame
  auc_df <- rbind(auc_df,
                  data.frame(Algorithm = alg,
                             AUC = round(auc_val, 3),
                             Lower_CI = round(ci_val[1], 3),
                             Upper_CI = round(ci_val[3], 3)))
}

print(auc_df)

colors <- c("blue", "red", "green", "orange", "purple", "brown", "cyan", "magenta", "darkgreen")

plot(roc_list[[1]], col = colors[1], lwd = 2, main = "ROC Curves for All Algorithms")
for (i in 2:length(roc_list)) {
  lines(roc_list[[i]], col = colors[i], lwd = 2)
}
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = algorithms, col = colors, lwd = 2, cex = 0.8)


## PR AUC

set.seed(123)  # reproducibility

# Prevalence
prevalence <- 0.0524

algorithms <- c("CRB_251", "USA_SIMs_EHR_27", "Japan_SIMS_34", "Singapore_EHR_A_34", "Singapore_EHR_B_1270", "Singapore_EHR_C_45", "Singapore_EHR_D_39", "Japan_EHR_21", "NHS_UK_50")

pr_list <- list()
auc_df <- data.frame(Algorithm = character(),
                     PR_AUC = numeric(),
                     CI_lower = numeric(),
                     CI_upper = numeric(),
                     stringsAsFactors = FALSE)

B <- 1000  # bootstrap replicates

# Loop over algorithms
for (alg in algorithms) {
  
  # Fit logistic regression and get predicted probabilities
  model <- glm(as.formula(paste("Goldstandard_575 ~", alg)), 
               data = df, family = binomial)
  pred_probs <- predict(model, type = "response")
  
  # PR curve (original data)
  pr_obj <- pr.curve(scores.class0 = pred_probs[df$Goldstandard_575 == 1],
                     scores.class1 = pred_probs[df$Goldstandard_575 == 0],
                     curve = TRUE)
  pr_list[[alg]] <- pr_obj
  
  # Bootstrap for 95% CI
  pr_auc_boot <- numeric(B)
  for (b in 1:B) {
    idx <- sample(1:nrow(df), size = nrow(df), replace = TRUE)
    df_boot <- df[idx, ]
    
    model_boot <- glm(as.formula(paste("Goldstandard_575 ~", alg)), 
                      data = df_boot, family = binomial)
    pred_probs_boot <- predict(model_boot, type = "response")
    
    pr_obj_boot <- pr.curve(scores.class0 = pred_probs_boot[df_boot$Goldstandard_575 == 1],
                            scores.class1 = pred_probs_boot[df_boot$Goldstandard_575 == 0],
                            curve = FALSE)
    
    pr_auc_boot[b] <- pr_obj_boot$auc.integral
  }
  
  ci_lower <- quantile(pr_auc_boot, 0.025)
  ci_upper <- quantile(pr_auc_boot, 0.975)
  pr_auc <- mean(pr_auc_boot)
  
  # Store results
  auc_df <- rbind(auc_df, 
                  data.frame(Algorithm = alg,
                             PR_AUC = round(pr_auc, 3),
                             CI_lower = round(ci_lower, 3),
                             CI_upper = round(ci_upper, 3)))
}

print(auc_df)

# Plot all PR curves together
colors <- c("blue", "red", "green", "orange", "purple", "brown", "cyan", "magenta", "darkgreen")

plot(pr_list[[1]]$curve[,1], pr_list[[1]]$curve[,2], type = "l", col = colors[1],
     xlab = "Recall", ylab = "Precision", lwd = 2,
     main = "Precision-Recall Curves for All Algorithms",
     xlim = c(0,1), ylim = c(0,1))

for (i in 2:length(pr_list)) {
  lines(pr_list[[i]]$curve[,1], pr_list[[i]]$curve[,2], col = colors[i], lwd = 2)
}

# Add baseline for prevalence
abline(h = prevalence, lty = 2, col = "gray") 

legend("topright", legend = algorithms, col = colors, lwd = 2, cex = 0.8)








#ROC curve

algorithms <- c("Singapore_EHR_B_1270","CRB_251","NHS_UK_50",
                "Singapore_EHR_C_45","Singapore_EHR_D_39","Singapore_EHR_A_34",
                "Japan_SIMS_34","USA_SIMs_EHR_27","Japan_EHR_21")

colors <- c("blue","red","green","yellow","purple","orange","cyan","magenta","brown")

# Algorithm names for legend
alg_names <- c("Singapore SIMs-B","Minnesota CRB","NHS-UK",
               "Singapore SIMs-C","Singapore SIMs-D","Singapore SIMs-A",
               "Japan SIMs","USA SIMs","Japan SAMT")

# Compute ROC objects and AUCs
roc_list <- lapply(algorithms, function(alg) {
  roc(df$Goldstandard_575, df[[alg]], levels=c(0,1), direction="<")
})

auc_list <- sapply(roc_list, auc)

# Plot the first ROC
plot(roc_list[[1]], col=colors[1], lwd=2, main="ROC Curves for All Algorithms")

# Add remaining ROC curves
for(i in 2:length(roc_list)){
  lines(roc_list[[i]], col=colors[i], lwd=2)
}

# Add diagonal reference line
abline(a=0, b=1, lty=2, col="gray")

# Add legend with AUC
par(xpd=TRUE, mar=c(5,4,4,8))  # allow drawing outside right margin
plot(roc_list[[1]], col=colors[1], lwd=2, main="ROC Curves for All Algorithms")

for(i in 2:length(roc_list)){
  lines(roc_list[[i]], col=colors[i], lwd=2)
}
abline(a=0, b=1, lty=2, col="gray")

legend("bottomright",  inset=c(-0.3,0), 
       legend = paste0(alg_names, " (AUC=", round(auc_list,3), ")"),
       col = colors, lwd = 2, cex=0.7,
       ncol = 9) 




#PR AUC
df <- Final_count_all
compute_pr_auc <- function(preds, labels, B=2000, seed=123) {
  set.seed(seed)
  n <- length(labels)
  pr_aucs <- numeric(B)
  
  for (i in 1:B) {
    idx <- sample(1:n, n, replace=TRUE)  # bootstrap sample
    fg <- preds[idx][labels[idx] == 1]   # positives
    bg <- preds[idx][labels[idx] == 0]   # negatives
    pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = FALSE)
    pr_aucs[i] <- pr$auc.integral
  }
  
  # Point estimate
  fg <- preds[labels == 1]
  bg <- preds[labels == 0]
  pr_obj <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = FALSE)
  pr_point <- pr_obj$auc.integral
  
  # 95% CI
  pr_ci <- quantile(pr_aucs, probs=c(0.025,0.975))
  
  return(list(AUC=pr_point, CI_lower=pr_ci[1], CI_upper=pr_ci[2]))
}
algorithms <- c("Singapore_EHR_B_1270","CRB_251","NHS_UK_50","Singapore_EHR_C_45","Singapore_EHR_D_39", "Singapore_EHR_A_34", "Japan_SIMS_34", "USA_SIMs_EHR_27", "Japan_EHR_21")

pr_results <- lapply(algorithms, function(alg) {
  res <- compute_pr_auc(df[[alg]], df$Goldstandard_575)
  data.frame(
    Algorithm = alg,
    PR_AUC = round(res$AUC,3),
    Lower_CI = round(res$CI_lower,3),
    Upper_CI = round(res$CI_upper,3),
    PR_AUC_CI = paste0(round(res$AUC,3)," (",round(res$CI_lower,3),"-",round(res$CI_upper,3),")")
  )
})

pr_df <- do.call(rbind, pr_results)
pr_df





# measures of agreement (κ statistic)

df <- Final_count_all
data_pair <- data.frame(
  gold = df$Goldstandard_575,
  alg1 = df$Japan_EHR_21)
kappa_result <- kappa2(data_pair, "unweighted")
print(kappa_result)








df <- Final_count_all
compute_mcc <- function(gold, pred) {
  TP <- sum(as.numeric(pred == 1 & gold == 1))
  TN <- sum(as.numeric(pred == 0 & gold == 0))
  FP <- sum(as.numeric(pred == 1 & gold == 0))
  FN <- sum(as.numeric(pred == 0 & gold == 1))
  
  numerator <- TP * TN - FP * FN
  denominator <- sqrt(as.numeric((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))
  
  if (denominator == 0 || is.na(denominator)) return(0)
  return(numerator / denominator)
}

# Bootstrap function for one algorithm
mcc_boot_fun <- function(data, indices) {
  gold <- df$Goldstandard_575[indices]
  pred <- df$Japan_EHR_21[indices]
  compute_mcc(gold, pred)
}

# Run bootstrap
set.seed(123)
boot_res <- boot(df, mcc_boot_fun, R = 1000)

# 95% CI with 5 decimal places
ci <- boot.ci(boot_res, type = "perc")$percent[4:5]
ci_rounded <- round(ci, 5)
ci_rounded
boot_res









# Heatmap style table

# Load packages
install.packages("tidyverse")
library(tidyverse)
install.packages("reshape2")
library(reshape2)
install.packages("scales")
library(scales)
install.packages("dplyr")
library(dplyr) # for color formatting

# Create dataframe
df <- data.frame(
  Algorithm = c("Minnesota CRB", "USA SIMs", "Japan SIMs", "Singapore SIMs-A", "Singapore SIMs-B", "Singapore SIMs-C","Singapore SIMs-D", "Japan SAMT", "NHS-UK"),
  Accuracy = c(44.12, 42.07, 42.88, 42.73, 57.05, 42.22, 42.51, 42.80, 42.29),
  Sensitivity = c(17.63, 1.76, 2.90, 2.77, 92.95, 3.02, 2.90, 2.02, 3.40),
  Specificity = c(80.70, 97.74, 98.09, 97.91, 7.48, 96.35, 97.22, 99.13, 96.00),
  PPV = c(55.78,51.85,67.65,64.71,58.11,53.33,58.97,76.19,54.00),
  NPV = c(41.50,41.88,42.25,42.17,43.43,41.84,42.03,42.28,41.85),
  F1 = c(26.79,3.41,5.56,5.31,71.51,5.72,5.52,3.93,6.40),
  MCC = c(-0.021,-0.018,0.031,0.022,0.008,-0.017,0.003,0.046,-0.016),
  Kappa = c(-0.015,-0.004,0.008,0.006,0.005,-0.005,0.001,0.010,-0.005),
  ROC_AUC = c(0.49,0.50,0.51,0.50,0.50,0.50,0.50,0.51,0.50),
  PR_AUC = c(0.57,0.58,0.59,0.59,0.58,0.58,0.58,0.59,0.58)
)

# Reshape for heatmap
df_long <- df %>%
  pivot_longer(cols = -Algorithm, names_to = "Metric", values_to = "Value")

library(ggplot2)

# Plot heatmap
ggplot(df_long, aes(x=Metric, y=Algorithm, fill=Value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="yellow", mid="#CC0000", high="#32CD32", midpoint=0.5, limits=c(min(df_long$Value), max(df_long$Value))) +
  geom_text(aes(label=round(Value,2)), size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Performance Metrics Across Algorithms",
       fill="Metric Value")

install.packages("formattable")
library(formattable)

formattable(df, list(
  Accuracy = color_tile("red", "green"),
  Sensitivity = color_tile("red", "green"),
  Specificity = color_tile("red", "green"),
  F1 = color_tile("red", "green"),
  MCC = color_tile("red", "green"),
  PPV = color_tile("red", "green"),
  NPV = color_tile("red", "green"),
  Kappa = color_tile("red", "green"),
  ROC_AUC = color_tile("red", "green"),
  PR_AUC= color_tile("red", "green")
))

cat("Legend: Red = low values, Green = high values")

library(reshape2)
library(ggplot2)

df_long <- melt(df, id.vars = "Algorithm")
ggplot(df_long, aes(x = variable, y = Algorithm, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  labs(fill = "Metric value")





## For venn diagram

df <- Final_count_all

install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)


ComplexUpset::upset(
  df,
  intersect = c(
    "Singapore_SIMs_B","Minnesota_CRB","NHS_UK",
    "Singapore_SIMs_C","Singapore_SIMs_D","Singapore_SIMs_A",
    "Japan_SIMs","USA_SIMs","Japan_SAMT","Gold_standard"
  ),
  base_annotations = list(
    'Intersection size' = intersection_size(counts = TRUE)
  ),
  width_ratio = 0.3,
  name = "Algorithms",
  set_sizes = FALSE   # <-- disables the set size bars
) +
  ggtitle("Overlap Between Statin Intolerance Algorithms") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






# With <5 as a separate group

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)

# ---- USER: update this to your actual dataframe name ----
# df <- your_dataframe

# Define the algorithm columns (must exist in df)
sets <- c(
  "Singapore_SIMs_B","Minnesota_CRB","NHS_UK",
  "Singapore_SIMs_C","Singapore_SIMs_D","Singapore_SIMs_A",
  "Japan_SIMs","USA_SIMs","Japan_SAMT","Gold_standard"
)

# Basic checks
if (!all(sets %in% names(df))) {
  stop("Some 'sets' columns are missing from df. Check column names.")
}

# Ensure binary membership: convert logical/TRUE/FALSE or counts >0 into 1/0
df_bin <- df %>%
  mutate(across(all_of(sets), ~ ifelse(is.na(.x), 0,
                                       ifelse(.x %in% c(TRUE, "TRUE"), 1,
                                              ifelse(.x > 0, 1, as.integer(.x))))))

# 1) Build a human-readable pattern string per row (list of set names that are 1)
df_patterns <- df_bin %>%
  rowwise() %>%
  mutate(pattern_sets = {
    hits <- sets[which(c_across(all_of(sets)) == 1)]
    if (length(hits) == 0) NA_character_ else paste(hits, collapse = ";")
  }) %>%
  ungroup()

# 2) Count rows per unique pattern_sets (NA -> "None" if you want)
pattern_counts <- df_patterns %>%
  mutate(pattern_sets = ifelse(is.na(pattern_sets), "None", pattern_sets)) %>%
  count(pattern_sets, name = "count") %>%
  arrange(desc(count))

# 3) Collapse patterns with count < threshold into "<5"
threshold <- 5
pattern_counts <- pattern_counts %>%
  mutate(label = ifelse(count < threshold, "<5", pattern_sets))

agg_counts <- pattern_counts %>%
  group_by(label) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

# 4) Identify 'big' patterns (count >= threshold) for a truthful membership matrix
big_patterns <- pattern_counts %>%
  filter(count >= threshold) %>%
  arrange(desc(count)) %>%
  mutate(pattern_id = row_number())

# 5) Build long membership data for big patterns for plotting the matrix
#    Each big pattern -> one row in matrix; we create a row for each set indicating membership 1/0
matrix_long <- big_patterns %>%
  # split pattern_sets into vector of set names
  mutate(members_list = strsplit(pattern_sets, ";")) %>%
  select(pattern_id, pattern_sets, count, members_list) %>%
  unnest_longer(members_list, values_to = "member_set") %>%
  # create a row per pattern x set with member=1 if member_set equals set, else 0
  complete(pattern_id, member_set = sets, fill = list(member_set = NA)) %>%
  mutate(member = ifelse(!is.na(member_set) & grepl(member_set, pattern_sets), 1, 0)) %>%
  # ensure ordering of sets top->bottom in plot (reverse here so top is first element in 'sets')
  mutate(set = factor(member_set, levels = rev(sets))) %>%
  select(pattern_id, pattern_sets, count, set, member) %>%
  # set label factor order to match bar plot order later (big patterns only)
  mutate(pattern_label = factor(pattern_sets, levels = rev(big_patterns$pattern_sets)))

# 6) Bar plot of aggregated intersection counts (includes "<5")
agg_counts <- agg_counts %>%
  mutate(label = factor(label, levels = rev(label)))  # preserve display order

barp <- ggplot(agg_counts, aes(x = label, y = total)) +
  geom_col() +
  coord_flip() +
  labs(x = "Intersection (groups)", y = "Count", title = paste0("Intersection counts (collapsed <", threshold, ")")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

# 7) Membership matrix plot for big patterns (truthful membership)
#    We use pattern_label as x and set as y; dot if member==1
matrix_plot <- ggplot(matrix_long, aes(x = pattern_label, y = set)) +
  geom_point(aes(size = member), shape = 16) +
  scale_size_continuous(range = c(0, 4), breaks = c(0,1), labels = c("", "1")) +
  labs(x = NULL, y = NULL, title = paste0("Membership matrix (patterns with count >= ", threshold, ")")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# 8) Combine plots vertically
combined <- matrix_plot / barp + plot_layout(heights = c(2, 1))
print(combined)

# ---- End ----

# Notes:
# - The "<5" bar aggregates counts but does NOT have a truthful membership matrix row
#   (because it mixes different membership patterns).
# - If you want an approximate representation of "<5" membership in the matrix (e.g., show a dot
#   if any small pattern included that set), I can add that — but it will be an approximation.




library(dplyr)

df_overlap <- df %>%
  select(Gold_standard, Singapore_SIMs_B, Minnesota_CRB, NHS_UK,
         Singapore_SIMs_C, Singapore_SIMs_D, Singapore_SIMs_A,
         Japan_SIMs, USA_SIMs, Japan_SAMT) %>%
  summarise(across(-Gold_standard, ~sum(.x & Gold_standard))) %>%
  pivot_longer(cols = everything(), names_to = "Algorithm", values_to = "Overlap_with_GoldStandard")

df_overlap

library(ggplot2)

ggplot(df_overlap, aes(x = Algorithm, y = Overlap_with_GoldStandard, fill = Algorithm)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Overlap of Each Algorithm with Gold Standard", y = "Number of Patients")




GS <- Final_count_all
Demo <- SmokingAlcohol_SI

merged_df <- inner_join(GS, Demo, by = "Patient_UUID")

merged_df

write_xlsx(merged_df, "C:/UNSW/Shagoofa/Year 3/Phenotyping of statin intolerance/Manuscript/Data/GS_SmokingAlcohol.xlsx")

SmokingAlcohol <- GS_SmokingAlcohol
tbl <- table(SmokingAlcohol$SmokingStatus, SmokingAlcohol$Gold_standard)
print(tbl)


df <- Demographics
tbl <- table(df$ETHNICTY)
print(tbl)
