#Meta analysis of Relative Risk (RR)
install.packages('meta')
library(meta)
x <- metabin(data = RCT_Intervention_deliverer, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="RR", studlab = Study, byvar = Intervention_delivered_by)
x
forest(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Submission/EJPC/Revision/Forest plot/RR_RCT_Int deliverer.jpg", 
     width = 1200, 
     height = 800)
forest(x)
dev.off()
funnel(x)


--------------------------------------
  
#Meta analysis of Odds Ratio (OR)
install.packages('meta')
library(meta)
x <- metabin(data = Adherence_measures, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="OR", studlab = Study, subgroup = Adherence_measure)
x
forest(x, overall = FALSE)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Submission/EJPC/Revision/Forest plot/OR_Adherence measures.jpg", 
     width = 1200, 
     height = 1000)
forest(x, overall = FALSE)
dev.off()
funnel(x)




----------------------------
  
  
#Meta analysis of Relative Risk (RR) for adherence
install.packages('meta')
library(meta)
x <- metabin(data = All_adherence_measures, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="RR", studlab = Study)
x
forest (x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Submission/EJPC/Revision/Forest plot/RR_Publication bias.jpg", 
    width = 1200, 
    height = 1000)
forest(x)
dev.off()
funnel(x)


-----------------------

  
#SMD of LDL-C 
install.packages('meta')
library(meta)
x <- metacont(data = LDL_C, I_Total, I_Mean_Chol, I_SD, C_Total, C_Mean_Chol, C_SD, method.smd = "H", sm = "SMD", random = T, fixed = F, studlab = Study, byvar = Intervention_strategy)
x
forest(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Forest plot new 2/MA_LDL-C.jpg", 
     width = 1000, 
     height = 700)
forest(x)
dev.off()



#SMD of TC 
install.packages('meta')
library(meta)
x <- metacont(data = TC, I_Total, I_Mean_Chol, I_SD, C_Total, C_Mean_Chol, C_SD, method.smd = "H", sm = "SMD", random = T, fixed = F, studlab = Study, byvar = Intervention_strategy)
x
forest(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Forest plot new 2/MA_TC.jpg", 
     width = 1200, 
     height = 500)
forest(x)
dev.off()


----------------------
  
  
#Meta analysis of Odds Ratio (OR) non-digital vs digital or mixed 
install.packages('meta')
library(meta)
x <- metabin(data = Study_type, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="OR", studlab = Study, byvar = Study_type)
x
forest(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Forest plots new/MA_Study type.jpg", 
     width = 1200, 
     height = 800)
forest(x)
dev.off()


#Meta analysis of Odds Ratio (OR) non-digital vs digital vs mixed 
install.packages('meta')
library(meta)
x <- metabin(data = Intervention_deliverer, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="OR", studlab = Study, byvar = Intervention_delivered_by)
x
forest(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/MA_Intervention deliverer.jpg", 
     width = 1200, 
     height = 800)
forest(x)
dev.off()

---------------
#Publication bias: funnel plots and egger's test

install.packages('meta')
library(meta)
x <- metabin(data = Statin_intolerance, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="RR", studlab = Study)
x
forest(x)
funnel(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin intolerance_Final tables and graphs/Submission/BMJ Evidence based medicine/Revision/Forrest plots/Publication bias/RR_Publication bias_NRSI.jpg", 
     width = 1200, 
     height = 800)
funnel(x)
dev.off()

text(x$TE, x$seTE, labels = x$studlab, pos = 4, cex = 0.7, offset = 20)
egger_test <- metabias(x, method.bias = "egger")
print(egger_test)

# publication bias without visual outliers
x <- metabin(data = All_adherence_measures, I_Event, I_Total, C_Event, C_Total, random = T, fixed = F, method = "MH", sm="RR", studlab = Study)
x
forest(x)
funnel(x)
jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Forest plot new 2/Publication bias without calculated outliers.jpg", 
     width = 1200, 
     height = 800)
funnel(x)
dev.off()

# Finding outliers
rr_values <- x$TE
se_values <- x$seTE
overall_effect <- x$TE.random
overall_se <- x$seTE.random
upper_limit <- overall_effect + 1.96 * overall_se
lower_limit <- overall_effect - 1.96 * overall_se
funnel_data <- data.frame(study = x$studlab,
                          RR = rr_values,
                          SE = se_values)
outlier_indices <- which(funnel_data$RR < lower_limit | funnel_data$RR > upper_limit)
outlier_studies <- funnel_data[outlier_indices, ]
print(outlier_studies)
print(funnel_data)
print(overall_se)
print (overall_effect)

forest(x)








## Network meta-analysis

install.packages("netmeta")
library(netmeta)

pw <- pairwise(treat = treat, event = event, n = n,
               studlab = Study, data = Network_meta_analysis, sm = "RR")

net <- netmeta(TE = pw$TE, seTE = pw$seTE,
               treat1 = pw$treat1, treat2 = pw$treat2,
               studlab = pw$studlab, sm = "RR", reference.group = "Control")

summary(net)
forest(net)

jpeg("C:/UNSW/Shagoofa/Year 3/Systematic review and meta-analysis/Full text review/SR and MA_Statin adherence_Final tables and graphs/Submission/EJPC/Revision/Forest plot/Network Meta analysis.jpg", 
     width = 1200, 
     height = 800)
forest(net)
dev.off()

net$I2

split_result <- netsplit(net)
plot(split_result)






## Meta regression
library(dplyr)
df_clean <- Meta_regression %>%
  group_by(Study) %>%
  filter(n() == 2) %>% # Keep only studies with 2 rows (1 intervention + 1 control)
  ungroup()

meta_list <- df_clean %>%
  group_by(Study) %>%
  group_split()

meta_results <- lapply(meta_list, function(group) {
  if (nrow(group) != 2) return(NULL)
  
  control <- group %>% filter(Intervention_strategy == "Control")
  intervention <- group %>% filter(Intervention_strategy != "Control")
  
  if (nrow(control) == 1 && nrow(intervention) == 1) {
    metabin(
      event.e = intervention$event,
      n.e = intervention$n,
      event.c = control$event,
      n.c = control$n,
      studlab = group$Study[1],
      sm = "OR",
      method = "MH",
      incr = 0.5
    )
  } else {
    return(NULL)
  }
})

meta_results <- Filter(Negate(is.null), meta_results)
TEs <- sapply(meta_results, function(x) x$TE)
seTEs <- sapply(meta_results, function(x) x$seTE)
studies <- sapply(meta_results, function(x) x$studlab)
meta_combined <- metagen(
  TE = TEs,
  seTE = seTEs,
  studlab = studies,
  sm = "OR",
  method.tau = "REML"
)

moderators <- df_clean %>%
  filter(Intervention_strategy != "Control") %>%
  select(Study, Intervention_type, Intervention_setting, Intervention_deliverer)

meta_combined$Intervention_type <- moderators$Intervention_type[match(meta_combined$studlab, moderators$Study)]
meta_combined$Intervention_setting <- moderators$Intervention_setting[match(meta_combined$studlab, moderators$Study)]
meta_combined$Intervention_deliverer <- moderators$Intervention_deliverer[match(meta_combined$studlab, moderators$Study)]

meta_regression <- metareg(meta_combined, ~ Intervention_type + Intervention_setting + Intervention_deliverer)
summary(meta_regression)
bubble(Meta_regression)




## Contour enhanced funnel plot
# install.packages("metafor")
library(metafor)
library(meta)


dat <- All_adherence_measures
dat <- All_adherence_measures_without_calc_outliers

m <- metabin(event.e = I_Event,
             n.e = I_Event + I_No_Event,
             event.c = C_Event,
             n.c = C_Event + C_No_Event,
             studlab = Study,
             data = dat,
             sm = "RR",      # risk ratio
             method = "MH",  # Mantel-Haenszel method
             fixed = FALSE,
             random = TRUE)  # random-effects

col.contour <- c("gray75", "gray85", "gray95")

funnel(m,
       xlab = "Log Risk Ratio",
       ylab = "Standard Error",
       log = "x",                  # log scale for RR
       xlim = c(0.5, 2.5),         # adjust to your data
       contour = c(0.9, 0.95, 0.99),
       col.contour = col.contour)

