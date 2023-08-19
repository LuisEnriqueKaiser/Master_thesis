# some housekeeping
rm(list=ls())

# libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")



# triple diff in diff fin dependence
# loading data and preprocessing
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_prepared_for_fin_dependence_effects.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

tr = subset(data, data$treated==1)
sd(tr$lead1_r_d_intensity, na.rm = TRUE)
# to subset the data later, i save a copy
data_base <- data

# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did")

# Remove rows with missing values only in the subset columns
data <- data[complete.cases(data[, subset_columns]), ]


# financial dependence triple diff in diff 
triple_diff_in_diff_fin_sector = plm(lead1_r_d_intensity ~ did + post * factor(fin_dependent_sector)+ factor(fin_dependent_sector) * did +
                                       factor(gvkey) + factor(year),
                                     model = "within",
                                     index = c("gvkey", "year"),
                                     data = data)

#clustering
cluster_var <- data$subclass
vcov_cluster <- vcovHC(triple_diff_in_diff_fin_sector, cluster = "group", cluster.by = cluster_var)
triple_diff_in_diff_fin_sector$vcov <- vcov_cluster
print(summary(triple_diff_in_diff_fin_sector))



# interaction market to book


# data preprocessing and loading
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated
median(data$m_b_calculated, na.rm = TRUE)
#group creation
data$m_b_group = NaN
threshold_m_b_group = 1.5
for(i in 1:nrow(data)) {       # for-loop over rows
  firm_before = subset(data, data$gvkey == data$gvkey[i])
  data$m_b_group[i] = ifelse(mean(firm_before$m_b_calculated, na.rm = TRUE)>threshold_m_b_group, 1, 0)
}

# regression
did_interaction_mb = plm(lead1_r_d_intensity ~ did*factor(m_b_group) + post * factor(m_b_group)+
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         vcov = function(x) vcovHC(x, cluster = "subclass"),
                         data = data)
#clustering
cluster_var <- data$subclass
vcov_cluster <- vcovHC(did_interaction_mb, cluster = "group", cluster.by = cluster_var)
did_interaction_mb$vcov <- vcov_cluster
print(summary(did_interaction_mb))


# age
# data preprocessing
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
maturity_threshold = 8
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

# maturity status
data$maturity = NaN
threshold_matureness = 20

for(i in 1:nrow(data)) {       # for-loop over rows
  data$maturity[i] = switch(
    data$year[i] - 2006,
    ifelse(data$age[i]>=threshold_matureness-4,1,0),
    ifelse(data$age[i]>=threshold_matureness-3,1,0),
    ifelse(data$age[i]>=threshold_matureness-2,1,0),
    ifelse(data$age[i]>=threshold_matureness-1,1,0),
    ifelse(data$age[i]>=threshold_matureness,1,0),
    ifelse(data$age[i]>=threshold_matureness+1,1,0),
    ifelse(data$age[i]>=threshold_matureness+2,1,0),
    ifelse(data$age[i]>=threshold_matureness+3,1,0),
    ifelse(data$age[i]>=threshold_matureness+4,1,0),
    ifelse(data$age[i]>=threshold_matureness+5,1,0),
    ifelse(data$age[i]>=threshold_matureness+6,1,0),
    ifelse(data$age[i]>=threshold_matureness+7,1,0),
    ifelse(data$age[i]>=threshold_matureness+8,1,0))
}
# regression
did_interaction_age= plm(lead1_r_d_intensity ~ did*factor(maturity) + post * factor(maturity)+
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         vcov = function(x) vcovHC(x, cluster = "subclass"),
                         data = data)

# clustering
cluster_var <- data$subclass
vcov_cluster <- vcovHC(did_interaction_age, cluster = "group", cluster.by = cluster_var)
did_interaction_age$vcov <- vcov_cluster
print(summary(did_interaction_age))



# loan maturity

# data loading and preprocessing
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

# creating the deciding indicator variable
data$loan_maturity = ifelse(data$avg_maturity_pre_tr<=45, 1, 0)

# regression
did_int_loan_maturity = plm(lead1_r_d_intensity ~  factor(loan_maturity) * did + factor(loan_maturity)*post+
                              factor(year) + factor(gvkey),
                            model = "within",
                            index = c("gvkey", "year"),
                            vcov = function(x) vcovHC(x, cluster = "subclass"),
                            data = data)
#clustering
cluster_var <- data$subclass
vcov_cluster <- vcovHC(did_int_loan_maturity, cluster = "group", cluster.by = cluster_var)
did_int_loan_maturity$vcov <- vcov_cluster
print(summary(did_int_loan_maturity))


# build the latex table out of these 4 interactions


stargazer(
  triple_diff_in_diff_fin_sector, did_interaction_age ,did_int_loan_maturity, did_interaction_mb,
  font.size = "footnotesize",
  no.space = TRUE,
  dep.var.labels = c("R\\&D int_{t+1}"),
  covariate.labels = c("Treat x Post (TxP)", "TxP x Fin. Dep.","TxP x Age","TxP x Loan Mat.","TxP x MB"),
  omit.stat = c("f", "adj.rsq"),
  column.sep.width = "4pt",
  digits = 3,
  #align = TRUE,
  omit = c("year", "gvkey", "post"),
  header = FALSE,
  title = "Triple difference-in-differences results for heterogeneous effects regressions. \\newline
  Specifications are conducted in line with the baseline regression, adding the
  respective interactions to assess group differences. Non-informative interactions are omitted in the results.
  Standard errors are clustered across matched subclasses (firms).",
  label = "tab::triple_diff_in_diff",
  add.lines = list(
    c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
    c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
    c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))
  ),
  type = "latex",
  out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/triple_diff_sum.tex")
