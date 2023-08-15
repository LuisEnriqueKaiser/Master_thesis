# This part here is the baseline regression with different threshold values, used as a robustness check

rm(list = ls())

#library("tidyverse")
library("plm")
library("tidyverse")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_7.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated_7

treated_data = subset(data, data$treated_7 == 1)
mean(treated_data$lead1_r_d_intensity, na.rm = TRUE)

# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did","year", "gvkey" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead_1_tr_7 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_tr_7))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_tr_7, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_1_tr_7$vcov <- vcov_cluster
print(summary(did_reg_lead_1_tr_7))


data_tr = subset(subset_reg1, subset_reg1$treated == 1)
mean(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 3.445531
sd(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 5.347

# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_7 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)

print(summary(did_reg_lead_2_tr_7))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2_tr_7, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_2_tr_7$vcov <- vcov_cluster
print(summary(did_reg_lead_2_tr_7))

data_tr = subset(subset_reg2, subset_reg2$treated == 1)
mean(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 3.505538
sd(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 5.65207
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################




library("tidyverse")
library("plm")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_11.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated_11



# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]



did_reg_lead_1_tr_11 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_tr_11))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_tr_11, cluster = "group", cluster.by = cluster_var)

# Perform coefficient test with clustered standard errors
did_reg_lead_1_tr_11$vcov <- vcov_cluster
print(summary(did_reg_lead_1_tr_11))


data_tr = subset(subset_reg1, subset_reg1$treated == 1)
mean(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 3.335
sd(data_tr$lead1_r_d_intensity, na.rm =TRUE)
# 5.347




# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_11 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)


print(summary(did_reg_lead_2_tr_11))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2_tr_11, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_2_tr_11$vcov <- vcov_cluster
print(summary(did_reg_lead_2_tr_11))


tr_tr_11 = subset(data, data$treated_11 == 1)
mean(tr_tr_11$lead2_r_d_intensity, na.rm = TRUE)
#3.881757
sd(tr_tr_11$lead2_r_d_intensity, na.rm = TRUE)
# 7.096537


stargazer(did_reg_lead_1_tr_7, did_reg_lead_2_tr_7, did_reg_lead_1_tr_11, did_reg_lead_2_tr_11,
          column.labels = c("7\\% threshold","7\\% threshold","11\\% threshold", "11\\% threshold"),
          label = "tab::res_robustness_threshold",
          dep.var.labels=c("R\\&D int_{t+1}", "R\\&D int_{t+2}", "R\\&D int_{t+1}", "R\\&D int_{t+2}"),
          covariate.labels = c("Treat x Post (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          no.space=TRUE,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"),str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))
          ),
          digits=3,
          title = "Results different threshold values \\newline This table reports results for alternative treatment thresholds.
          In the baseline regressions, banks with a tier 1 capital ratio below 8\\% at least once in the pre-treatment period are
          considered in the treatment group of banks and mapped onto firms. This threshold is reduced to 7\\%, for which the results are
          reported in the leftmost two columns, and increased to 11\\%, for which the results are reported in the two rightmost columns.
          For both thresholds, a new matching is performed with the same pre-treatment covariates. Standard errors are clustered across matched subclasses (firms)",
          align = FALSE,
          omit=c( "year","gvkey"),
          header = FALSE,
          type = "latex",
          out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/rob_diff_thresholds.tex")
