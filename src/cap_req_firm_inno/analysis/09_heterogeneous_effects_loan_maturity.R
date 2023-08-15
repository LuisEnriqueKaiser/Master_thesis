# Loan Maturity dependence
rm(list = ls())
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

data$loan_maturity = ifelse(data$avg_maturity_pre_tr<=45, 1, 0)
lower_maturities = subset(data, data$avg_maturity_pre_tr<=45)
lower_maturities_tr = subset(lower_maturities, lower_maturities$treated == 1)
print(mean(lower_maturities_tr$lead1_r_d_intensity, na.rm = TRUE))
print(sd(lower_maturities_tr$lead1_r_d_intensity, na.rm = TRUE))


higher_maturities = subset(data, data$avg_maturity_pre_tr>44)
higher_maturities_tr = subset(higher_maturities, higher_maturities$treated == 1)
print(mean(higher_maturities_tr$lead1_r_d_intensity, na.rm = TRUE))
print(sd(higher_maturities_tr$lead1_r_d_intensity, na.rm = TRUE))


median(data$avg_maturity_pre_tr)


did_reg_lead_1_low = plm(lead1_r_d_intensity ~  did +
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                         data = lower_maturities)

cluster_var <- lower_maturities$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_low, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_low$vcov <- vcov_cluster
print(summary(did_reg_lead_1_low))


did_reg_lead_1_high = plm(lead1_r_d_intensity ~  did +
                            factor(year) + factor(gvkey),
                          model = "within",
                          index = c("gvkey", "year"),
                          vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                          data = higher_maturities)

cluster_var <- higher_maturities$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_high, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_high$vcov <- vcov_cluster
print(summary(did_reg_lead_1_high))


low_tr = subset(lower_maturities, lower_maturities$treated == 1)
print(mean(low_tr$lead1_r_d_intensity, na.rm = TRUE))

stargazer(did_reg_lead_1_low, did_reg_lead_1_high,
          dep.var.labels=c("R\\&D int_{t+1}"),
          label = "tab::results_loans",
          covariate.labels = c("Treat x Post (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.labels = c("Lower Maturities Loans", "Higher Maturities Loans"),
          column.sep.width = "4pt",
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))
            ),
          no.space=FALSE,
          digits=3,
          omit = c("year","gvkey") ,
          title = "Loan Maturities \\newline
          Regression results for the initial matched sample subsamples, based on the average loan maturity.
          Samples are split based on the average pre-treatment loan maturity for each firm. Firms that are on average above the median loan maturity in the
          pre-treatment period
          are placed in the high-maturity group and firms below the threshold are in the low-maturity group. Standard errors are clustered across
          matched subclasses (firms).",
          header = TRUE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturities_loans.tex")
