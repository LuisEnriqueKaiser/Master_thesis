# leads & lags
# diff in diff design

# some housekeeping
rm(list=ls())
# libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")


# loading in the data and preprocessing
matched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
matched_data$post = ifelse(matched_data$year > 2011, 1, 0)
matched_data$did = matched_data$post * matched_data$treated


##################################################
#Testing for common pre trends with the matched data
##################################################

# creating the placebo post variables
matched_data$lag_1_post = ifelse(matched_data$year > 2010, 1, 0)
matched_data$did_lag_1 = matched_data$lag_1_post * matched_data$treated
matched_data$lag_2_post = ifelse(matched_data$year > 2009, 1, 0)
matched_data$did_lag_2 = matched_data$lag_2_post * matched_data$treated

matched_data$lead1_post = ifelse(matched_data$year > 2012, 1, 0)
matched_data$did_lead1 = matched_data$lead1_post * matched_data$treated
matched_data$lead2_post = ifelse(matched_data$year > 2013, 1, 0)
matched_data$did_lead2 = matched_data$lead2_post * matched_data$treated
matched_data$lead3_post = ifelse(matched_data$year > 2014, 1, 0)
matched_data$did_lead3 = matched_data$lead3_post * matched_data$treated

# regression
did_reg_pretrend_test = plm(lead1_r_d_intensity ~  + did + did_lag_1+did_lag_2+did_lead1+ did_lead2+did_lead3+
                              factor(year) + factor(gvkey),
                            model = "within",
                            index = c("gvkey", "year"),
                            data = matched_data)
# clustering
cluster_var <- matched_data$subclass
vcov_cluster <- vcovHC(did_reg_pretrend_test, cluster = "group", cluster.by = cluster_var)
did_reg_pretrend_test$vcov <- vcov_cluster
print(summary(did_reg_pretrend_test))


# creating the latex file 
stargazer(did_reg_pretrend_test,
          dep.var.labels="R\\&D int_{t+1}",
          covariate.labels = c("Treat x Post (TxP)", "Treat x Post_{t-1} (TxP)","Treat x Post_{t-2} (TxP)",
                               "Treat x Post_{t+1} (TxP)",
                               "Treat x Post_{t+2} (TxP)", "Treat x Post_{t+3} (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left")),
            c('Other covariates', str_pad("No", 12, side = "left"))
          ),
          no.space=FALSE,
          digits=3,
          omit = "year",
          title = "Dynamic difference-in-differences \\newline
          Dynamic difference-in-difference on the matched sample. Standard errors are clustered across subclasses (firms).",
          label = "fig:din_diff",
          header = TRUE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/dynamic_diff_in_diff.tex")
