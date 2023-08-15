#library("tidyverse")
rm(list = ls())

library("plm")
library("tidyverse")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

#####################################################################################
# research intensity in relation to sales
percentile_threshold <- 99

# Calculate the value corresponding to the percentile threshold
threshold_value <- quantile(data$lead1_r_d_intensity_sale, probs = percentile_threshold / 100, na.rm = TRUE)

# Filter the dataset to remove observations above the threshold
data <- data[data$lead1_r_d_intensity_sale <= threshold_value, ]



data = data.frame(data)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity_sale", "did")

# Remove rows with missing values only in the subset columns

subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset(subset_reg1,  select=subset_columns)
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]
did_reg_lead1_rd_sale = plm(lead1_r_d_intensity_sale ~   did +
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             data = subset_reg1)

print(summary(did_reg_lead1_rd_sale))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead1_rd_sale, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead1_rd_sale$vcov <- vcov_cluster
print(summary(did_reg_lead1_rd_sale))

tr = subset(subset_reg1, subset_reg1$treated == 1)
mean(tr$lead1_r_d_intensity_sale, na.rm= TRUE)




data = data.frame(data)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity_sale", "did")

# Remove rows with missing values only in the subset columns

subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset(subset_reg1,  select=subset_columns)
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]
did_reg_lead2_rd_sale = plm(lead1_r_d_intensity_sale ~   did +
                              factor(year) + factor(gvkey),
                            model = "within",
                            index = c("gvkey", "year"),
                            data = subset_reg2)

print(summary(did_reg_lead2_rd_sale))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead2_rd_sale, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead2_rd_sale$vcov <- vcov_cluster
print(summary(did_reg_lead2_rd_sale))




stargazer(did_reg_lead1_rd_sale, did_reg_lead2_rd_sale,
          dep.var.caption  = "Different outcome variables",
          label = "tab::rob_check_last",
          dep.var.labels=c("R\\&D int_{t+1}(Sale)","R\\&D int_{t+2}(Sale)"),
          covariate.labels = c("Treat x Post (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          no.space=TRUE,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"))
          ),
          digits=3,
          title = "Alternative outcome variables \\newline
          This table reports the results for alternative outcome variables on the same matched sample. The different
          outcome variable is based on the sales figures instead of total assets in the denominator. Standard errors are clustered
          across matched subclasses (firms).",
          align = FALSE,  omit=c("year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/rob_res_diff_outcome.tex")
