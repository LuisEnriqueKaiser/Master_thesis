#library("tidyverse")
rm(list = ls())

library("plm")
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

#####################################################################################
# research intensity in relation to sales


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



######################################################################################################
######################################################################################################

# rq research intensity

# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_rq", "did" )
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
did_reg_lead1_r_d_rq= plm(lead1_r_d_rq ~   did +
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             data = subset_reg1)


print(summary(did_reg_lead1_r_d_rq))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead1_r_d_rq, cluster = "group", cluster.by = cluster_var)
did_reg_lead1_r_d_rq$vcov <- vcov_cluster
print(summary(did_reg_lead1_r_d_rq))





# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_rq", "did" )

subset_reg1 <- data[complete.cases(data[, subset_columns]), ]

did_reg_lead2_r_d_rq= plm(lead2_r_d_rq ~   did +
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             data = data)

print(summary(did_reg_lead2_r_d_rq))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead2_r_d_rq, cluster = "group", cluster.by = cluster_var)
did_reg_lead2_r_d_rq$vcov <- vcov_cluster
print(summary(did_reg_lead2_r_d_rq))




stargazer(did_reg_lead1_rd_sale, did_reg_lead1_r_d_rq, did_reg_lead2_r_d_rq,
          dep.var.caption  = "Different outcome variables",
          dep.var.labels=c("R\\&D int_{t+1}(Sale)", "R\\&D_{t+1} (RQ)", "R\\&D_{t+2} (RQ)"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"))
          ),
          digits=3,
          title = "Alternative outcome variables \\newline 
          This table reports the results for alternative outcome variables on the same matched sample. The first column reports the results for 
          an alternative outcome variable, based on the sales figures instead of total assets in the denominator. 
          The two columns on the right side report the results for the research intensity, reported in the RQ dataset (see linking figure)", align = FALSE,  omit=c("year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/rob_res_diff_outcome.tex")









