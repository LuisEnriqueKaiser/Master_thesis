###############################################################################################
###############################################################################################
            # This part here is with different threshold values
###############################################################################################
###############################################################################################

rm(list = ls())

#library("tidyverse")
library("plm")
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_10.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated_10


# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did","year", "gvkey" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]
summary(subset_reg1)


did_reg_lead_1_tr_10 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = data)

print(summary(did_reg_lead_1_tr_10))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_tr_10, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_1_tr_10$vcov <- vcov_cluster
print(summary(did_reg_lead_1_tr_10))



# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_10 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)

print(summary(did_reg_lead_2_tr_10))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2_tr_10, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_2_tr_10$vcov <- vcov_cluster
print(summary(did_reg_lead_2_tr_10))



##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################




library("tidyverse")
library("plm")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_12.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated_12


# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]



did_reg_lead_1_tr_12 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_tr_12))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_tr_12, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_1_tr_12$vcov <- vcov_cluster
print(summary(did_reg_lead_1_tr_12))


# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_12 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)


print(summary(did_reg_lead_2_tr_12))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2_tr_12, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_2_tr_12$vcov <- vcov_cluster
print(summary(did_reg_lead_2_tr_12))




stargazer(did_reg_lead_1_tr_10, did_reg_lead_2_tr_10, did_reg_lead_1_tr_12, did_reg_lead_2_tr_12,
          column.labels = c("10\\% threshold","10\\% threshold","12\\% threshold", "12\\% threshold"),
          dep.var.labels=c("R\\&D int_{t+1}", "R\\&D int_{t+2}", "R\\&D int_{t+1}", "R\\&D int_{t+2}"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Sector trends', str_pad("\\xmark", 12, side = "left"), str_pad("\\xmark", 12, side = "left"), str_pad("\\xmark", 12, side = "left"), str_pad("\\xmark", 12, side = "left"))
          ),
          digits=3,
          title = "Results different threshold values\\newline This table reports results for alternative treatment thresholds. 
          In the baseline regressions, bank with a-tier 1 capital ratio below 8 \\% for at least one time in the pretreatment period are
          considered in the treatment group of banks and mapped onto firms. This threshold is increased to 10 \\%, for which the results are 
          reported in the first two columns and to 12\\%, for which the results are reported in the two left columns. For both thresholds
          a new matching is performed with the same pretreatment covariates.", align = TRUE,  omit=c( "year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/rob_diff_thresholds.tex")


