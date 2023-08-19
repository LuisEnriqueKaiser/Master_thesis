# some housekeeping
rm(list = ls())
#libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

# loading in the data
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

############################################################################################################################
############################################################################################################################

# first step: Create the datasets, with the treatment lender shares and their respective matches

## create the sub 50 % subset
data_sub_50_set <- data[data$treatment_lender_share_50 != 1 | data$treated == 0, ]
data_sub_50_set <- data_sub_50_set %>%
  group_by(subclass) %>%
  filter(n_distinct(gvkey) > 1)
data_sub_50_set$did = data_sub_50_set$post *data_sub_50_set$treated

# descriptives
mean_subset_1 = subset(data_sub_50_set,data_sub_50_set$treated == 1)
mean(mean_subset_1$lead1_r_d_intensity, na.rm  = TRUE)

## create the 50 % subset
data_50_set <- data[data$treatment_lender_share_50 == 1 | data$treated == 0, ]
data_50_set <- data_50_set %>%
  group_by(subclass) %>%
  filter(n_distinct(gvkey) > 1)
data_50_set$did = data_50_set$post *data_50_set$treatment_lender_share_50
# descriptives
mean_subset_2 = subset(data_50_set,data_50_set$treated == 1)
mean(mean_subset_2$lead1_r_d_intensity, na.rm  = TRUE)


## create the 100 % subset
data_100_set <- data[data$treatment_lender_share_1 == 1 | data$treated == 0, ]
data_100_set <- data_100_set %>%
  group_by(subclass) %>%
  filter(n_distinct(gvkey) > 1)
data_100_set$did = data_100_set$post * data_100_set$treatment_lender_share_1
# descriptives 
mean_subset_3 = subset(data_100_set,data_100_set$treated == 1)
mean(mean_subset_3$lead1_r_d_intensity, na.rm  = TRUE)




#####################################################################################

#  sub 50 %
# regression 
did_reg_lead_1_sub_50 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = data_sub_50_set)


#clustering
cluster_var <- data_sub_50_set$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_sub_50, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_sub_50$vcov <- vcov_cluster
print(summary(did_reg_lead_1_sub_50))


# 50
# regression 
did_reg_lead_1_50 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = data_50_set)

# clustering
cluster_var <- data_50_set$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_50, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_50$vcov <- vcov_cluster
print(summary(did_reg_lead_1_50))




# 100
# regression 
did_reg_lead_1 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = data_100_set)
# clustering
cluster_var <- data_100_set$subclass
vcov_cluster <- vcovHC(did_reg_lead_1, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1$vcov <- vcov_cluster
print(summary(did_reg_lead_1))

# create the latex table 
stargazer(did_reg_lead_1_sub_50, did_reg_lead_1_50,did_reg_lead_1,
          label = "tab::rob_check_binning",
          dep.var.labels=c("R\\&D int_{t+1}"),
          covariate.labels = c("Treat x Post (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.labels = c("Sub 50\\% group", "Above 50\\% group", "100\\% group"),
          column.sep.width = "4pt",
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))
          ),
          no.space=FALSE,
          digits=3,
          omit = c("year","gvkey") ,
          title = "Different relative shares of treated lenders \\newline Regression results for subsets of the initial sample, based on the
          the relative share of treated banks of all lenders for each firm in the pre-treatment period.
          Treated firms and their respective match are split into three groups:
          The sub 50\\% group lends from more non-treated banks than treated banks in the pre-treatment period. The above 50\\%
          group lends from more treated banks than from non-treated banks. The 100\\% group only lends from treated banks in the pre-treatment
          period. Matched observations from the control group are classified according to this treatment match.
          Standard errors are clustered across matched subclasses (firms).",
          header = TRUE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/rob_bins.tex")
