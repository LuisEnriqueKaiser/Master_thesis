# some housekeeping
rm(list=ls())
# read in the libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")


tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_prepared_for_fin_dependence_effects.csv")

data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

tr = subset(data, data$treated==1)
mean(tr$lead1_r_d_intensity)
sd(tr$lead1_r_d_intensity, na.rm = TRUE)
# to subset the data later, i save a copy
data_base <- data

# First for the financial dependent industries
# only for financial dependent industries
fin_dep_data = subset(data, data$fin_dependent_sector == 1)
fin_dep_data_tr = subset(fin_dep_data, fin_dep_data$treated == 1)
mean(fin_dep_data_tr$lead1_r_d_intensity, na.rm = TRUE)
sd(fin_dep_data_tr$lead1_r_d_intensity, na.rm = TRUE)


# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did", "gvkey", "year")

# Remove rows with missing values only in the subset columns
fin_dep_data <- fin_dep_data[complete.cases(fin_dep_data[, subset_columns]), ]

did_reg_lead_1_fin_dependent = plm(lead1_r_d_intensity ~  did +factor(gvkey) + factor(year),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = fin_dep_data)


print(summary(did_reg_lead_1_fin_dependent))
cluster_var <- fin_dep_data$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_fin_dependent, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_1_fin_dependent$vcov <- vcov_cluster
print(summary(did_reg_lead_1_fin_dependent))



# now the non financially dependent sectors
non_fin_dep = subset(data_base, data_base$fin_dependent_sector == 0)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did", "gvkey", "year")
# Remove rows with missing values only in the subset columns
non_fin_dep <- non_fin_dep[complete.cases(non_fin_dep[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead_1_non_fin_sector = plm(lead1_r_d_intensity ~  did +factor(gvkey) + factor(year),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = non_fin_dep)

print(summary(did_reg_lead_1_non_fin_sector))
cluster_var <- non_fin_dep$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_non_fin_sector, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_reg_lead_1_non_fin_sector$vcov <- vcov_cluster
print(summary(did_reg_lead_1_non_fin_sector))

# means
treated_fin_dep = subset(fin_dep_data, fin_dep_data$treated == 1)
print(mean(treated_fin_dep$lead1_r_d_intensity, na.rm = TRUE))
print(sd(treated_fin_dep$lead1_r_d_intensity, na.rm = TRUE))

treated_non_fin_dep = subset(non_fin_dep, non_fin_dep$treated == 1)
print(mean(treated_non_fin_dep$lead1_r_d_intensity, na.rm = TRUE))
print(sd(treated_non_fin_dep$lead1_r_d_intensity, na.rm = TRUE))

# Building the latex table
stargazer(did_reg_lead_1_fin_dependent, did_reg_lead_1_non_fin_sector,
          label = "tab::fin_dep_res",
          dep.var.labels=c("R\\&D int_{t+1}"),
          covariate.labels = c("Post x Treat"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          column.labels = c("High financial dependence", "Low financial dependence"),
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"),  str_pad("\\checkmark", 12, side = "left")),
            c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))),
          digits=3,
          title = "Industry level external finance dependence: \\newline Regression results for subsamples of the initial sample.
          Industry-level financial dependence is computed by using net change in capital. The 70\\% percentile of net change
          in capital of firms is used as the threshold value. All industries, defined by the first two digits of the General Industry Identifier (GIND), above the
          threshold in their mean values before the treatment period, are considered highly external finance dependent. All sectors, with mean net changes in capital
          below the threshold in the pre-treatment period, are considered low external finance dependent. Since firms are distributed asymmetrically
          across industries, I could not ensure balanced datasets when considering a sector-based approach. The 70 \\% percentile ensured the most balanced dataset.
          Standard errors are clustered across matched subclasses (firms).",
          omit=c( "year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/fin_dependence_results_sector.tex")
