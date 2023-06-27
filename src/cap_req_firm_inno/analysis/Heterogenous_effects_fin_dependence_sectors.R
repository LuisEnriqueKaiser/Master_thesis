library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")

tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_prepared_for_h_effects.csv")


data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated
# to subset the data later, i save a copy
data_base <- data

# First for the financial dependent industries
# only for financial dependent industries
data = subset(data, data$fin_dependent_sector == 1)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx","age",
                    "oth_inv_delta_calculated","ch_calculated", "at" )

# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]



did_reg_lead_1_fin_dependent = plm(lead1_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_fin_dependent))



# now the non financially dependent sectors
data = subset(data_base, data_base$fin_dependent_sector ==0)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx","age",
                    "oth_inv_delta_calculated","ch_calculated", "at" )
# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead_1_non_fin_sector = plm(lead1_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_non_fin_sector))






# Building the latex table


stargazer(did_reg_lead_1_fin_dependent, did_reg_lead_1_non_fin_sector,
          dep.var.labels=c("R and D intensity_{t+1}"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          column.labels = c("Financial dependent sectors", "Financial non-dependent sectors"),
          digits=3,
          title = "Regression Results", align = TRUE, omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                                "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                                "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/fin_dependence_results_sector.tex")
