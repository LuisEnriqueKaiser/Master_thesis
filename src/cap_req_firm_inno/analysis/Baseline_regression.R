# diff in diff design
#install.packages("tidyverse")
#install.packages("plm")
#install.packages("stargazer")

rm(list = ls())
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data = subset(data, data$year <= 2015)
data = subset(data, data$year >= 2008)
#data$transition_period <- ifelse(data$year >= 2011 & data$year <= 2013, 1, 0)


#data$did = data$transition_period * data$treated
data$did = data$post * data$treated
data$years_since = ifelse(data$year > 2011, data$year - tr_year, 0)
data$lin_est = data$years_since * data$treated

#summary(data)
#data <- data[complete.cases(data), ]
#data = data[apply(data, 1, function(row) all(is.finite(row))),]




##################################################
  # Baseline regression without banking variables
##################################################





# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, "lead1_r_d_intensity"]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]


did_reg_lead_1 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1))



# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = data)

print(summary(did_reg_lead_2))

cov_names=c("Treat x Post","Cash Flow", "Market to Book Ratio","Sales growth",
                   "Property, Plant and
                   Equipment", "Leverage", "Cash", "Return on Assets", "Oth. Investments",
                   "Total Assets", "Capital Expenditures", "Age")


stargazer(did_reg_lead_1, did_reg_lead_2, no.space=TRUE,
          dep.var.labels=c("R and D intensity_{t+1}", "R and intensity_{t+2}"),
          covariate.labels = cov_names,
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          digits=3,
          title = "Regression Results", align = TRUE, omit="year",
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/baseline_regression_lead2.tex")






##################################################
# Baseline regression with a difference counter
##################################################



# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "lin_est", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                                                                         "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at")


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]


did_lin_var_lead_1 = plm(lead1_r_d_intensity ~  lin_est +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)


print(summary(did_lin_var_lead_1))


# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "lin_est", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_lin_var_lead_2 = plm(lead2_r_d_intensity ~  lin_est +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)

print(summary(did_lin_var_lead_2))



cov_names_lin=c("Treat x Years since","Cash Flow", "Market to Book Ratio","Sales growth",
            "Property, Plant and Equipment", "Leverage", "Cash", "Return on Assets", "Oth. Investments",
            "Total Assets", "Capital Expenditures", "Age")



stargazer(did_lin_var_lead_1, did_lin_var_lead_2,
          dep.var.labels=c("R and D intensity_{t+1}", "R and D intensity_{t+2}"),
          covariate.labels = c("Treat x Years since"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          digits=3,
          title = "Regression Results", align = TRUE,  omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                               "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                               "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/baseline_regression_linear.tex")





####################################################################################################
# Baseline regression without matching but with covariates
####################################################################################################



library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")

tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")
#data$post = ifelse(data$year > 2011, 1, 0)

data$post <- ifelse(data$year > tr_year, 1, 0)
#data$transition_period <- ifelse(data$year >= 2011 & data$year <= 2013, 1, 0)


#data$did = data$transition_period * data$treated
data$did = data$post * data$treated





# Specify the subset of columns for complete cases check
columns =         c("r_d_intensity", "lead1_r_d_intensity", "lead2_r_d_intensity", "did", "lead1_r_d_intensity", "lead2_r_d_intensity", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated","net_change_capital", "lev_calculated","roa","capx","oth_inv_delta_calculated",
                    "other_inv_sum_calculated","ch_calculated", "at","age", "year","gvkey")

data = subset(data, select=columns)
# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]
summary(subset_reg1)



did_reg= plm(r_d_intensity ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               oth_inv_delta_calculated + capx+age+net_change_capital+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = subset_reg1)

print(summary(did_reg))

did_reg_lead1= plm(lead1_r_d_intensity ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               other_inv_sum_calculated + capx+age+net_change_capital+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = subset_reg1)

print(summary(did_reg_lead1))

did_reg_lead2= plm(lead2_r_d_intensity ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               other_inv_sum_calculated + capx+age+net_change_capital+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = subset_reg1)

print(summary(did_reg_lead2))
