#library("tidyverse")
library("plm")
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

#####################################################################################

data = data.frame(data)
# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity_sale", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx",
                    "oth_inv_delta_calculated","ch_calculated", "at","age" )

# Remove rows with missing values only in the subset columns

subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead1_rd_sale = plm(lead1_r_d_intensity_sale ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               other_inv_sum_calculated + at+ capx+age+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = subset_reg1)

print(summary(did_reg_lead1_rd_sale))




######################################################################################################
######################################################################################################


# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_rq", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx",
                    "oth_inv_delta_calculated","ch_calculated", "at","age" )

# Remove rows with missing values only in the subset columns

subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead1_r_d_rq= plm(lead1_r_d_rq ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               other_inv_sum_calculated + at+ capx+age+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = data)


summary(did_reg_lead1_r_d_rq)





# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_rq", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx",
                    "oth_inv_delta_calculated","ch_calculated", "at","age" )

# Remove rows with missing values only in the subset columns

subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]

did_reg_lead2_r_d_rq= plm(lead2_r_d_rq ~   did +
               cf_calculated + m_b_calculated +sales_growth_calculated+
               ppent_calculated + lev_calculated+ ch_calculated+roa+age+
               other_inv_sum_calculated + at+ capx+age+
               factor(year) + factor(gvkey),
             model = "within",
             index = c("gvkey", "year"),
             vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
             data = data)



summary(did_reg_lead2_r_d_rq)





stargazer(did_reg_lead1_rd_sale, did_reg_lead1_r_d_rq, did_reg_lead2_r_d_rq,
          dep.var.caption  = "Different outcome variables",
          dep.var.labels=c("R and D int_{t+1}(Sale)", "R and D_{t+2} (RQ)", "R and D_{t+2} (RQ)"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          digits=3,
          title = "Regression Results", align = TRUE,  omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                               "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                               "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/robust_res_2.tex")



















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
subset_columns <- c("lead1_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx","age",
                    "oth_inv_delta_calculated","ch_calculated", "at" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]



did_reg_lead_1_tr_10 = plm(lead1_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_tr_10))


# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_10 = plm(lead2_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)

print(summary(did_reg_lead_2_tr_10))




##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################



#rm(list = ls())

library("tidyverse")
library("plm")
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_12.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated_12


# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx","age",
                    "oth_inv_delta_calculated","ch_calculated", "at" )


# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
subset_reg1 = subset_reg1[apply(subset_reg1, 1, function(row) all(is.finite(row))),]



did_reg_lead_1_tr_12 = plm(lead1_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg1)

print(summary(did_reg_lead_1_tr_12))


# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "did", "ln_sales_calculated", "cf_calculated", "m_b_calculated","roa","capx",
                    "sales_growth_calculated","ppent_calculated",  "capx", "lev_calculated", "ch_calculated","at" )


# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_reg_lead_2_tr_12 = plm(lead2_r_d_intensity ~  did +
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = subset_reg2)

print(summary(did_reg_lead_2_tr_12))


stargazer(did_reg_lead_1_tr_10, did_reg_lead_2_tr_10, did_reg_lead_1_tr_12, did_reg_lead_2_tr_12,
          dep.var.caption  = "Tier 1 Capital Ratio: 10 per cent and 12 per cent to risk weighted assets",
          dep.var.labels=c("R and D intensity_{t+1}", "R and D intensity_{t+2}", "R and D intensity_{t+1}", "R and D intensity_{t+2}"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          digits=3,
          title = "Regression Results", align = TRUE,  omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                               "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                               "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/robust_res_1.tex")
