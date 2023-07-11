# leads & lags
# diff in diff design

# some housekeeping 
rm(list=ls())

library("tidyverse")
library("plm")
library("stargazer")

matched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post = ifelse(data$year > 2011, 1, 0)
data$did = data$post * data$treated


##################################################
#Testing for common pre trends with the matched data
##################################################

data$lag_1_post = ifelse(data$year > 2010, 1, 0)
data$did_lag_1 = data$lag_1_post * data$treated
data$lag_2_post = ifelse(data$year > 2009, 1, 0)
data$did_lag_2 = data$lag_2_post * data$treated


data$lead1_post = ifelse(data$year > 2012, 1, 0)
data$did_lead1 = data$lead1_post * data$treated
data$lead2_post = ifelse(data$year > 2013, 1, 0)
data$did_lead2 = data$lead2_post * data$treated


did_reg_pretrend_test = plm(lead1_r_d_intensity ~  + did + did_lag_1+did_lag_2+did_lead1+ did_lead2+
                              factor(year) + factor(gvkey),
                            model = "within",
                            vcov = function(x) vcovHC(x, cluster = "gind"),
                            index = c("gvkey", "year"),
                            data = data)
print(summary(did_reg_pretrend_test))




############################################################################################
############################################################################################
############################################################################################
############################################################################################

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")



data$post = ifelse(data$year > 2011, 1, 0)
data$did = data$post * data$treated


##################################################
#Testing for common pre trends with the matched data
##################################################

data$lag_1_post = ifelse(data$year > 2010, 1, 0)
data$did_lag_1 = data$lag_1_post * data$treated
data$lag_2_post = ifelse(data$year > 2009, 1, 0)
data$did_lag_2 = data$lag_2_post * data$treated


data$lead1_post = ifelse(data$year > 2012, 1, 0)
data$did_lead1 = data$lead1_post * data$treated
data$lead2_post = ifelse(data$year > 2013, 1, 0)
data$did_lead2 = data$lead2_post * data$treated


did_reg_pretrend_test = plm(lead1_r_d_intensity ~  + did + did_lag_1+did_lag_2+did_lead1+ did_lead2+
                              factor(year) + factor(gvkey),
                            model = "within",
                            vcov = function(x) vcovHC(x, cluster = "gind"),
                            index = c("gvkey", "year"),
                            data = data)
print(summary(did_reg_pretrend_test))
