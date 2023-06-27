# leads & lags

# diff in diff design
#install.packages("tidyverse")
#install.packages("plm")
#install.packages("stargazer")


library("tidyverse")
library("plm")
library("stargazer")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post = ifelse(data$year > 2011, 1, 0)
data$did = data$post * data$treated

summary(data)
data <- data[complete.cases(data), ]
data = data[apply(data, 1, function(row) all(is.finite(row))),]

summary(data)


##################################################
#Testing for common pre trends
##################################################

data$lag_1_post = ifelse(data$year > 2010, 1, 0)
data$did_lag_1 = data$lag_1_post * data$treated
data$lag_2_post = ifelse(data$year > 2009, 1, 0)
data$did_lag_2 = data$lag_2_post * data$treated


data$lead1_post = ifelse(data$year > 2012, 1, 0)
data$did_lead1 = data$lead1_post * data$treated
data$lead2_post = ifelse(data$year > 2013, 1, 0)
data$did_lead2 = data$lead2_post * data$treated




used_data = data
used_data <- data[complete.cases(used_data), ]

did_reg_pretrend_test = plm(lead1_r_d_intensity ~  + did + did_lag_1+did_lag_2+did_lead1+ did_lead2+
                              cf_calculated + m_b_calculated +sales_growth_calculated+
                              ppent_calculated + lev_calculated+ ch_calculated+roa+
                              other_inv_sum_calculated + at+ capx+age+
                              factor(year) + factor(gind_first_4),
                            model = "within",
                            vcov = function(x) vcovHC(x, cluster = "gind"),
                            index = c("gvkey", "year"),
                            data = used_data)
print(summary(did_reg_pretrend_test))
