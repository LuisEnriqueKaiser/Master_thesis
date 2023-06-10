# diff in diff design
#install.packages("tidyverse")
#install.packages("plm")
#install.packages("stargazer")


library("tidyverse")
library("plm")
library("stargazer")

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post = ifelse(data$year >= 2011, 1, 0)
data$did = data$post * data$treated

did_reg = plm(r_d_intensity ~ did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                  ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                model = "within",
                index = c("gvkey", "year"),
                vcov = function(x) vcovHC(x, cluster = "gind"),
                data = data)

did_reg_lead_1 = plm(lead1_r_d_intensity ~  did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                  ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                model = "within",
                index = c("gvkey", "year"),
                vcov = function(x) vcovHC(x, cluster = "gind"),
                data = data)
did_reg_lead_2 = plm(lead2_r_d_intensity ~did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                       ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = data)


did_reg_ch = plm(r_d_change_intensity ~  did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
              model = "within",
              index = c("gvkey", "year"),
              vcov = function(x) vcovHC(x, cluster = "gind"),
              data = data)


did_reg_lead_1_ch = plm(lead1_r_d_change_int ~  did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                  ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                model = "within",
                index = c("gvkey", "year"),
                vcov = function(x) vcovHC(x, cluster = "gind"),
                data = data)

did_reg_lead_2_ch = plm(lead2_r_d_change_int ~  did + ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                          ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                        model = "within",
                        index = c("gvkey", "year"),
                        vcov = function(x) vcovHC(x, cluster = "gind"),
                        data = data)




summary(did_reg_lead_2)





##################################################
          #Testing for common pre trends
##################################################

data$lag_1_post = ifelse(data$year >= 2010, 1, 0)
data$did_lag_1 = data$lag_1_post * data$treated
data$lag_2_post = ifelse(data$year >= 2009, 1, 0)
data$did_lag_2 = data$lag_2_post * data$treated
data$lag_3_post = ifelse(data$year >= 2008, 1, 0)
data$did_lag_3 = data$lag_3_post * data$treated


did_reg_pretrend_test = plm(lead1_r_d_intensity ~  + did + did_lag_1+did_lag_2+did_lag_3+  ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                  ppent_calculated + lev_calculated+ ch_calculated + factor(year) + factor(gvkey),
                model = "within",
                index = c("gvkey", "year"),
                vcov = function(x) vcovHC(x, cluster = "gind"),
                data = data)
summary(did_reg_pretrend_test)
