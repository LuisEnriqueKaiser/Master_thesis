#  heterogenous effects for young and mature firms and for high m/b values and low m/b values
# some housekeeping
rm(list=ls())


data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
maturity_threshold = 8
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

# maturity status
data$maturity = NaN
threshold_matureness = 20
for(i in 1:nrow(data)) {       # for-loop over rows
  data$maturity[i] = switch(
    data$year[i] - 2006,
    ifelse(data$age[i]>=threshold_matureness-4,1,0),
    ifelse(data$age[i]>=threshold_matureness-3,1,0),
    ifelse(data$age[i]>=threshold_matureness-2,1,0),
    ifelse(data$age[i]>=threshold_matureness-1,1,0),
    ifelse(data$age[i]>=threshold_matureness,1,0),
    ifelse(data$age[i]>=threshold_matureness+1,1,0),
    ifelse(data$age[i]>=threshold_matureness+2,1,0),
    ifelse(data$age[i]>=threshold_matureness+3,1,0),
    ifelse(data$age[i]>=threshold_matureness+4,1,0),
    ifelse(data$age[i]>=threshold_matureness+5,1,0),
    ifelse(data$age[i]>=threshold_matureness+6,1,0),
    ifelse(data$age[i]>=threshold_matureness+7,1,0),
    ifelse(data$age[i]>=threshold_matureness+8,1,0))
    }


mature_firms = subset(data, data$maturity == 1)
immature_firms = subset(data, data$maturity == 0)




mature_firms_did = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = mature_firms)


print(summary(mature_firms_did))
cluster_var <- mature_firms$subclass
vcov_cluster <- vcovHC(mature_firms_did, cluster = "group", cluster.by = cluster_var)
mature_firms_did$vcov <- vcov_cluster
print(summary(mature_firms_did))




immature_firms_did = plm(lead1_r_d_intensity ~  did +
                          factor(year) + factor(gvkey),
                        model = "within",
                        index = c("gvkey", "year"),
                        data = immature_firms)

print(summary(immature_firms_did))
cluster_var <- immature_firms$subclass
vcov_cluster <- vcovHC(immature_firms_did, cluster = "group", cluster.by = cluster_var)
immature_firms_did$vcov <- vcov_cluster
print(summary(immature_firms_did))

immature_firms_treat = subset(immature_firms, immature_firms$treated == 1)

mature_firms_treat = subset(mature_firms, mature_firms$treated == 1)

mean(mature_firms$lead1_r_d_intensity, na.rm = TRUE)
mean(immature_firms$lead1_r_d_intensity, na.rm = TRUE)

mean(mature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)
mean(immature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)



stargazer(mature_firms_did, immature_firms_did,
          dep.var.labels=c("R\\&D int_{t+1}"),
          covariate.labels = c("Treat x Post"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          no.space=TRUE,
          column.labels = c("Older Firms", "Younger Firms"),
          digits=3,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"))), 
          title = "Firm Age: \\newline 
          
          
          ", align = FALSE, omit=c( "year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturity_results.tex")





##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


#rm(list=ls())
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated
median(data$m_b_calculated, na.rm = TRUE)
# for the future, i want to include maybe the kaplan zingales score as a measurement of constrained firms and innovative firms
data$m_b_group = NaN
threshold_m_b_group = 1.5
for(i in 1:nrow(data)) {       # for-loop over rows
  firm_before = subset(data, data$gvkey == data$gvkey[i])
  data$m_b_group[i] = ifelse(mean(firm_before$m_b_calculated, na.rm = TRUE)>threshold_m_b_group, 1, 0)
}


high_market_to_book = subset(data, data$m_b_group == 1)
low_market_to_book = subset(data, data$m_b_group == 0)

high_market_to_book_did = plm(lead1_r_d_intensity ~  did +
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         data = high_market_to_book)

print(summary(high_market_to_book_did))
cluster_var <- high_market_to_book$subclass
vcov_cluster <- vcovHC(high_market_to_book_did, cluster = "group", cluster.by = cluster_var)
high_market_to_book_did$vcov <- vcov_cluster
print(summary(high_market_to_book_did))

high_market_to_book_tr = subset(high_market_to_book, high_market_to_book$treated == 1)
mean(high_market_to_book_tr$lead1_r_d_intensity, na.rm=TRUE)


low_market_to_book_did = plm(lead1_r_d_intensity ~  did +
                                factor(year) + factor(gvkey),
                              model = "within",
                              index = c("gvkey", "year"),
                              data = low_market_to_book)

print(summary(low_market_to_book_did))
cluster_var <- high_market_to_book$subclass
vcov_cluster <- vcovHC(low_market_to_book_did, cluster = "group", cluster.by = cluster_var)
low_market_to_book_did$vcov <- vcov_cluster
print(summary(low_market_to_book_did))




stargazer(mature_firms_did, immature_firms_did,high_market_to_book_did, low_market_to_book_did,
          dep.var.labels=c("R\\&D int_{t+1}"),
          covariate.labels = c("Treat x Post"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          no.space=TRUE,
          column.labels = c("Older Firms", "Younger Firms", "High M/B", "Low M/B"),
          digits=3,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"),str_pad("\\checkmark", 12, side = "left"),str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"),str_pad("\\checkmark", 12, side = "left"))), 
          title = "Firm Age: \\newline This table reports results for subsample of the matched sample, based on firm age 
          and market-to-book ratio. For firm age the first occurence in the Compustat dataset is set as the birthyear and age is computed accordingly. Firms which are
          Firms, which are in the compustat dataset before 2006 are considered in the older group, while firms, which have their 
          first occurence after 2006 are considered in the younger firms category. The results are reported in the first two columns. 
          For the market-to-book ratio categorization, firms with a higher ", align = FALSE, omit=c( "year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturity_results.tex")





# Loan Maturity dependence 


rm(list = ls())
library("plm")
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

lower_maturities = subset(data, data$avg_maturity_pre_tr<=44)
higher_maturities = subset(data, data$avg_maturity_pre_tr>44) 
median(data$avg_maturity_pre_tr)


did_reg_lead_1_low = plm(lead1_r_d_intensity ~  did +
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                         data = lower_maturities)

cluster_var <- lower_maturities$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_low, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_low$vcov <- vcov_cluster
print(summary(did_reg_lead_1_low))


did_reg_lead_1_high = plm(lead1_r_d_intensity ~  did +
                            factor(year) + factor(gvkey),
                          model = "within",
                          index = c("gvkey", "year"),
                          vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                          data = higher_maturities)

cluster_var <- higher_maturities$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_high, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_high$vcov <- vcov_cluster
print(summary(did_reg_lead_1_high))




stargazer(did_reg_lead_1_low, did_reg_lead_1_high,
          dep.var.labels=c("R\\&D int_{t+1}"),
          covariate.labels = c("DiD"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.labels = c("Lower Maturities Loans", "Higher Maturities Loans"),
          column.sep.width = "4pt",
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"))
          ),
          no.space=FALSE,
          digits=3,
          omit = c("year","gvkey") ,
          title = "Loan Maturities \\newline Regression results for subsamples of the initial matched sample, based on the average loan maturity.
          Samples are split based on the average pretreatment loan maturity for each firm. Firms, which are above the median pretreatment loan maturity
          are in the high maturity group, and firms which are below the threshold are in the low maturity group.
          All firms, with a mean makret-to-book ratio above 1.5 in the pre-treatment period are considered in the high market-to-book category, while
          firms with a lower mean market-to-book ratio are considered in the lower market-to-book category. Results are reported on the two right columns.",
          header = TRUE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturities_loans.tex")



