#  heterogenous effects for young and mature firms

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
                       cf_calculated + m_b_calculated +sales_growth_calculated+
                       ppent_calculated + lev_calculated+ ch_calculated+roa+
                       other_inv_sum_calculated + at+ capx+age+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                     data = mature_firms)


print(summary(mature_firms_did))


immature_firms_did = plm(lead1_r_d_intensity ~  did +
                          cf_calculated + m_b_calculated +sales_growth_calculated+
                          ppent_calculated + lev_calculated+ ch_calculated+roa+
                          other_inv_sum_calculated + at+ capx+age+
                          factor(year) + factor(gvkey),
                        model = "within",
                        index = c("gvkey", "year"),
                        vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                        data = immature_firms)

print(summary(immature_firms_did))





stargazer(mature_firms_did, immature_firms_did,
          dep.var.labels=c("R and D intensity_{t+1}"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          column.labels = c("Mature Firms", "Immature Firms"),
          digits=3,
          title = "Regression Results", align = TRUE, omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                              "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                              "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturity_results.tex")





##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################



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
                           cf_calculated + m_b_calculated +sales_growth_calculated+
                           ppent_calculated + lev_calculated+ ch_calculated+roa+
                           other_inv_sum_calculated + at+ capx+age+
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                         data = high_market_to_book)

print(summary(high_market_to_book_did))


low_market_to_book = plm(lead1_r_d_intensity ~  did +
                                cf_calculated + m_b_calculated +sales_growth_calculated+
                                ppent_calculated + lev_calculated+ ch_calculated+roa+
                                other_inv_sum_calculated + at+ capx+age+
                                factor(year) + factor(gvkey),
                              model = "within",
                              index = c("gvkey", "year"),
                              vcov = function(x) vcovHC(x, cluster = "gind_first_4"),
                              data = low_market_to_book)

print(summary(low_market_to_book))






stargazer(high_market_to_book_did, low_market_to_book,mature_firms_did, immature_firms_did,
          dep.var.labels=c("R and D intensity_{t+1}"),
          covariate.labels = c("Diff in Diff"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "1pt",
          no.space=TRUE,
          column.labels = c("High Market to Book ratio", "Low Market to Book ratio", "Mature Firms", "Immature Firms"),
          digits=3,
          title = "Regression Results", align = TRUE, omit=c( "year","gvkey", "cf_calculated", "m_b_calculated", "sales_growth_calculated",
                                                              "ppent_calculated", "lev_calculated", "ch_calculated", "roa",
                                                              "other_inv_sum_calculated", "at","capx","age"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/sec_het_effects.tex")
