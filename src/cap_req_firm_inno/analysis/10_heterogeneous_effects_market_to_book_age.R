#  heterogenous effects for young and mature firms and for high m/b values and low m/b values
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

# some housekeeping

rm(list=ls())
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
tr_year = 2011
maturity_threshold = 8
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated


# maturity status of firms
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

# create the two groups
mature_firms = subset(data, data$maturity == 1)
immature_firms = subset(data, data$maturity == 0)


# regression for mature firms
mature_firms_did = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = mature_firms)


print(summary(mature_firms_did))
# clustering
cluster_var <- mature_firms$subclass
vcov_cluster <- vcovHC(mature_firms_did, cluster = "group", cluster.by = cluster_var)
mature_firms_did$vcov <- vcov_cluster
print(summary(mature_firms_did))



# regression for immature firms
immature_firms_did = plm(lead1_r_d_intensity ~  did +
                          factor(year) + factor(gvkey),
                        model = "within",
                        index = c("gvkey", "year"),
                        data = immature_firms)

print(summary(immature_firms_did))
#clustering
cluster_var <- immature_firms$subclass
vcov_cluster <- vcovHC(immature_firms_did, cluster = "group", cluster.by = cluster_var)
immature_firms_did$vcov <- vcov_cluster
print(summary(immature_firms_did))

# descriptive stats for subsets
immature_firms_treat = subset(immature_firms, immature_firms$treated == 1)
mature_firms_treat = subset(mature_firms, mature_firms$treated == 1)

mean(mature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)
sd(mature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)

mean(immature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)
sd(immature_firms_treat$lead1_r_d_intensity, na.rm = TRUE)

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


#rm(list=ls())
# groupwise regression for the two distinct m/b groups
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

# create the two groups
median(data$m_b_calculated, na.rm = TRUE)
data$m_b_group = NaN
# set the threshold
threshold_m_b_group = 1.5

for(i in 1:nrow(data)) {       # for-loop over rows
  firm_before = subset(data, data$gvkey == data$gvkey[i])
  data$m_b_group[i] = ifelse(mean(firm_before$m_b_calculated, na.rm = TRUE)>threshold_m_b_group, 1, 0)
}
# group splitting
high_market_to_book = subset(data, data$m_b_group == 1)
high_market_to_book_tr = subset(high_market_to_book, high_market_to_book$treated== 1)
# descriptives
print(mean(high_market_to_book_tr$lead1_r_d_intensity, na.rm = TRUE))
print(sd(high_market_to_book_tr$lead1_r_d_intensity, na.rm = TRUE))

low_market_to_book = subset(data, data$m_b_group == 0)
low_market_to_book_tr = subset(low_market_to_book, low_market_to_book$treated== 1)
# descriptives
print(mean(low_market_to_book_tr$lead1_r_d_intensity, na.rm = TRUE))
print(sd(low_market_to_book_tr$lead1_r_d_intensity, na.rm = TRUE))


# regression for high mb group
high_market_to_book_did = plm(lead1_r_d_intensity ~  did +
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         data = high_market_to_book)

print(summary(high_market_to_book_did))
# clustering
cluster_var <- high_market_to_book$subclass
vcov_cluster <- vcovHC(high_market_to_book_did, cluster = "group", cluster.by = cluster_var)
high_market_to_book_did$vcov <- vcov_cluster
print(summary(high_market_to_book_did))
high_market_to_book_tr = subset(high_market_to_book, high_market_to_book$treated == 1)
mean(high_market_to_book_tr$lead1_r_d_intensity, na.rm=TRUE)

# regression for low m/b group
low_market_to_book_did = plm(lead1_r_d_intensity ~  did +
                                factor(year) + factor(gvkey),
                              model = "within",
                              index = c("gvkey", "year"),
                              data = low_market_to_book)

print(summary(low_market_to_book_did))
# clustering
cluster_var <- high_market_to_book$subclass
vcov_cluster <- vcovHC(low_market_to_book_did, cluster = "group", cluster.by = cluster_var)
low_market_to_book_did$vcov <- vcov_cluster
print(summary(low_market_to_book_did))



# make the latex file
stargazer(mature_firms_did, immature_firms_did,high_market_to_book_did, low_market_to_book_did,
          dep.var.labels=c("R\\&D int_{t+1}"),
          label = "tab::results_m_b",
          covariate.labels = c("Treat x Post (TxP)"),
          omit.stat = c("f","adj.rsq"),
          font.size = "footnotesize",
          column.sep.width = "4pt",
          no.space=TRUE,
          column.labels = c("Older Firms", "Younger Firms", "High M/B", "Low M/B"),
          digits=3,
          add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"),str_pad("\\checkmark", 12, side = "left"),str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"),str_pad("\\checkmark", 12, side = "left")),
            c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))),

          title = "Firm age \\& market-to-book ratios:
          \\newline This table presents results for two subsets of the matched sample based on firm age and market-to-book ratio.
          To determine the firm age, I use the first occurrence in the Compustat dataset as the birth year and calculate the age accordingly.
          Firms already present in the Compustat dataset before 2006 are categorized as older firms,
          while firms with their first occurrence after 2006 are classified as younger. These results are displayed in the first two columns.
          Regarding the market-to-book ratio categorization, firms with a mean market-to-book ratio above 1.5 in the pre-treatment period are placed
          in the high market-to-book ratio group, while firms below the threshold are assigned to the low market-to-book ratio group. A market-
          to-book ratio of 1.5 is the median for the matched sample.
          Standard errors are clustered across matched subclasses (firms).", align = FALSE, omit=c( "year","gvkey"),
          header = FALSE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/maturity_results.tex")

