library("dplyr")    # For data manipulation
library("plm")
# data prep

# Create a dataframe with variables for treatment, covariates, and outcome
raw_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv")

data = raw_data[c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx",
                  "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind","ebitda","ni","firm_born",
                  "Assets_total_lender","net_income_banks","loan_banks_total","banks_allowances_total",
                  "dt", "cshi", "dcpstk")]#, "seq", "ch","ppent", "ivstch"]]


# first checking
columns_to_check <- c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx","ebitda","ni",
                        "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind", "dt", "cshi", "dcpstk")

columns_to_impute <-c("Assets_total_lender","net_income_banks","loan_banks_total","banks_allowances_total")
# Drop rows with missing or infinite values


data <- data[complete.cases(data[, columns_to_check]) & apply(data[, columns_to_check], 1, function(x) all(is.finite(x))), ]


# at banks
data$Assets_total_lender <- ifelse(is.na(data$Assets_total_lender), ave(data$Assets_total_lender, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$Assets_total_lender)
data$Assets_total_lender <- ifelse(is.nan(data$Assets_total_lender), ave(data$Assets_total_lender, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$Assets_total_lender)
data$Assets_total_lender <- ifelse(is.infinite(data$Assets_total_lender), ave(data$Assets_total_lender, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$Assets_total_lender)




# loans banks

data$loan_banks_total <- ifelse(is.na(data$loan_banks_total), ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$loan_banks_total)
data$loan_banks_total <- ifelse(is.nan(data$loan_banks_total), ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$loan_banks_total)
data$loan_banks_total <- ifelse(is.infinite(data$loan_banks_total), ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$loan_banks_total)


data$loan_banks_total[is.na(data$loan_banks_total)] <- ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE))
data$loan_banks_total[is.nan(data$loan_banks_total)] <- ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE))
data$loan_banks_total[is.infinite(data$loan_banks_total)] <- ave(data$loan_banks_total, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE))

# loans allowed banks
data$banks_allowances_total[is.na(data$banks_allowances_total)] <- ave(data$banks_allowances_total, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE))
data$banks_allowances_total[is.nan(data$banks_allowances_total)] <- ave(data$banks_allowances_total, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE))
data$banks_allowances_total[is.infinite(data$banks_allowances_total)] <- ave(data$banks_allowances_total, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE))

# net income banks
data$net_income_banks[is.na(data$net_income_banks)] <- ave(data$net_income_banks, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE))
data$net_income_banks[is.nan(data$net_income_banks)] <- ave(data$net_income_banks, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE))
data$net_income_banks[is.infinite(data$net_income_banks)] <- ave(data$net_income_banks, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE))






# impute the still missing values

for (col in columns_to_impute) {
  # Calculate the mean of the column
  mean_value <- mean(data[[col]], na.rm = TRUE)

  # Fill NA values with the mean
  data[[col]][is.na(data[[col]])] <- mean_value
}
data = pdata.frame(data,index=c("gvkey","year"))

duplicated_rows <- duplicated(data[c("gvkey", "year")], by = c("gvkey", "year"))
duplicate_data <- data[duplicated_rows, ]
data <- data[!duplicated_rows, ]


# create lagged variables
# xrd
data$lag1_xrd <- plm::lag(data$xrd, 1)
data$dt_lag =  plm::lag(data$dt, 1)
data$cshi_lag = plm::lag(data$cshi, 1)
data$dcpstk_lag <- NA
data$dcpstk_lag <- plm::lag(data$dcpstk, 1)

# age variable
data$age <- as.integer(data$year) - as.integer(data$firm_born)

# sales
data$sale =ifelse(data$sale>0, data$sale, 0)
data$lag1_sale = plm::lag(data$sale, 1)

# total assets lagged
data$lag1_at = plm::lag(data$at, 1)

# create the outcome variables r and d intensity
data$r_d_intensity = (data$xrd/data$at) * 100
data = subset(data, r_d_intensity < 100)
data$lead1_r_d_intensity = plm::lead(data$r_d_intensity, 1)
data$lead2_r_d_intensity = plm::lead(data$lead1_r_d_intensity, 1)

# create next outcome variable change in r and d intensityy
data$r_d_change_intensity = data$r_d_intensity - (data$lag1_xrd/data$lag1_at*100)
data$lead1_r_d_change_intensity = data$lead1_r_d_intensity - data$r_d_intensity
data$lead2_r_d_change_intensity = data$lead2_r_d_intensity - data$lead1_r_d_intensity

# ginds higher level
data$gind_first_4 = as.integer(substr(data$gind, 1, 4))

# sales
data$ln_sales_calculated <- log(data$sale)
# market to book value
data$m_b_calculated <- (data$mkvalt - data$ceq + data$csho*data$prcc_c)/data$at * 100
# cash flow?
data$cf_calculated = (data$ibc+data$dp)/data$at * 100
# plants and property
data$ppent_calculated = data$ppent/data$at * 100
# sales growth percent
data$sales_growth_calculated = data$ln_sales_calculated - log(data$lag1_sale)
#leverage
data$lev_calculated <- (data$dlc + data$dltt)*100 /data$at
# Return on assets
data$roa = (data$ni)/data$at * 100
#cash
data$ch_calculated <- data$ch/data$at * 100
# other investments
data$other_inv_sum_calculated = log(data$capx + data$aqc)
# other investments lagged
data$lag1_oth_inv_sum = plm::lag(data$other_inv_sum_calculated, 1)

# delta other investments
data$oth_inv_delta_calculated <- (data$other_inv_sum_calculated- data$lag1_oth_inv_sum) /data$lag1_at * 100

# create the variables for the financial dependence variable

data$delta_dt = data$dt        - data$dt_lag
data$delta_cshi = data$cshi    - data$cshi_lag
data$delta_dcpstk= data$dcpstk - data$dcpstk_lag
data$net_change_capital = (data$delta_dt + data$delta_cshi * data$prcc_c + data$delta_dcpstk)/data$at


columns_to_check = c("ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx",
                    "oth_inv_delta_calculated","ch_calculated", "at","age")
data <- data[complete.cases(data[, columns_to_check]) & apply(data[, columns_to_check], 1, function(x) all(is.finite(x))), ]


write.csv(data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv", row.names = FALSE)
