 # this script is intended to prepare the firm-level dataset for the matching process.

# some housekeeping
rm(list = ls())

# required libraries
library("dplyr")    # For data manipulation
library("plm")

winsorize_dataframe <- function(data, column_names, v_d, v_u) {
  # percentile winsorizing of data
  for (col_name in column_names) {
    col_values <- data[[col_name]]
    q1 <- quantile(col_values, probs = v_d, na.rm = TRUE)
    q99 <- quantile(col_values, probs = v_u, na.rm = TRUE)

    data <- data[col_values > q1 & col_values <= q99, ]

  }

  return(data)
}

#read in the data
raw_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv")

# subset only used variables
data = raw_data[c("gvkey","sale","nr_of_lenders_pretreatment_period","nr_of_treated_lenders_pretreatment_period"
                  , "treated_sum_before_8", "treated","xrd","dp","ceq",
                  "prcc_c","csho", "dlc","dltt","treated", "treated_11", "treated_7","capx",
                  "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind","ebitda","ni","firm_born",
                  "avg_maturity_per_year","avg_maturity_pre_tr",
                  "mean_eps_pretreatment_bank",
                  "banks_allowances_loan_mean_pretreatment", "banks_net_income_mean_pretreatment", "banks_assets_total_mean_pretreatment",
                  "dt", "cshi", "dcpstk")]

# first checking, ensuring a full dataset for these variables.
columns_to_check <- c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx","ebitda","ni",
                        "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind", "dt", "cshi", "dcpstk")

data <- data[complete.cases(data[, columns_to_check]) & apply(data[, columns_to_check], 1, function(x) all(is.finite(x))), ]


# treated lender dependence variable creation

data$nr_of_lenders_pretreatment_period = ifelse(is.na(data$nr_of_lenders_pretreatment_period), 0, data$nr_of_lenders_pretreatment_period)
data$nr_of_treated_lenders_pretreatment_period = ifelse(is.na(data$nr_of_treated_lenders_pretreatment_period), 0, data$nr_of_treated_lenders_pretreatment_period)
data$tr_lender_share = data$nr_of_treated_lenders_pretreatment_period /data$nr_of_lenders_pretreatment_period

# treated lender share and the respective indicator variables
data$tr_lender_share[is.na(data$tr_lender_share) | is.infinite(data$tr_lender_share)] =  0
data$treatment_lender_share_50 = ifelse(data$tr_lender_share>= 0.5,1,0)
data$treatment_lender_share_75 = ifelse(data$tr_lender_share>= 0.75,1,0)
data$treatment_lender_share_1 =  ifelse(data$tr_lender_share== 1,1,0)

# filling the still empty loan maturity variables
data$avg_maturity_pre_tr[is.na(data$avg_maturity_pre_tr) | is.infinite(data$avg_maturity_pre_tr)] = 0
data$avg_maturity_per_year[is.na(data$avg_maturity_per_year) | is.infinite(data$avg_maturity_per_year)] = 0

# creating the age variable
data$age <- as.integer(data$year) - as.integer(data$firm_born)

# remove duplicate rows
duplicated_rows <- duplicated(data[c("gvkey", "year")], by = c("gvkey", "year"))
duplicate_data <- data[duplicated_rows, ]
data <- data[!duplicated_rows, ]

# create a paneldata dataframe. Age variable had to be created beforehand, since the
# datatype messes up the year variable, but it makes lag and lead variable much more
# easy to create
data = pdata.frame(data,index=c("gvkey","year"))


# create lagged variables
# xrd
data$lag1_xrd <- plm::lag(data$xrd, 1)

# other variables
data$dt_lag =  plm::lag(data$dt, 1)
data$cshi_lag = plm::lag(data$cshi, 1)
data$dcpstk_lag <- NA
data$dcpstk_lag <- plm::lag(data$dcpstk, 1)

# sales
# clean sales
data$sale =ifelse(data$sale>0, data$sale, 0)
data$lag1_sale = plm::lag(data$sale, 1)

# total assets lagged
data$lag1_at = plm::lag(data$at, 1)

# create the outcome variables r & d intensity
data$r_d_intensity = (data$xrd/data$at) * 100
data$r_d_intensity_sale = (data$xrd/data$sale) * 100

data = subset(data, r_d_intensity < 100)
data$lead1_r_d_intensity = plm::lead(data$r_d_intensity, 1)

data$lead2_r_d_intensity = plm::lead(data$lead1_r_d_intensity, 1)

data$lead1_r_d_intensity_sale =  plm::lead(data$r_d_intensity_sale, 1)
data$lead2_r_d_intensity_sale =  plm::lead(data$r_d_intensity_sale, 2)


# create  change in r and d intensity
data$r_d_change_intensity = data$r_d_intensity - (data$lag1_xrd/data$lag1_at*100)
data$lead1_r_d_change_intensity = data$lead1_r_d_intensity - data$r_d_intensity
data$lead2_r_d_change_intensity = data$lead2_r_d_intensity - data$lead1_r_d_intensity

# ginds higher level
data$gind_first_4 = as.integer(substr(data$gind, 1, 4))
data$gind_first_2 = as.integer(substr(data$gind, 1, 2))

# sales
data$ln_sales_calculated <- log(data$sale)
# market to book value
data$m_b_calculated <- (data$mkvalt - data$ceq + data$csho*data$prcc_c)/data$at
# cash flow
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
# log capx
data$log_capx = log(data$capx)
# delta other investments
data$oth_inv_delta_calculated <- (data$other_inv_sum_calculated- data$lag1_oth_inv_sum) /data$lag1_at * 100

# create the variables for the financial dependence variable
data$delta_dt = data$dt        - data$dt_lag
data$delta_cshi = data$cshi    - data$cshi_lag
data$delta_dcpstk= data$dcpstk - data$dcpstk_lag
data$net_change_capital = (data$delta_dt + data$delta_cshi * data$prcc_c + data$delta_dcpstk)/data$at
# winsorize data
columns_to_winsorize = c("lead1_r_d_intensity")
data = winsorize_dataframe(data, columns_to_winsorize, v_d = 0.001, v_u = 0.99)


# safe the prepared data for the matching process
write.csv(data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv", row.names = FALSE)
