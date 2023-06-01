# this script performs preliminary data cleaning, matching of control and treatment group through the propensity score
# matching algorithm. 


# Load necessary libraries
#install.packages("MatchIt")
#install.packages("cobalt")
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("cobalt")
# Generate sample data
set.seed(123)  # Set seed for reproducibility

impute_all_columns <- function(data) {
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- ifelse(is.na(data[[col]]), ave(data[[col]], data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data[[col]])
    }
  }
  
  return(data)
}


typeof(data$year)
# Create a dataframe with variables for treatment, covariates, and outcome
raw_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv")
data = raw_data[c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx", 
                  "aqc","at","ppent", "ch","ibc", "mkvalt", "year")]#, "seq", "ch","ppent", "ivstch"]]


data2 = data
for (searchyear in unique(data$year)) {
  # Filter data for the current year
  year_data <- filter(data, year == searchyear)

}

# fill in the empty values with the mean for each gvkey
# at
data$at[is.na(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE))
data$at[is.nan(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE))
data$at[is.infinite(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE))

# dltt
data$dltt <- ifelse(is.na(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dltt)
data$dltt <- ifelse(is.nan(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dltt)
data$dltt <- ifelse(is.infinite(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dltt)


# sale
data$sale <- ifelse(is.na(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$sale)
data$sale <- ifelse(is.nan(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$sale)
data$sale <- ifelse(is.infinite(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$sale)

# ceq
data$ceq <- ifelse(is.na(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ceq)
data$ceq <- ifelse(is.nan(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ceq)
data$ceq <- ifelse(is.infinite(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ceq)


# prcc_c 
data$prcc_c <- ifelse(is.na(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$prcc_c)
data$prcc_c <- ifelse(is.nan(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$prcc_c)
data$prcc_c <- ifelse(is.infinite(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$prcc_c)


# csho
data$csho <- ifelse(is.na(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$csho)
data$csho <- ifelse(is.nan(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$csho)
data$csho <- ifelse(is.infinite(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$csho)

# dlc
data$dlc <- ifelse(is.na(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dlc)
data$dlc <- ifelse(is.nan(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dlc)
data$dlc <- ifelse(is.infinite(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dlc)

# capx
data$capx <- ifelse(is.na(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$capx)
data$capx <- ifelse(is.nan(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$capx)
data$capx <- ifelse(is.infinite(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$capx)


# aqc
data$aqc <- ifelse(is.na(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$aqc)
data$aqc <- ifelse(is.nan(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$aqc)
data$aqc <- ifelse(is.infinite(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$aqc)

# ppent 
data$ppent <- ifelse(is.na(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ppent)
data$ppent <- ifelse(is.nan(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ppent)
data$ppent <- ifelse(is.infinite(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, infinites.rm = TRUE)), data$ppent)

# ch
data$ch <- ifelse(is.na(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ch)
data$ch <- ifelse(is.nan(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ch)
data$ch <- ifelse(is.infinite(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ch)

# ibc
data$ibc <- ifelse(is.na(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ibc)
data$ibc <- ifelse(is.nan(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ibc)
data$ibc <- ifelse(is.infinite(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ibc)


# mkvalt 
data$mkvalt <- ifelse(is.na(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$mkvalt)
data$mkvalt <- ifelse(is.nan(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$mkvalt)
data$mkvalt <- ifelse(is.infinite(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, nan.infinite = TRUE)), data$mkvalt)

# dp 
data$dp <- ifelse(is.na(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dp)
data$dp <- ifelse(is.nan(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dp)
data$dp <- ifelse(is.infinite(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dp)






# create lagged variables
# xrd
data <- data %>%
  group_by(gvkey) %>%
  arrange(year) %>%
  mutate(lag1_xrd = lag(xrd, n = 1, default = NA)) %>%
  as.data.frame()

data$lag1_xrd <- ifelse(is.na(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_xrd)
data$lag1_xrd <- ifelse(is.nan(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_xrd)
data$lag1_xrd <- ifelse(is.infinite(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_xrd)

data$lag1_xrd[is.nan(data$lag1_xrd)] <- mean(data$lag1_xrd, na.rm = TRUE)


  
  
# sales
data$sale =ifelse(data$sale>0, data$sale, 0)

data <- data %>%
  group_by(gvkey) %>%
  arrange(year) %>%
  mutate(lag1_sale = lag(sale, n = 1, default = NA)) %>%
  as.data.frame()

data$lag1_sale <- ifelse(is.na(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_sale)
data$lag1_sale <- ifelse(is.nan(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_sale)
data$lag1_sale <- ifelse(is.infinite(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_sale)

data$lag1_sale[is.na(data$lag1_sale)] <- mean(data$lag1_sale, na.rm = TRUE)
data$lag1_sale[is.nan(data$lag1_sale)] <- mean(data$lag1_sale, nan.rm = TRUE)
data$lag1_sale[is.infinite(data$lag1_sale)] <- mean(data$lag1_sale, infinite.rm = TRUE)




# total assets 
data <- data %>%
  group_by(gvkey) %>%
  arrange(year) %>%
  mutate(lag1_at = lag(at, n = 1, default = NA)) %>%
  as.data.frame()

data$lag1_at <- ifelse(is.na(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_at)
data$lag1_at <- ifelse(is.nan(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_at)
data$lag1_at <- ifelse(is.infinite(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_at)

data$lag1_at[is.na(data$lag1_at)] <- mean(data$lag1_at, na.rm = TRUE)
data$lag1_at[is.nan(data$lag1_at)] <- mean(data$lag1_at, nan.rm = TRUE)
data$lag1_at[is.infinite(data$lag1_at)] <- mean(data$lag1_at, infinite.rm = TRUE)



# create the outcome variables 

data$r_d_intensity = (data$xrd/data$at) * 100
data$r_d_change_intensity =  (data$xrd - data$lag1_xrd) / data$lag1_at * 100
#data$r_d_change = (data$xrd - data$lag1_xrd) / 




# calculated values
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
data$lev_calculated <- (data$dlc + data$dltt) /data$at * 100
#cash
data$ch_calculated <- data$ch/data$at * 100 
# other investments 
data$other_inv_sum_calculated = data$capx + data$aqc


# other investments lagged 
data <- data %>%
  group_by(gvkey) %>%
  arrange(year) %>%
  mutate(lag1_oth_inv_sum = lag(other_inv_sum_calculated, n = 1, default = NA)) %>%
  as.data.frame()

data$lag1_oth_inv_sum <- ifelse(is.na(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_oth_inv_sum)
data$lag1_oth_inv_sum <- ifelse(is.nan(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_oth_inv_sum)
data$lag1_oth_inv_sum <- ifelse(is.infinite(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_oth_inv_sum)

data$lag1_oth_inv_sum[is.na(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, na.rm = TRUE)
data$lag1_oth_inv_sum[is.nan(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, nan.rm = TRUE)
data$lag1_oth_inv_sum[is.infinite(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, infinite.rm = TRUE)
# delta other investments
data$oth_inv_calculated <- (data$other_inv_sum- data$lag1_oth_inv_sum) /data$lag1_at * 100



#########################################
#########################################
#########################################

# Perform propensity score matching

data[sapply(data, is.infinite)] <- 0
data[sapply(data, is.nan)] <- 0

matched_data = data_frame()
# Iterate over unique years
for (searchyear in unique(data$year)) {
  # Filter data for the current year
  year_data <- filter(data, year == searchyear)
  
  # Perform propensity score matching
  ps_match <- matchit(treated ~ ln_sales_calculated + m_b_calculated + sales_growth_calculated + 
                        ppent_calculated + lev_calculated+ ch_calculated + r_d_change_intensity , data = year_data, method = "nearest")
  
  # Obtain matched data for the current year
  year_matched_data <- match.data(ps_match)
  
  # Add year variable to matched data
  year_matched_data$year <- searchyear
  
  # Append matched data for the current year to the overall matched dataset
  matched_data <- bind_rows(matched_data, year_matched_data)
}


# Output matched data and balance table
mean(matched_data$xrd)
aggregate(data[data$treated == 0, "xrd"], by = list(year = data[data$treated == 0, "year"]), FUN = mean)
aggregate(data[data$treated == 1, "xrd"], by = list(year = data[data$treated == 1, "year"]), FUN = mean)


balance_table <- cobalt::bal.tab(ps_match)
balance_table
write.csv(matched_data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv", row.names = FALSE)

