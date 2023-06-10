# this script performs preliminary data cleaning, matching of control and treatment group through the propensity score
# matching algorithm.


# Load necessary libraries
#install.packages("MatchIt")
#install.packages("cobalt")
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
# Generate sample data
set.seed(123)  # Set seed for reproducibility
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")



calculate_means_2 <- function(input_df) {
  # Convert all columns to numeric
  input_df <- as.data.frame(lapply(input_df, as.numeric))

  unique_gvkeys <- unique(input_df$gvkey)
  output_df <- data.frame(gvkey = unique_gvkeys)

  for (col_name in colnames(input_df)) {
    if (col_name != "gvkey") {
      means <- aggregate(input_df[col_name], list(input_df$gvkey), mean)
      output_df <- merge(output_df, means, by.x = "gvkey", by.y = "Group.1", all.x = TRUE)
      colnames(output_df)[colnames(output_df) == col_name] <- paste0(col_name)
    }
  }

  return(output_df)
}


# create lagged variables
# xrd

data <-
  data %>%
  group_by(gvkey) %>%
  mutate(lag1_xrd = dplyr::lag(xrd, n = 1, default = NA))

data$lag1_xrd <- ifelse(is.na(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_xrd)
data$lag1_xrd <- ifelse(is.nan(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_xrd)
data$lag1_xrd <- ifelse(is.infinite(data$lag1_xrd), ave(data$lag1_xrd, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_xrd)

data$lag1_xrd[is.nan(data$lag1_xrd)] <- mean(data$lag1_xrd, na.rm = TRUE)




# sales
data$sale =ifelse(data$sale>0, data$sale, 0)

data <-
  data %>%
  group_by(gvkey) %>%
  mutate(lag1_sale = dplyr::lag(sale, n = 1, default = NA))

data$lag1_sale <- ifelse(is.na(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_sale)
data$lag1_sale <- ifelse(is.nan(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_sale)
data$lag1_sale <- ifelse(is.infinite(data$lag1_sale), ave(data$lag1_sale, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_sale)

data$lag1_sale[is.na(data$lag1_sale)] <- mean(data$lag1_sale, na.rm = TRUE)
data$lag1_sale[is.nan(data$lag1_sale)] <- mean(data$lag1_sale, nan.rm = TRUE)
data$lag1_sale[is.infinite(data$lag1_sale)] <- mean(data$lag1_sale, infinite.rm = TRUE)




# total assets
data <- data %>%
  group_by(gvkey) %>%
  mutate(lag1_at = lag(at, n = 1, default = NA))

data <- data %>%
  group_by(gvkey) %>%
  dplyr::mutate(lead1_at = dplyr::lead(at, n = 1, default = NA))

data$lag1_at <- ifelse(is.na(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_at)
data$lag1_at <- ifelse(is.nan(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_at)
data$lag1_at <- ifelse(is.infinite(data$lag1_at), ave(data$lag1_at, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_at)

data$lag1_at[is.na(data$lag1_at)] <- mean(data$lag1_at, na.rm = TRUE)
data$lag1_at[is.nan(data$lag1_at)] <- mean(data$lag1_at, nan.rm = TRUE)
data$lag1_at[is.infinite(data$lag1_at)] <- mean(data$lag1_at, infinite.rm = TRUE)



# create the outcome variables

data$r_d_intensity = (data$xrd/data$at) * 100
data$r_d_change_intensity =  (data$xrd - data$lag1_xrd / data$at- data$lag1_at) * 100

data <- data %>%
  group_by(gvkey) %>%
  mutate(lead1_r_d_intensity = (dplyr::lead(r_d_intensity, n = 1, default = NA)))

data <- data %>%
  group_by(gvkey) %>%
  mutate(lead2_r_d_intensity = (dplyr::lead(lead1_r_d_intensity, n = 1, default = NA)))

data <- data %>%
  group_by(gvkey) %>%
  mutate(lead1_r_d_change_int = (dplyr::lead(r_d_change_intensity, n = 1, default = NA)))

data <- data %>%
  group_by(gvkey) %>%
  mutate(lead2_r_d_change_int = (dplyr::lead(lead1_r_d_change_int, n = 1, default = NA)))



data <- data[complete.cases(data$lead1_r_d_intensity), ]


#data$r_d_change = (data$xrd - data$lag1_xrd) /



# calculated values
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

#cash
data$ch_calculated <- data$ch/data$at * 100
# other investments
data$other_inv_sum_calculated = data$capx + data$aqc


# other investments lagged
data <- data %>%
  group_by(gvkey) %>%
  mutate(lag1_oth_inv_sum = dplyr::lag(other_inv_sum_calculated, n = 1, default = NA))

data$lag1_oth_inv_sum <- ifelse(is.na(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$lag1_oth_inv_sum)
data$lag1_oth_inv_sum <- ifelse(is.nan(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$lag1_oth_inv_sum)
data$lag1_oth_inv_sum <- ifelse(is.infinite(data$lag1_oth_inv_sum), ave(data$lag1_oth_inv_sum, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$lag1_oth_inv_sum)

data$lag1_oth_inv_sum[is.na(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, na.rm = TRUE)
data$lag1_oth_inv_sum[is.nan(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, nan.rm = TRUE)
data$lag1_oth_inv_sum[is.infinite(data$lag1_oth_inv_sum)] <- mean(data$lag1_oth_inv_sum, infinite.rm = TRUE)
# delta other investments
data$oth_inv_delta_calculated <- (data$other_inv_sum_calculated- data$lag1_oth_inv_sum) /data$lag1_at * 100



#########################################
#########################################
#########################################

# Perform propensity score matching

data[sapply(data, is.infinite)] <- 0
data[sapply(data, is.nan)] <- 0
data[sapply(data, is.na)] <- 0


match_basis = subset(data,year<2011)
match_basis = calculate_means_2(input_df = data)

matched_data = data_frame()
# Iterate over unique years
# Filter data for the current year
for (searchgind in unique(match_basis$gind_first_4)){
    # Perform propensity score matching
    gind_data = filter(match_basis, gind_first_4==searchgind)
    if (length(unique(gind_data$treated)) < 2) {
      next  # Skip to the next iteration
    }

    ps_match <- matchit(treated ~ ln_sales_calculated + cf_calculated + m_b_calculated + sales_growth_calculated +
                          ppent_calculated + lev_calculated+ ch_calculated + oth_inv_delta_calculated , data = gind_data, method = "nearest", distance = "glm")
    gind_matched_data <- match.data(ps_match)
#    gind_matched_data$year <- searchyear
    matched_data <- bind_rows(matched_data, gind_matched_data)
  }

# remap the matched observations


  #We estimate the propensity score from a logit regression
  #with the treatment dummy as the dependent variable and the mean values of ln(Sales),
  #M/B, PPE, CF, S.Growth, Leverage, Cash, change in R&D, change in x investments (i.e., capital and acquisition expenditures),
  #and the bank’s total securitized assets over the pre-period (2007–2009) as independent variables.

  # Obtain matched data for the current year


  # Add year variable to matched data

  # Append matched data for the current year to the overall matched dataset





remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)

remapped_matching_observations = subset(remapped_matching_observations, year >= 2007 & year <= 2015)

write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv", row.names = FALSE)
