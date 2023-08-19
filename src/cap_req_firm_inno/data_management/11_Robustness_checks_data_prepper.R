# this script performs preliminary data cleaning, matching of control and treatment group through the propensity score
# matching algorithm.
rm(list = ls())
# Load libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("psych")
library("xtable")
# Generate sample data
set.seed(123)  # Set seed for reproducibility
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")

# all possible matching variables
columns_for_matching =  c("ln_sales_calculated", "cf_calculated" ,"m_b_calculated","net_change_capital",
                          "other_inv_sum_calculated", "capx","ebitda","roa","xrd","r_d_intensity","nr_of_lenders_pretreatment_period",
                          "ppent_calculated" ,"lev_calculated", "ch_calculated","at","age","avg_maturity_per_year",
                          "r_d_change_intensity", "gind_first_4", "gvkey", "treated","mkvalt","tr_lender_share",
                          "sales_growth_calculated", "oth_inv_delta_calculated", "treated_7")

# percentile winsorizing function
winsorize_dataframe <- function(data, column_names, v_d, v_u) {
  for (col_name in column_names) {
    col_values <- data[[col_name]]
    q1 <- quantile(col_values, probs = v_d, na.rm = TRUE)
    q99 <- quantile(col_values, probs = v_u, na.rm = TRUE)

    data <- data[col_values > q1 & col_values <= q99, ]

  }

  return(data)
}


# function creates pretreatment means for all variables given to it, except for the gvkey variable 
calculate_means2 <- function(input_df, columns) {
  # Convert all columns to numeric
  input_df <- as.data.frame(lapply(input_df, as.numeric))

  unique_gvkeys <- unique(input_df$gvkey)
  output_df <- data.frame(gvkey = unique_gvkeys)

  for (col_name in columns) {
    if (col_name != "gvkey") {
      means <- aggregate(input_df[col_name], list(input_df$gvkey), mean)
      for (r in 1:nrow(means)){
        if  ((is.infinite(means[r,2])) | (is.na(means[r,2])) | (is.nan(means[r,2]))){
          means[r,2] <- mean(input_df[[col_name]][is.finite(input_df[[col_name]])], na.rm = TRUE)
        }
      }
      output_df <- merge(output_df, means, by.x = "gvkey", by.y = "Group.1", all.x = TRUE)
      colnames(output_df)[colnames(output_df) == col_name] <- paste0(col_name)
    }
  }

  return(output_df)
}

# matching for the 7 percent threshold
# winsorizing for t+2, which was not done beforehand in the prep script
columns_to_winsorize = c("lead2_r_d_intensity")
data = winsorize_dataframe(data, columns_to_winsorize, v_d = 0.001, v_u = 0.99)

# matching
match_basis = subset(data,year<=2011)
match_basis = subset(match_basis, year >2007)
match_basis = calculate_means2(input_df = match_basis, columns = columns_for_matching)
matched_data = data_frame()

# matching procedure
ps_match <- matchit(treated_7 ~ other_inv_sum_calculated+lev_calculated+ch_calculated+
                      sales_growth_calculated+m_b_calculated+net_change_capital+
                      factor(gind_first_4)
                    , data = match_basis,
                    method = "nearest", distance = "logit")
matched_data = match.data(ps_match)

# remap the matched observations
remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)

remapped_matching_observations$subclass <- matched_data$subclass[match(remapped_matching_observations$gvkey, matched_data$gvkey)]
remapped_matching_observations = subset(remapped_matching_observations, year >= 2008 & year <= 2016)

length(unique(remapped_matching_observations$gvkey))
write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_7.csv", row.names = FALSE)



########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

# now the matching process for the 11 percent threshold
rm(list = ls())



# Load necessary libraries
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("psych")
library("xtable")
# Generate sample data
set.seed(123)  # Set seed for reproducibility
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")

# all possible matching columns
columns_for_matching =  c("ln_sales_calculated", "cf_calculated" ,"m_b_calculated","net_change_capital",
                          "other_inv_sum_calculated", "capx","ebitda","roa","xrd","r_d_intensity","nr_of_lenders_pretreatment_period",
                          "ppent_calculated" ,"lev_calculated", "ch_calculated","at","age","avg_maturity_per_year",
                          "r_d_change_intensity", "gind_first_4", "gvkey", "treated","mkvalt","tr_lender_share",
                          "sales_growth_calculated", "oth_inv_delta_calculated", "treated_11")


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



calculate_means2 <- function(input_df, columns) {
  # Convert all columns to numeric
  input_df <- as.data.frame(lapply(input_df, as.numeric))

  unique_gvkeys <- unique(input_df$gvkey)
  output_df <- data.frame(gvkey = unique_gvkeys)

  for (col_name in columns) {
    if (col_name != "gvkey") {
      means <- aggregate(input_df[col_name], list(input_df$gvkey), mean)
      for (r in 1:nrow(means)){
        if  ((is.infinite(means[r,2])) | (is.na(means[r,2])) | (is.nan(means[r,2]))){
          means[r,2] <- mean(input_df[[col_name]][is.finite(input_df[[col_name]])], na.rm = TRUE)
        }
      }
      output_df <- merge(output_df, means, by.x = "gvkey", by.y = "Group.1", all.x = TRUE)
      colnames(output_df)[colnames(output_df) == col_name] <- paste0(col_name)
    }
  }

  return(output_df)
}

# matching
# winsorizing, which was not done beforehand in the prep script
columns_to_winsorize = c("lead2_r_d_intensity")
data = winsorize_dataframe(data, columns_to_winsorize, v_d = 0.001, v_u = 0.999)
match_basis = subset(data,year<=2011)
match_basis = subset(match_basis, year >2007)
match_basis = calculate_means2(input_df = match_basis, columns = columns_for_matching)


matched_data = data_frame()

# matching procedure
ps_match <- matchit(treated_11 ~ other_inv_sum_calculated+lev_calculated+ch_calculated+
                      sales_growth_calculated+m_b_calculated+net_change_capital+
                      factor(gind_first_4)
                    , data = match_basis,
                    method = "nearest", distance = "logit")
matched_data = match.data(ps_match)

# remap the matched observations
remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)

remapped_matching_observations$subclass <- matched_data$subclass[match(remapped_matching_observations$gvkey, matched_data$gvkey)]
remapped_matching_observations = subset(remapped_matching_observations, year >= 2008 & year <= 2016)

columns_to_winsorize = c("lead1_r_d_intensity")
#remapped_matching_observations = winsorize_dataframe(remapped_matching_observations, columns_to_winsorize)
write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_robust_11.csv", row.names = FALSE)
