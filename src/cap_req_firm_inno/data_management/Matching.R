# this script performs preliminary data cleaning, matching of control and treatment group through the propensity score
# matching algorithm.


# Load necessary libraries
#install.packages("MatchIt")
#install.packages("cobalt")
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
#install.packages("psych")
library("psych")
library(xtable)
# Generate sample data
set.seed(123)  # Set seed for reproducibility
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")


columns_to_winsorize = c("cf_calculated", "m_b_calculated",
                         "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx","oth_inv_delta_calculated",
                         "other_inv_sum_calculated","ch_calculated", "at","age")




columns_for_matching =  c("ln_sales_calculated", "cf_calculated" ,"m_b_calculated","net_change_capital",
                           "other_inv_sum_calculated", "capx","ebitda","roa","xrd","r_d_intensity",
                          "ppent_calculated" ,"lev_calculated", "ch_calculated","at","age","avg_maturity_per_year",
                           "r_d_change_intensity", "gind_first_4", "gvkey", "treated","mkvalt",
                          "sales_growth_calculated", "oth_inv_delta_calculated")

data <- data[complete.cases(data[, columns_for_matching]), ]

winsorize_dataframe <- function(data, column_names) {
  for (col_name in column_names) {
    col_values <- data[[col_name]]
    q1 <- quantile(col_values, probs = 0.001)
    q99 <- quantile(col_values, probs = 0.999)

    data <- data[col_values >= q1 & col_values <= q99, ]

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

match_basis = subset(data,year<2011)
match_basis = subset(match_basis, year >2007)
match_basis = calculate_means2(input_df = match_basis, columns = columns_for_matching)

ps_match <- matchit(treated ~ cf_calculated + m_b_calculated +sales_growth_calculated+
                      ppent_calculated + lev_calculated+ ch_calculated+roa+net_change_capital+avg_maturity_per_year+
                      oth_inv_delta_calculated + capx+age+
                      factor(gind_first_4)
                    , data = match_basis,
                    method = "nearest", distance = "logit")
matched_data = match.data(ps_match)
# remap the matched observations




remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)

remapped_matching_observations = subset(remapped_matching_observations, year >= 2007 & year <= 2016)
remapped_matching_observations = winsorize_dataframe(remapped_matching_observations, columns_to_winsorize)
write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv", row.names = FALSE)
