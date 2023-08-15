# this script performs preliminary data cleaning, matching of control and treatment group through the propensity score
# matching algorithm.

# some housekeeping
rm(list = ls())

# Load necessary libraries
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("psych")
library("xtable")


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



set.seed(123)  # Set seed for reproducibility

data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")

columns_for_matching =  c("ln_sales_calculated", "cf_calculated" ,"m_b_calculated","net_change_capital",
                           "other_inv_sum_calculated", "capx","ebitda","roa","xrd","r_d_intensity","nr_of_lenders_pretreatment_period",
                          "ppent_calculated" ,"lev_calculated", "ch_calculated","at","age","avg_maturity_per_year",
                           "r_d_change_intensity", "gind_first_4", "gvkey", "treated","mkvalt","tr_lender_share",
                          "sales_growth_calculated", "oth_inv_delta_calculated")
# ensure data availability
data <- data[complete.cases(data[, columns_for_matching]), ]
# matching
match_basis = subset(data,year<=2011)
match_basis = subset(match_basis, year >=2008)
match_basis = calculate_means2(input_df = match_basis, columns = columns_for_matching)
# matching process
ps_match <- matchit(treated ~ other_inv_sum_calculated+lev_calculated+ch_calculated+cf_calculated+
                      sales_growth_calculated+m_b_calculated+net_change_capital+age+avg_maturity_per_year+
                      factor(gind_first_4)
                    , data = match_basis,
                    method = "nearest", distance = "logit")
matched_data = match.data(ps_match)
# remapping
remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)
remapped_matching_observations$subclass <- matched_data$subclass[match(remapped_matching_observations$gvkey, matched_data$gvkey)]
remapped_matching_observations = subset(remapped_matching_observations, year >= 2008 & year <= 2016)
# saving
write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv", row.names = FALSE)
