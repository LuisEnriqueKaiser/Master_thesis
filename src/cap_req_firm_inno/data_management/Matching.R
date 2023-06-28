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



columns_for_matching =  c("ln_sales_calculated", "cf_calculated" ,"m_b_calculated",
                           "other_inv_sum_calculated", "capx","ebitda","roa","xrd","r_d_intensity",
                          "ppent_calculated" ,"lev_calculated", "ch_calculated","at","age",
                          "r_d_change_intensity", "gind_first_4", "gvkey", "treated","mkvalt",
                          "sales_growth_calculated","loan_banks_total", "oth_inv_delta_calculated")

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

match_basis = subset(data,year<=2011)
match_basis = subset(match_basis, year >2007)
match_basis = calculate_means2(input_df = match_basis, columns = columns_for_matching)


matched_data = data_frame()

# Iterate over unique years
# Filter data for the current year
# for (searchgind in unique(match_basis$gind_first_4)){
#     # Perform propensity score matching
#     gind_data = filter(match_basis, gind_first_4==searchgind)
#     if (length(unique(gind_data$treated)) < 2) {
#       print(searchgind)
#       next  # Skip to the next iteration
#     }
#     ps_match <- matchit(treated ~ lev_calculated + roa + sales_growth_calculated+
#                                   oth_inv_delta_calculated + m_b_calculated+cf_calculated+
#                                   at+ oth_inv_delta_calculated +r_d_change_intensity
#                           , data = gind_data,
#                           method = "nearest", distance = "logit")
#     gind_matched_data <- match.data(ps_match)
# # gind_matched_data$year <- searchyear
#     matched_data <- bind_rows(matched_data, gind_matched_data)
#   }


ps_match <- matchit(treated ~ cf_calculated + m_b_calculated +sales_growth_calculated+
                      ppent_calculated + lev_calculated+ ch_calculated+roa+
                      oth_inv_delta_calculated + at+ capx+age+
                      factor(gind_first_4)
                    , data = match_basis,
                    method = "nearest", distance = "logit")
matched_data = match.data(ps_match)
# remap the matched observations

remapped_matching_observations <- data %>% filter(gvkey %in% matched_data$gvkey)

remapped_matching_observations = subset(remapped_matching_observations, year >= 2005 & year <= 2020)

write.csv(remapped_matching_observations, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv", row.names = FALSE)
