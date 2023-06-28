# financial dependence data prepper
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")

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

tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

data_pre_intervention = subset(data, year < tr_year)
median_fin_dep = median(data_pre_intervention$net_change_capital, na.rm = TRUE)
# Calculate the mean of net_change_capital for each gvkey
group_means <- aggregate(net_change_capital ~ gvkey, data = data_pre_intervention, mean, na.rm = TRUE)
# Merge the group means with the original dataframe
data <- merge(data, group_means, by = "gvkey", suffixes = c("", "_mean"))
data$fin_dependence_firms = ifelse(data$net_change_capital_mean>median_fin_dep, 1, 0)
mean(data$fin_dependence_firms)

########################################################################
########################################################################
########################################################################
#################################### For the sectors####################
########################################################################


# Calculate the mean of net change in capital for each gind_first_4 group
group_means <- aggregate(net_change_capital ~ gind_first_4, data = data, mean)

# Merge the group means with the original data
data <- merge(data, group_means, by = "gind_first_4", suffixes = c("", "_mean"))

# Create a new column "comparison" with values 1 if mean is above median_fin_dep, 0 otherwise
data$fin_dependent_sector <- ifelse(data$net_change_capital_mean > median_fin_dep, 1, 0)
mean(data$fin_dependent_sector)


write.csv(data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_prepared_for_h_effects.csv", row.names = FALSE)
