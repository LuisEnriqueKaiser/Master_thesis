# descriptives
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

compute_mean_sd <- function(data) {
  # Convert data to numeric matrix
  data <- as.matrix(data, na.rm = TRUE)

  means <- apply(data, 2, function(x) mean(x[is.finite(x)], na.rm = TRUE))
  sds <- apply(data, 2, function(x) sd(x[is.finite(x)], na.rm = TRUE))

  # Create a dataframe with mean and standard deviation
  result <- data.frame(mean = means, sd = sds)
  return(result)
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

treated = subset(data,treated==1)
untreated = subset(data, treated==0)
stats_treated_before = compute_mean_sd(treated)
stats_treated_before = stats_treated_before %>%
  rename(
    tr_before_mean = mean,
    tr_before_sd = sd
  )
stats_untreated_before = compute_mean_sd(untreated)
stats_untreated_before =  stats_untreated_before %>%
  rename(
    untr_before_mean = mean,
    untr_before_sd = sd
  )





# compute statistics
treated = subset(remapped_matching_observations,treated==1)
untreated = subset(remapped_matching_observations, treated==0)
stats_treated_after = compute_mean_sd(treated)
stats_treated_after = stats_treated_after %>%
  rename(
    tr_after_mean = mean,
    tr_after_sd = sd
  )


stats_untreated_after = compute_mean_sd(untreated)
stats_untreated_after = stats_untreated_after %>%
  rename(
    untr_after_mean = mean,
    untr_after_sd = sd
  )
stats_summary = cbind(stats_untreated_before,stats_treated_before,stats_untreated_after,stats_treated_after)
stats_summary$rownames <- rownames(stats_summary)


stats_summary = subset(stats_summary, stats_summary$rownames %in% columns_for_matching)

stats = stats_summary
used_list = c("lev_calculated", "roa", "sales_growth_calculated",
              "oth_inv_delta_calculated", "m_b_calculated","cf_calculated",
              "at", "oth_inv_delta_calculated", "xrd", "r_d_intensity", "age")
stats = subset(stats, stats$rownames %in% used_list)
stats = stats[,-9]

stats$diff_tr_untr_before = stats$untr_before_mean -stats$tr_before_mean
stats$diff_tr_untr_atter = stats$untr_after_mean - stats$tr_after_mean

stats = stats[,c(1,2,3,4,9,5,6,7,8,10)]
# Create the LaTeX table representation
#latex_table <- print(xtable(table_obj), include.rownames = TRUE)

# Scale down the table uniformly
#latex_table <- gsub("\\\\begin\\{tabular\\}\\{", "\\\\begin{tabular}{\\\\scalebox\\{0.9\\}{", latex_table)

# Save LaTeX table to a file
#output_file <- "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/descriptive_stats.tex"
#writeLines(latex_table, output_file)

write.csv(stats_summary, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/stats.csv", row.names = TRUE)
