# descriptives table generating

# Load necessary libraries

library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("psych")
library(xtable)
library(Hmisc)
library(dplyr)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

prematched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")
postmatched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
# Generate sample data
set.seed(123)  # Set seed for reproducibility


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

treated_prematch = subset(prematched_data,treated==1)
untreated_prematch = subset(prematched_data, treated==0)


stats_treated_before = compute_mean_sd(treated_prematch)
stats_treated_before = stats_treated_before %>%
  rename(
    tr_before_mean = mean,
    tr_before_sd = sd
  )


stats_untreated_before = compute_mean_sd(untreated_prematch)
stats_untreated_before =  stats_untreated_before %>%
  rename(
    untr_before_mean = mean,
    untr_before_sd = sd
  )





# compute statistics
treated_postmatch = subset(postmatched_data,treated==1)
untreated_postmatch = subset(postmatched_data, treated==0)
stats_treated_after = compute_mean_sd(treated_postmatch)
stats_treated_after = stats_treated_after %>%
  rename(
    tr_after_mean = mean,
    tr_after_sd = sd
  )


stats_untreated_after = compute_mean_sd(untreated_postmatch)
stats_untreated_after = stats_untreated_after %>%
  rename(
    untr_after_mean = mean,
    untr_after_sd = sd
  )
stats_summary = cbind(stats_untreated_before,stats_treated_before,stats_untreated_after,stats_treated_after)
stats_summary$rownames <- rownames(stats_summary)


#stats_summary = subset(stats_summary, stats_summary$rownames %in% columns_for_matching)

stats = stats_summary
used_list = c( "r_d_intensity", "ln_sales_calculated", "cf_calculated", "m_b_calculated",
                    "sales_growth_calculated","ppent_calculated", "lev_calculated","roa","capx",
                    "oth_inv_delta_calculated","ch_calculated", "at","age")


stats = subset(stats, stats$rownames %in% used_list)
stats$diff_tr_untr_before = stats$untr_before_mean -stats$tr_before_mean
stats$diff_tr_untr_atter = stats$untr_after_mean - stats$tr_after_mean

stats = stats[,c(1,3,10,5,7,11)]
stats_rounded = round_df(df = stats, digit = 2)

stats_rounded = stats_rounded %>% rename( "Pre - Untr."= untr_before_mean)
stats_rounded = stats_rounded %>% rename( "Pre - Tr."= tr_before_mean)
stats_rounded = stats_rounded %>% rename( "Pre - Diff. "= diff_tr_untr_before)
stats_rounded = stats_rounded %>% rename( "Pos - Untr."= untr_after_mean)
stats_rounded = stats_rounded %>% rename( "Pos - Tr."= tr_after_mean)
stats_rounded = stats_rounded %>% rename( "Pos - Diff."= diff_tr_untr_atter)
rownames(stats_rounded) <- c("capx","at","age","rd int", "ln sale",
                             "m b", "cf", "ppent", "sales gr.", "lev", "roa",
                             "ch", "oth inv delta")



Hmisc::latex(stats_rounded,Title = "Mean values before and after matching",
             file="/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/descriptives.tex")



# Create the LaTeX table representation
#latex_table <- print(xtable(table_obj), include.rownames = TRUE)

# Scale down the table uniformly
#latex_table <- gsub("\\\\begin\\{tabular\\}\\{", "\\\\begin{tabular}{\\\\scalebox\\{0.9\\}{", latex_table)

# Save LaTeX table to a file
#output_file <- "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/descriptive_stats.tex"
#writeLines(latex_table, output_file)

write.csv(stats_summary, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/stats.csv", row.names = TRUE)
