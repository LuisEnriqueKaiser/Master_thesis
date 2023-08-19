# descriptives table generating
# this was the basis for the descriptive tables in the final thesis
# in an earlier iteration, I created the tables automatically
# outcommented parts are legacy of that, but are not important anymore.


rm(list=ls())
# Load necessary libraries
library("MatchIt")  # For propensity score matching
library("dplyr")    # For data manipulation
library("psych")
library("xtable")
library("Hmisc")

# rounding the numbers in a dataframe
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

# this function calculates the mean and the standard deviation of a dataframe
compute_mean_sd <- function(data) {
  # Convert data to numeric matrix
  data <- as.matrix(data, na.rm = TRUE)

  means <- apply(data, 2, function(x) mean(x[is.finite(x)], na.rm = TRUE))
  sds <- apply(data, 2, function(x) sd(x[is.finite(x)], na.rm = TRUE))

  # Create a dataframe with mean and standard deviation
  result <- data.frame(mean = means, sd = sds)
  return(result)
}


# load in the data
prematched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv")
postmatched_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
postmatched_data <- postmatched_data[, !(names(postmatched_data) %in% "subclass")]


# Generate sample data
set.seed(123)  # Set seed for reproducibility


# create statistics before matching
treated_prematch = subset(prematched_data,treated==1)
untreated_prematch = subset(prematched_data, treated==0)

# create the mean and sd dataframe
stats_treated_before = compute_mean_sd(treated_prematch)
stats_treated_before = stats_treated_before %>%
  rename(
    tr_before_mean = mean,
    tr_before_sd = sd
  )

# create the mean and sd dataframe 
stats_untreated_before = compute_mean_sd(untreated_prematch)
stats_untreated_before =  stats_untreated_before %>%
  rename(
    untr_before_mean = mean,
    untr_before_sd = sd
  )





# compute statistics for after matching 
treated_postmatch = subset(postmatched_data,treated==1)
untreated_postmatch = subset(postmatched_data, treated==0)

# create the mean and sd dataframe 
stats_treated_after = compute_mean_sd(treated_postmatch)
stats_treated_after = stats_treated_after %>%
  rename(
    tr_after_mean = mean,
    tr_after_sd = sd
  )

# create the mean and sd dataframe 
stats_untreated_after = compute_mean_sd(untreated_postmatch)
stats_untreated_after = stats_untreated_after %>%
  rename(
    untr_after_mean = mean,
    untr_after_sd = sd
  )

# cbind them together
stats_summary = cbind(stats_untreated_before,stats_treated_before,stats_untreated_after,stats_treated_after)
stats_summary$rownames <- rownames(stats_summary)


stats = stats_summary
# boil the dataframe down to my used variables
used_list = c( "lead1_r_d_intensity", "other_inv_sum_calculated", "lev_calculated", "cf_calculated","ch_calculated",
                    "sales_growth_calculated","m_b_calculated", "net_change_capital","avg_maturity_per_year","age")
names = c("R\\&D intensity_{t+1}", "Oth. inv. sum", "Leverage", "Cash Flow","Cash", "Sales Growth", "Market to Book ratio", "Net change capital",
          "Average maturity per year", "Firm age")

stats = subset(stats, stats$rownames %in% used_list)

mean(postmatched_data$other_inv_sum_calculated[is.finite(postmatched_data$other_inv_sum_calculated)], na.rm = TRUE)

#stats$diff_tr_untr_before = stats$untr_before_mean -stats$tr_before_mean
#stats$diff_tr_untr_atter = stats$untr_after_mean - stats$tr_after_mean


#stats = stats[,c(1,2,3,4,5,6,7, 8)]

# Change order of rows
#order_indices <- order(c(2, 3, 1, 4,5,6,7,8,9))  # Specify the desired order of row indices
#stats <- stats[order_indices, ]

#rownames(stats) <- c("R\\&D int._{t+1}", "Avg. maturity p. year","Firm age","Market to Book ratio",
#                     "Cash Flow","Cash","Sales Growth","Oth. inv. sum", "Leverage",  "Net change capital")


stats_rounded = round_df(df = stats, digit = 2)
stats_rounded
#stats_rounded = stats_rounded %>% rename( "(1)"= untr_before_mean)
#stats_rounded = stats_rounded %>% rename( "(2)"= untr_before_sd)
#stats_rounded = stats_rounded %>% rename( "(3)"= tr_before_mean)
#stats_rounded = stats_rounded %>% rename( "(4)"= tr_before_sd)

#stats_rounded = stats_rounded %>% rename( "Pre - Diff. "= diff_tr_untr_before)
#stats_rounded = stats_rounded %>% rename( "(5)"= untr_after_mean)
#stats_rounded = stats_rounded %>% rename( "(6)"= untr_after_sd)

#stats_rounded = stats_rounded %>% rename( "(7)"= tr_after_mean)
#stats_rounded = stats_rounded %>% rename( "(8)"= tr_after_sd)

#stats_rounded = stats_rounded %>% rename( "Pos - Diff."= diff_tr_untr_atter)



#Hmisc::latex(stats_rounded,Title = "Mean values variables - before and after matching",caption="Descriptive statistics for the unmatched and matched samples \\newline
#             Column (1): Mean untreated observations before matching; Column (2): Standard deviation untreated observations before matching; \\newline
#             Column (3): Mean treated observations before matching; Column (4): Standard deviation treated observations before matching; \\newline
#             Column (5): Mean untreated observations after matching; Column (6): Standard deviation untreated observations after matching; \\newline
#             Column (7): Mean treated observations after matching; Column (8): Standard deviation treated observations after matching. ",
#             center = "center",
#             file="/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/descriptives.tex")
