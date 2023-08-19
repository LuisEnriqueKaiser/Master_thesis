# financial dependence data prepper
# this script computes the financial dependence variable, introduced by Amore et al
# they had a certain process of splitting the data, which i am reproducing for my setup

# I make certain changes to their setup, in particular I create the
# indicator variable on the firm level,
# rather than on the sector level

# some housekeeping
rm(list=ls())

# required libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

# preprocess data
tr_year = 2011
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated

data_pre_intervention = subset(data, year <= tr_year)
# median group is defined by the median pre intervention after 2006
threshold_fin_dep = quantile(data_pre_intervention$net_change_capital,0.7, na.rm = TRUE)

########################################################################
########################################################################
######################## For the sectors################################
########################################################################


# Calculate the mean of net change in capital for each gind_first_4 group
group_means <- aggregate(net_change_capital ~ gind_first_2, data = data, mean)
# Merge the group means with the original data
data <- merge(data, group_means, by = "gind_first_2", suffixes = c("", "_mean"))
# Create a new column "comparison" with values 1 if mean is above median_fin_dep, 0 otherwise
data$fin_dependent_sector <- ifelse(data$net_change_capital_mean > threshold_fin_dep, 1, 0)

write.csv(data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data_prepared_for_fin_dependence_effects.csv", row.names = FALSE)
