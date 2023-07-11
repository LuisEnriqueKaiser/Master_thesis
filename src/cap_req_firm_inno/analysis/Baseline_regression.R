# diff in diff design
# Main regression after matching
# some housekeeping
rm(list = ls())
# required libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

# loading in the data
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")

# only using necessary years of data
data = subset(data, data$year <= 2015)
data = subset(data, data$year >= 2008)

length(unique(data[["gvkey"]]))

# creating the did variables
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated
# creating the linear estimator
data$years_since = ifelse(data$year > 2011, data$year - tr_year, 0)
data$lin_est = data$years_since * data$treated


##################################################
             # Baseline regression 
##################################################


# Remove rows with missing values only in the subset columns
subset_columns <- c("lead1_r_d_intensity", "did")
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]
# estimation
did_reg_lead_1 = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg1)

# cluster standard errors
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1, cluster = "group", cluster.by = cluster_var)
# map clustered se onto the model 
did_reg_lead_1$vcov <- vcov_cluster
print(summary(did_reg_lead_1))


# same procedure for sector trends included
did_reg_lead_1_sectors = plm(lead1_r_d_intensity ~  did +
                       factor(year) + factor(gvkey) + factor(gind_first_2) * factor(year),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg1)
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_reg_lead_1_sectors, cluster = "group", cluster.by = cluster_var)
did_reg_lead_1_sectors$vcov <- vcov_cluster

print(summary(did_reg_lead_1_sectors))




# for lead 2 research and development, same procedure

subset_columns <- c("lead2_r_d_intensity", "did")
# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]


did_reg_lead_2 = plm(lead2_r_d_intensity ~  did +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg2)

cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2, cluster = "group", cluster.by = cluster_var)
did_reg_lead_2$vcov <- vcov_cluster

print(summary(did_reg_lead_2))

# same procedure for lead 2 r and d intensity with sector time fe 

did_reg_lead_2_sector = plm(lead2_r_d_intensity ~  did +factor(gind_first_2) * factor(year)+
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg2)

cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_reg_lead_2_sector, cluster = "group", cluster.by = cluster_var)
did_reg_lead_2_sector$vcov <- vcov_cluster

print(summary(did_reg_lead_2_sector))



# put all 4 regressions in a nice latex table and safe it

stargazer(
  did_reg_lead_1, did_reg_lead_2, did_reg_lead_1_sectors, did_reg_lead_2_sector,
  font.size = "footnotesize",
  title = "Regression Results: Firm characteristics \\newline This table reports the difference-in-difference results.
  Research and development intensity in the subsequent year and the year following are the outcome variables. 
  Samples were matched on a basket of covariates described beforehand.
  The first two columns report results without sector x time fixed effects, the two result columns on the right include sector x time fixed effects.
  Standard errors are clustered across matched subclasses.",
  no.space = TRUE,
  dep.var.labels = c("R\\&D int_{t+1}", "R\\&D int_{t+2}","R\\&D int_{t+1}", "R\\&D int_{t+2}"),
  covariate.labels = "Treat x Post",
  omit.stat = c("f", "adj.rsq"),
  column.sep.width = "4pt",
  digits = 3,
  add.lines = list(
    c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
    c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
    c('Sector trends', str_pad("\\xmark", 12, side = "left"), str_pad("\\xmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
    c('Additional covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"))
    ),
  #align = TRUE,
  omit = "year",
  header = FALSE,
  type = "latex",
  out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/baseline_regression_lead2.tex")






##################################################
# Baseline regression with a difference counter
##################################################
rm(list = ls())
# required libraries
library("tidyverse")
library("plm")
library("stargazer")
library("sandwich")
library("stringr")
library("lmtest")

# loading in the data
data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/matched_data.csv")
# only using necessary years of data
data = subset(data, data$year <= 2015)
data = subset(data, data$year >= 2008)
# creating the did variables
tr_year = 2011
data$post <- ifelse(data$year > tr_year, 1, 0)
data$did = data$post * data$treated
# creating the linear estimator
data$years_since = ifelse(data$year > 2011, data$year - tr_year, 0)
data$lin_est = data$years_since * data$treated




# Specify the subset of columns for complete cases check
subset_columns <- c("lead1_r_d_intensity", "lin_est")
# Remove rows with missing values only in the subset columns
subset_reg1 <- data[complete.cases(data[, subset_columns]), ]

# same procedure as beforehand with a different treatment variable
did_lin_var_lead_1 = plm(lead1_r_d_intensity ~  lin_est +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg1)

#print(summary(did_lin_var_lead_1))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_lin_var_lead_1, cluster = "group", cluster.by = cluster_var)
did_lin_var_lead_1$vcov <- vcov_cluster
#print(summary(did_lin_var_lead_1))


# with sector trends
did_lin_var_lead_1_sector = plm(lead1_r_d_intensity ~  lin_est + factor(year)*factor(gind_first_2)+
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         data = subset_reg1)
#print(summary(did_lin_var_lead_1_sector))
cluster_var <- subset_reg1$subclass
vcov_cluster <- vcovHC(did_lin_var_lead_1_sector, cluster = "group", cluster.by = cluster_var)
did_lin_var_lead_1_sector$vcov <- vcov_cluster
#print(summary(did_lin_var_lead_1_sector))







# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "lin_est")
# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_lin_var_lead_2 = plm(lead2_r_d_intensity ~  lin_est +
                       factor(year) + factor(gvkey),
                     model = "within",
                     index = c("gvkey", "year"),
                     data = subset_reg2)

#print(summary(did_lin_var_lead_2))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_lin_var_lead_2, cluster = "group", cluster.by = cluster_var)
# Perform coefficient test with clustered standard errors
did_lin_var_lead_2$vcov <- vcov_cluster
#print(summary(did_lin_var_lead_2))




# Specify the subset of columns for complete cases check
subset_columns <- c("lead2_r_d_intensity", "lin_est")
# Remove rows with missing values only in the subset columns
subset_reg2 <- data[complete.cases(data[, subset_columns]), ]
#subset_reg2 = subset_reg2[apply(subset_reg2, 1, function(row) all(is.finite(row))),]


did_lin_var_lead_2_sector = plm(lead2_r_d_intensity ~  lin_est + factor(year)*factor(gind_first_2)+
                           factor(year) + factor(gvkey),
                         model = "within",
                         index = c("gvkey", "year"),
                         data = subset_reg2)

#print(summary(did_lin_var_lead_2_sector))
cluster_var <- subset_reg2$subclass
vcov_cluster <- vcovHC(did_lin_var_lead_2_sector, cluster = "group", cluster.by = cluster_var)
did_lin_var_lead_2_sector$vcov <- vcov_cluster
#print(summary(did_lin_var_lead_2_sector))




# put the last 4 regressions in a nice latex table 

cov_names_lin=c("Treat x Years since")



stargazer(did_lin_var_lead_1, did_lin_var_lead_2,
           dep.var.labels=c("R\\&D int_{t+1}", "R\\&D int_{t+2}"),
           covariate.labels = c("Treat x Years since"),
           omit.stat = c("f","adj.rsq"),
           font.size = "footnotesize",
           column.sep.width = "4pt",
           add.lines = list(
            c('Year fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 20, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Firm fixed effects', str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left")),
            c('Other covariates', str_pad("No", 12, side = "left"), str_pad("No", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"), str_pad("\\checkmark", 12, side = "left"))
            ),
           no.space=FALSE,
           digits=3,
           omit = "year",
           title = "Linear treatment \\newline This table reports the results for the stepwise treatment variable.
           In 2012, the post variable is set to be 1, increasing by 1 in each year. 
           Research and development intensity in the subsequent year and the year following are the outcome variables. 
           Standard errors are clustered across matched subclasses.",
           header = TRUE,type = "latex", out = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regression_results/baseline_regression_linear.tex")
