# data prep

# Create a dataframe with variables for treatment, covariates, and outcome
raw_data = read.csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv")
data = raw_data[c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx",
                  "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind")]#, "seq", "ch","ppent", "ivstch"]]

columns_to_check <- c("gvkey","sale","xrd","dp","ceq","prcc_c","csho", "dlc","dltt","treated","capx",
                        "aqc","at","ppent", "ch","ibc", "mkvalt", "year", "gind")

# Drop rows with missing or infinite values
clean_df <- data[complete.cases(data[, columns_to_check]) & apply(data[, columns_to_check], 1, function(x) all(is.finite(x))), ]




library("dplyr")    # For data manipulation


# fill in the empty values with the mean for each gvkey
# at
data$at[is.na(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE))
data$at[is.nan(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE))
data$at[is.infinite(data$at)] <- ave(data$at, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE))

# dltt
data$dltt <- ifelse(is.na(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dltt)
data$dltt <- ifelse(is.nan(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dltt)
data$dltt <- ifelse(is.infinite(data$dltt), ave(data$dltt, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dltt)


# sale
data$sale <- ifelse(is.na(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$sale)
data$sale <- ifelse(is.nan(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$sale)
data$sale <- ifelse(is.infinite(data$sale), ave(data$sale, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$sale)

# ceq
data$ceq <- ifelse(is.na(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ceq)
data$ceq <- ifelse(is.nan(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ceq)
data$ceq <- ifelse(is.infinite(data$ceq), ave(data$ceq, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ceq)


# prcc_c
data$prcc_c <- ifelse(is.na(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$prcc_c)
data$prcc_c <- ifelse(is.nan(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$prcc_c)
data$prcc_c <- ifelse(is.infinite(data$prcc_c), ave(data$prcc_c, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$prcc_c)


# csho
data$csho <- ifelse(is.na(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$csho)
data$csho <- ifelse(is.nan(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$csho)
data$csho <- ifelse(is.infinite(data$csho), ave(data$csho, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$csho)

# dlc
data$dlc <- ifelse(is.na(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dlc)
data$dlc <- ifelse(is.nan(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dlc)
data$dlc <- ifelse(is.infinite(data$dlc), ave(data$dlc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dlc)

# capx
data$capx <- ifelse(is.na(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$capx)
data$capx <- ifelse(is.nan(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$capx)
data$capx <- ifelse(is.infinite(data$capx), ave(data$capx, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$capx)


# aqc
data$aqc <- ifelse(is.na(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$aqc)
data$aqc <- ifelse(is.nan(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$aqc)
data$aqc <- ifelse(is.infinite(data$aqc), ave(data$aqc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$aqc)

# ppent
data$ppent <- ifelse(is.na(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ppent)
data$ppent <- ifelse(is.nan(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ppent)
data$ppent <- ifelse(is.infinite(data$ppent), ave(data$ppent, data$gvkey, FUN = function(x) mean(x, infinites.rm = TRUE)), data$ppent)

# ch
data$ch <- ifelse(is.na(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ch)
data$ch <- ifelse(is.nan(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ch)
data$ch <- ifelse(is.infinite(data$ch), ave(data$ch, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ch)

# ibc
data$ibc <- ifelse(is.na(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$ibc)
data$ibc <- ifelse(is.nan(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$ibc)
data$ibc <- ifelse(is.infinite(data$ibc), ave(data$ibc, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$ibc)


# mkvalt
data$mkvalt <- ifelse(is.na(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$mkvalt)
data$mkvalt <- ifelse(is.nan(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$mkvalt)
data$mkvalt <- ifelse(is.infinite(data$mkvalt), ave(data$mkvalt, data$gvkey, FUN = function(x) mean(x, nan.infinite = TRUE)), data$mkvalt)

# dp
data$dp <- ifelse(is.na(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, na.rm = TRUE)), data$dp)
data$dp <- ifelse(is.nan(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, nan.rm = TRUE)), data$dp)
data$dp <- ifelse(is.infinite(data$dp), ave(data$dp, data$gvkey, FUN = function(x) mean(x, infinite.rm = TRUE)), data$dp)



write.csv(data, file = "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/data_prepared_for_matching.csv", row.names = FALSE)
