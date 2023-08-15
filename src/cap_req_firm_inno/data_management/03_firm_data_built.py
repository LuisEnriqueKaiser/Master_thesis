import numpy as np
import pandas as pd


def calculate_treated_sum(firm_df, deals_df):
    """Calculates the sum of 'treated' column values from 'deals_df' for each gvkey in
    'firm_df', and stores the sum in a new column 'treated_sum' in 'firm_df'.

    Args:
        firm_df (DataFrame): The first dataframe (Firm_level_data) with 'gvkey' column.
        deals_df (DataFrame): The second dataframe (deals) with 'gvkey_borrower' and 'treated' columns.

    Returns:
        DataFrame: The modified 'firm_df' dataframe with 'treated_sum' column added.

    """
    # Create a new column 'treated_sum' in the 'firm_df' dataframe
    firm_df["bank_connections_before"] = 0
    firm_df["bank_connections_after"] = 0
    firm_df["treated_sum_before"] = 0
    firm_df["treated_sum_after"] = 0
    firm_df["treated_all_before"] = 0

    # Loop through gvkey values in 'firm_df'
    for gvkey_value in firm_df["gvkey"]:
        # Subset 'deals_df' for gvkey_borrower equal to gvkey_value
        subset = deals_df[deals_df["gvkey_borrower"] == gvkey_value]
        subset_before = subset[subset["year"] <= 2011]
        subset_after = subset[subset["year"] > 2011]
        # Set the sum as the value in 'treated_sum' column of 'firm_df'
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_before",
        ] = subset_before["treated"].sum()
        if (subset_before["treated"].sum() > 0) & (
            subset_before["treated"].sum == subset_before.shape[0]
        ):
            firm_df.loc[firm_df["gvkey"] == gvkey_value, "treated_all_before"] = 1

        # set the treated sum after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_after",
        ] = subset_after["treated"].sum()
        # set the bank connections before
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_before",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
        # set the bank connections after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_after",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
    return firm_df


def calculate_treated_sum11(firm_df, deals_df):
    """Calculates the sum of 'treated' column values from 'deals_df' for each gvkey in
    'firm_df', and stores the sum in a new column 'treated_sum' in 'firm_df'.

    Args:
        firm_df (DataFrame): The first dataframe (Firm_level_data) with 'gvkey' column.
        deals_df (DataFrame): The second dataframe (deals) with 'gvkey_borrower' and 'treated' columns.

    Returns:
        DataFrame: The modified 'firm_df' dataframe with 'treated_sum' column added.

    """
    # Create a new column 'treated_sum' in the 'firm_df' dataframe
    firm_df["bank_connections_before"] = 0
    firm_df["bank_connections_after"] = 0
    firm_df["treated_sum_before"] = 0
    firm_df["treated_sum_after"] = 0
    firm_df["treated_all_before"] = 0

    # Loop through gvkey values in 'firm_df'
    for gvkey_value in firm_df["gvkey"]:
        # Subset 'deals_df' for gvkey_borrower equal to gvkey_value
        subset = deals_df[deals_df["gvkey_borrower"] == gvkey_value]
        subset_before = subset[subset["year"] <= 2011]
        subset_after = subset[subset["year"] > 2011]
        # Set the sum as the value in 'treated_sum' column of 'firm_df'
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_before",
        ] = subset_before["treated_11"].sum()
        if (subset_before["treated_11"].sum() > 0) & (
            subset_before["treated_11"].sum == subset_before.shape[0]
        ):
            firm_df.loc[firm_df["gvkey"] == gvkey_value, "treated_all_before"] = 1

        # set the treated sum after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_after",
        ] = subset_after["treated_11"].sum()
        # set the bank connections before
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_before",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
        # set the bank connections after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_after",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
    return firm_df


def calculate_treated_sum7(firm_df, deals_df):
    """Calculates the sum of 'treated' column values from 'deals_df' for each gvkey in
    'firm_df', and stores the sum in a new column 'treated_sum' in 'firm_df'.

    Args:
        firm_df (DataFrame): The first dataframe (Firm_level_data) with 'gvkey' column.
        deals_df (DataFrame): The second dataframe (deals) with 'gvkey_borrower' and 'treated' columns.

    Returns:
        DataFrame: The modified 'firm_df' dataframe with 'treated_sum' column added.

    """
    # Create a new column 'treated_sum' in the 'firm_df' dataframe
    firm_df["bank_connections_before"] = 0
    firm_df["bank_connections_after"] = 0
    firm_df["treated_sum_before"] = 0
    firm_df["treated_sum_after"] = 0
    firm_df["treated_all_before"] = 0

    # Loop through gvkey values in 'firm_df'
    for gvkey_value in firm_df["gvkey"]:
        # Subset 'deals_df' for gvkey_borrower equal to gvkey_value
        subset = deals_df[deals_df["gvkey_borrower"] == gvkey_value]
        subset_before = subset[subset["year"] <= 2011]
        subset_after = subset[subset["year"] > 2011]
        # Set the sum as the value in 'treated_sum' column of 'firm_df'
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_before",
        ] = subset_before["treated_7"].sum()
        if (subset_before["treated_7"].sum() > 0) & (
            subset_before["treated_7"].sum == subset_before.shape[0]
        ):
            firm_df.loc[firm_df["gvkey"] == gvkey_value, "treated_all_before"] = 1

        # set the treated sum after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "treated_sum_after",
        ] = subset_after["treated_7"].sum()
        # set the bank connections before
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_before",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
        # set the bank connections after
        firm_df.loc[
            firm_df["gvkey"] == gvkey_value,
            "bank_connections_after",
        ] = subset_before["gvkey_lender_unique_matching"].nunique()
    return firm_df


def check_all_ones(dataframe, column_name):
    """Checks if all values in the specified column of a dataframe are equal to 1.

    Args:
        dataframe (DataFrame): The dataframe to check.
        column_name (str): The name of the column to check.

    Returns:
        int: Returns 1 if all values are 1, and 0 otherwise.

    """
    if (dataframe[column_name] == 1).all():
        return 1
    else:
        return 0


def fill_missing_xrd(first_df, second_df):
    """Fills missing 'xrd' values in the first dataframe by searching for matching
    'gvkey' values in the second dataframe.

    Args:
        first_df (DataFrame): The first dataframe to update.
        second_df (DataFrame): The second dataframe to search for missing values.

    Returns:
        DataFrame: The updated first dataframe with missing 'xrd' values filled.

    """
    for index, row in first_df.iterrows():
        if pd.isnull(row["xrd"]):  # Check if 'xrd' value is NaN
            gvkey_value = row["gvkey"]
            year = row["fyear"]
            matching_row = second_df[
                (second_df["gvkey"] == gvkey_value) & (second_df["fyear"] == year)
            ]  # Find matching row in second dataframe
            if not matching_row.empty:
                xrd_value = matching_row.iloc[0][
                    "xrd"
                ]  # Retrieve 'xrd' value from second dataframe
                first_df.at[
                    index,
                    "xrd",
                ] = xrd_value  # Update 'xrd' value in the first dataframe

    return first_df


def filter_df(df):
    # Convert the 'year' column to datetime type if it's not already
    df["fyear"] = pd.to_datetime(df["fyear"], format="%Y")

    # Group the DataFrame by 'gvkey' and check if there are observations both before and after 2010
    filtered_df = df.groupby("gvkey").filter(
        lambda x: any(x["fyear"] <= pd.to_datetime("2011", format="%Y"))
        and any(x["fyear"] > pd.to_datetime("2011", format="%Y")),
    )

    return filtered_df


def calculate_bank_averages(deals, firm_level_data, column):
    """Calculates bank specific variable averages over all banks, predating the
    treatment at the.
    """
    firm_level_data[column] = np.nan
    for index, row in firm_level_data.iterrows():
        subset = deals[
            (deals["gvkey_borrower"] == row["gvkey"]) & (deals["year"] <= 2011)
        ]
        # firm_level_data.loc[]
        if not subset.empty:
            mean = subset[column].mean(skipna=True)
            firm_level_data.at[index, column] = mean
    return firm_level_data


def count_banks(deals, firm_level_data):
    # get the subset of the deals, which are important predating the treatment
    deals_subset = deals[deals["year"] <= 2011]

    # loop through all firm level data and assign the number
    for index, row in firm_level_data.iterrows():
        countset = deals_subset[deals_subset["gvkey_borrower"] == row["gvkey"]]
        # firm_level_data.loc[]
        if not countset.empty:
            Bank_count = countset["gvkey_lender_unique_matching"].nunique()
            firm_level_data.at[index, "nr_of_lenders_pretreatment_period"] = Bank_count
    return firm_level_data


def count_treated_banks(deals, firm_level_data):
    # get the subset of the deals, which are important predating the treatment
    deals_subset = deals[deals["year"] <= 2011]
    deals_subset = deals_subset[deals_subset["treated"] == 1]

    # loop through all firm level data and assign the number
    for index, row in firm_level_data.iterrows():
        countset = deals_subset[deals_subset["gvkey_borrower"] == row["gvkey"]]
        # firm_level_data.loc[]
        if not countset.empty:
            Bank_count = countset["gvkey_lender_unique_matching"].nunique()
            firm_level_data.at[
                index,
                "nr_of_treated_lenders_pretreatment_period",
            ] = Bank_count
    return firm_level_data


def avg_maturity(deals, firm_level_data):
    """Retrieve the average maturity in the pretreatment period of loans per firm."""
    # get the subset of the deals, which are important predating the treatment
    deals_subset = deals[deals["year"] <= 2011]

    # loop through all firm level data and assign the number
    for index, row in firm_level_data.iterrows():
        countset = deals_subset[deals_subset["gvkey_borrower"] == row["gvkey"]]
        # firm_level_data.loc[]
        if not countset.empty:
            avg_maturity = countset["Maturity"].mean(skipna=True)
            firm_level_data.at[index, "avg_maturity_pre_tr"] = avg_maturity
    return firm_level_data


def yearly_avg_maturity(deals, firm_level_data):
    """Retrieve the average maturity per year of all loans per firm."""
    deals_subset = deals
    # loop through all firm level data and assign the number
    for index, row in firm_level_data.iterrows():
        countset = deals_subset[
            (deals_subset["gvkey_borrower"] == row["gvkey"])
            & (deals_subset["year"] == row["year"])
        ]
        # firm_level_data.loc[]
        if not countset.empty:
            avg_maturity = countset["Maturity"].mean(skipna=True)
            firm_level_data.at[index, "avg_maturity_per_year"] = avg_maturity
    return firm_level_data


#######################################
# Load in the data
Firm_level_data = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Compustat/Firm_level_annually.csv",
)

R_D = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/R_D/R_D_Exp.csv",
)
deals = pd.read_excel(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/deals.xlsx",
)

bank_specific_variables = [
    "mean_eps_pretreatment_bank",
    "banks_allowances_loan_mean_pretreatment",
    "banks_net_income_mean_pretreatment",
    "banks_assets_total_mean_pretreatment",
]


# initial data cleaning
Firm_level_data = Firm_level_data[Firm_level_data["curcd"] == "USD"]
Firm_level_data = Firm_level_data.dropna(subset=["acctstd"])  # one accounting standard
Firm_level_data = Firm_level_data[
    Firm_level_data["acctstd"] != "ND"
]  # one accounting standard


Firm_level_data["year"] = Firm_level_data["datadate"].str[:4].astype(int)
# create the "born variable"
Firm_level_data = Firm_level_data.sort_values("year")
Firm_level_data["firm_born"] = Firm_level_data.groupby("gvkey")["year"].transform(
    "first",
)


Firm_level_data = Firm_level_data[Firm_level_data["fyear"] < 2020]  # right timeframe
Firm_level_data = Firm_level_data[Firm_level_data["fyear"] > 2005]
# only observations with r&d expenditures are allowed
Firm_level_data = Firm_level_data[Firm_level_data["xrd"].notna()]
Firm_level_data = Firm_level_data[Firm_level_data["xrd"] > 0]
# only non-financial firms
Firm_level_data = Firm_level_data[
    (Firm_level_data.gind < 400000) | (Firm_level_data.gind > 499999)
]
Firm_level_data = Firm_level_data[(Firm_level_data.gind < 601010)]
# ensure that every firm has one observation before and after the treatment period
Firm_level_data = filter_df(df=Firm_level_data)


# Connect with the Dealscan legacy dataset
deals_sub = deals

# create the bank specific variables in the firm level dataset
for bank_specific in bank_specific_variables:
    print(bank_specific)
    Firm_level_data = calculate_bank_averages(
        deals=deals_sub,
        firm_level_data=Firm_level_data,
        column=bank_specific,
    )

# bank count for each firm and maturity mapping for each firm
print("counting banks now")
Firm_level_data = count_banks(deals=deals, firm_level_data=Firm_level_data)
Firm_level_data = count_treated_banks(deals=deals, firm_level_data=Firm_level_data)


# maturity mapping for each firm
Firm_level_data = avg_maturity(deals=deals, firm_level_data=Firm_level_data)
Firm_level_data = yearly_avg_maturity(deals=deals, firm_level_data=Firm_level_data)

# create treatment variable
Firm_level_data = calculate_treated_sum(firm_df=Firm_level_data, deals_df=deals)
Firm_level_data = Firm_level_data.dropna(subset=["treated_sum_before"])
Firm_level_data["treated_sum_before_8"] = Firm_level_data["treated_sum_before"]
Firm_level_data["treated"] = np.where(Firm_level_data["treated_sum_before_8"] > 0, 1, 0)

# create treatment variable for the other thresholds
Firm_level_data = calculate_treated_sum7(firm_df=Firm_level_data, deals_df=deals)
Firm_level_data = Firm_level_data.dropna(subset=["treated_sum_before"])
Firm_level_data["treated_sum_before_7"] = Firm_level_data["treated_sum_before"]
Firm_level_data["treated_7"] = np.where(
    Firm_level_data["treated_sum_before_7"] > 0,
    1,
    0,
)


Firm_level_data = calculate_treated_sum11(firm_df=Firm_level_data, deals_df=deals)
Firm_level_data = Firm_level_data.dropna(subset=["treated_sum_before"])
Firm_level_data["treated_sum_before_11"] = Firm_level_data["treated_sum_before"]
Firm_level_data["treated_11"] = np.where(
    Firm_level_data["treated_sum_before_11"] > 0,
    1,
    0,
)

# save the data
Firm_level_data.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv",
)
