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
        subset_before = subset[subset["year"] < 2010]
        subset_after = subset[subset["year"] >= 2010]
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


# enrich the missing xrd values with the RQ dataset
Firm_level_data = fill_missing_xrd(first_df=Firm_level_data, second_df=R_D)


# initial data cleaning
Firm_level_data = Firm_level_data[Firm_level_data["curcd"] == "USD"]
Firm_level_data = Firm_level_data.dropna(subset=["acctstd"])  # one accounting standard
Firm_level_data = Firm_level_data[
    Firm_level_data["acctstd"] != "ND"
]  # one accounting standard
Firm_level_data["year"] = Firm_level_data["datadate"].str[:4].astype(int)
Firm_level_data = Firm_level_data[Firm_level_data["fyear"] < 2018]  # right timeframe
Firm_level_data = Firm_level_data[Firm_level_data["fyear"] > 2005]

Firm_level_data = Firm_level_data[Firm_level_data["xrd"].notna()]
Firm_level_data = Firm_level_data[Firm_level_data["xrd"] > 0]
# only non-financial firms
Firm_level_data = Firm_level_data[
    (Firm_level_data.gind < 400000) | (Firm_level_data.gind > 499999)
]
Firm_level_data = Firm_level_data[(Firm_level_data.gind < 601010)]


# Connect with the Dealscan legacy dataset

Firm_level_data = calculate_treated_sum(firm_df=Firm_level_data, deals_df=deals)
Firm_level_data["treated"] = np.where(Firm_level_data["treated_sum_before"] > 0, 1, 0)

Firm_level_data.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/final_firm_level_data.csv",
)
