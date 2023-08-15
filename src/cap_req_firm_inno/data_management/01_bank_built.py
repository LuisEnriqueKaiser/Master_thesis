# this script has to be executed first, since it creates the bank specific tables, you need later

import pandas as pd


# define necessary functions
def extract_unique_values(data, column):
    """Extracts unique values out of a column in a dataframe."""
    unique_values = data[column].unique()
    unique_df = pd.DataFrame({column: unique_values})
    return unique_df


def process_dataframe(df, columns, threshold):
    """Creates the new treated column."""
    # Create a new column 'treated' with default value 0
    df2 = df
    df2["treated"] = 0

    # Iterate over each row in the dataframe
    for index, row in df2.iterrows():
        # Check if any of the predefined columns is below the threshold
        if any(row[col] < threshold for col in columns):
            # Set 'treated' value to 1
            df2.loc[index, "treated"] = 1

    return df2


def create_pivot(data, value):
    """Creates the pivot tables, I am using for the mapping onto the dealscan loan
    dataset.
    """
    data_pivot = pd.pivot_table(data, values=value, index="gvkey", columns="fyear")
    # Reset index to make gvkey a regular column
    data_pivot = data_pivot.reset_index()
    # Rename the columns with 'capr1_' prefix
    return data_pivot


# load in the data
banks_data = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/banks_data/compustat_yearly_banks.csv",
)
banks_data_matching_table = pd.read_excel(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regulatory_data/Linking_Documents/Dealscan_Lender_Link.xlsx",
    sheet_name="Compustat",
)


# clean the data
banks_data = banks_data.dropna(subset=["capr1"])
banks_data = banks_data[banks_data["fic"] == "USA"]

# matching table for legacy connection
banks_data_matching_table_dict = banks_data_matching_table.set_index(["gvkey"])[
    "lcoid"
].to_dict()
# match the lcoid banks
banks_data["lcoid_banks"] = banks_data["gvkey"].map(banks_data_matching_table_dict)

# Create a pivot table to reshape the data
banks_data_pivot = pd.pivot_table(
    banks_data,
    values="capr1",
    index="gvkey",
    columns="fyear",
)

# Reset index to make gvkey a regular column
banks_data_pivot = banks_data_pivot.reset_index()

# Rename the columns with 'capr1_' prefix
banks_data_pivot.columns = ["gvkey"] + [
    "capr1_" + str(year) for year in banks_data_pivot.columns[1:]
]
# get the treated column for a given threshold
banks_data_pivot_res = process_dataframe(
    df=banks_data_pivot,
    columns=["capr1_2007", "capr1_2008", "capr1_2009", "capr1_2010", "capr1_2011"],
    threshold=8,
)
# save the dataframe
banks_data_pivot_res.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/pivot_banks.csv",
)
##########################################################################################
# create the pivots for the 10 threshold
banks_data_pivot = pd.pivot_table(
    banks_data,
    values="capr1",
    index="gvkey",
    columns="fyear",
)
# Reset index to make gvkey a regular column
banks_data_pivot = banks_data_pivot.reset_index()
# Rename the columns with 'capr1_' prefix
banks_data_pivot.columns = ["gvkey"] + [
    "capr1_" + str(year) for year in banks_data_pivot.columns[1:]
]
banks_data_pivot_7_threshold = process_dataframe(
    df=banks_data_pivot,
    columns=["capr1_2007", "capr1_2008", "capr1_2009", "capr1_2010", "capr1_2011"],
    threshold=7,
)
##########################################################################################
# create the pivots for the 12 threshold

banks_data_pivot = pd.pivot_table(
    banks_data,
    values="capr1",
    index="gvkey",
    columns="fyear",
)
# Reset index to make gvkey a regular column
banks_data_pivot = banks_data_pivot.reset_index()
# Rename the columns with 'capr1_' prefix
banks_data_pivot.columns = ["gvkey"] + [
    "capr1_" + str(year) for year in banks_data_pivot.columns[1:]
]
banks_data_pivot_11_threshold = process_dataframe(
    df=banks_data_pivot,
    columns=["capr1_2007", "capr1_2008", "capr1_2009", "capr1_2010", "capr1_2011"],
    threshold=11,
)


##########################################################################################
# save the data
banks_data_pivot_7_threshold.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/pivot_banks7.csv",
)
banks_data_pivot_11_threshold.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/pivot_banks11.csv",
)

banks_data.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_data_processed.csv",
)
##########################################################################################

# create pivot for total assets
banks_data_assets_pivot = create_pivot(data=banks_data, value="at")
banks_data_assets_pivot.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_assets_total_pivot.csv",
)

# create a pivot for Net Income
banks_data_net_income_pivot = create_pivot(data=banks_data, value="ni")
banks_data_net_income_pivot.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_net_income_pivot.csv",
)

# create a pivot for all loans of a bank
banks_data_loans_pivot = create_pivot(data=banks_data, value="epsfi")
banks_data_loans_pivot.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_eps.csv",
)

# create a pivot for loan allowances
banks_data_loan_allowances_pivot = create_pivot(data=banks_data, value="lntal")
banks_data_loan_allowances_pivot.to_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_data_loans_allowances_pivot.csv",
)
