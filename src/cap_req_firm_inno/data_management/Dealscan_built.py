# this script connects the dealscan dataset with its lender part and the gvkey from banks and firms.
# on top of that, it adds bank specific datapoints from the compustat bank dataset.

import pandas as pd


def subset_duplicates(dataframe, column):
    duplicates = dataframe[dataframe.duplicated(subset=column, keep=False)]
    unique = dataframe.drop_duplicates(subset=column, keep=False)
    return duplicates, unique


def group_duplicates(df, column):
    df_sorted = df.sort_values(by=column)
    groups = df_sorted.groupby(column)
    result_list = []

    # Get the maximum number of occurrences for any value in the column
    max_occurrences = groups.size().max()

    for i in range(max_occurrences):
        temp_df = pd.DataFrame()

        for _, group_df in groups:
            if len(group_df) > i:
                temp_df = pd.concat([temp_df, group_df.iloc[i : i + 1]])

        result_list.append(temp_df)

    return result_list


# legacy dealscan data on lenders
lenders = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Dealscan_legacy/Lender_legacy.csv",
    encoding="ISO-8859-1",
)

# legacy dealscan data on firms and deals
deals_firms = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Dealscan_legacy/facilities_legacy.csv",
    encoding="ISO-8859-1",
)

# gvkeys to bcoid from dealscan linking table for firms
gvkey_matches = pd.read_excel(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Dealscan-Compustat_Linking_Database_0XRRFYV.xlsx",
    sheet_name="link_data",
)

# Bank data gvkey matching table
banks_data_matching_table = pd.read_excel(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regulatory_data/Linking_Documents/Dealscan_Lender_Link.xlsx",
    sheet_name="Compustat",
)


# first: combine the different dealscan datasets

deals_firms["year"] = deals_firms["FacilityStartDate"].astype(str).str[:4]
deals_firms["year"] = deals_firms["year"].astype(int)
deals_firms = deals_firms[deals_firms["CountryOfSyndication"] == "USA"]

# lenders & cleaning of lenders
facid_bank_id = lenders[["FacilityID", "CompanyID"]]
facid_bank_id["FacilityID"] = facid_bank_id["FacilityID"].astype(int)
facid_bank_id["CompanyID"] = facid_bank_id["CompanyID"].astype(int)
facid_bank_id_dict = facid_bank_id.set_index("FacilityID").T.to_dict("list")
deals_firms["lenderid"] = deals_firms["FacilityID"].map(facid_bank_id_dict)

# Match the GVKEYS for companies with the borrowerIDs
gvkey_matches_dict = gvkey_matches.set_index("bcoid")["gvkey"].to_dict()
gvkey_matches_facid_dict = gvkey_matches.set_index("facid")["gvkey"].to_dict()
deals_firms["gkvey_bcoid"] = deals_firms["BorrowerCompanyID"].map(gvkey_matches_dict)
nan_count = deals_firms["gkvey_bcoid"].isna().sum()

# match the GVKEYs for the facility ID
deals_firms["gvkey_facid"] = deals_firms["FacilityID"].map(gvkey_matches_facid_dict)
deals_firms["gvkey_borrower"] = deals_firms["gkvey_bcoid"].fillna(
    deals_firms["gvkey_facid"],
)
nan_count_2 = deals_firms["gvkey_borrower"].isna().sum()

deals_firms = deals_firms.drop(["gkvey_bcoid", "gvkey_facid"], axis=1)
deals_firms = deals_firms.dropna(subset=["gvkey_borrower"])
deals_firms = deals_firms.dropna(subset=["lenderid"])
deals_firms["gvkey_borrower"].isna().sum()


# Match the GVKEYS for the banks with the corresponding lenderids

deals_firms["FacilityStartDate"] = pd.to_datetime(deals_firms["FacilityStartDate"])
banks_data_matching_table["year"] = (
    banks_data_matching_table["ds_start"].astype(str).str[:4]
)
banks_data_matching_table["ds_start"] = pd.to_datetime(
    banks_data_matching_table["ds_start"],
    format="%Y%m%d",
)
duplicates, uniques = subset_duplicates(banks_data_matching_table, "lcoid")
gvkey_lcoid_mapping = uniques.set_index("lcoid")["gvkey"].to_dict()
deals_firms["gvkey_lender_unique_matching"] = deals_firms.set_index(
    "lenderid",
).index.map(gvkey_lcoid_mapping)

# fill the gaps
duplis = []
duplis = group_duplicates(duplicates, "lcoid")
first = duplis[0]
second = duplis[1]
third = duplis[2]

gvkey_lcoid_mapping = first.set_index("lcoid")["gvkey"].to_dict()
deals_firms["gvkey_first_of_duplicates"] = deals_firms.set_index("lenderid").index.map(
    gvkey_lcoid_mapping,
)

gvkey_lcoid_mapping = second.set_index("lcoid")["gvkey"].to_dict()
deals_firms["gvkey_second_of_duplicates"] = deals_firms.set_index("lenderid").index.map(
    gvkey_lcoid_mapping,
)

gvkey_lcoid_mapping = third.set_index("lcoid")["gvkey"].to_dict()
deals_firms["gvkey_third_of_duplicates"] = deals_firms.set_index("lenderid").index.map(
    gvkey_lcoid_mapping,
)

deals_firms["gvkey_lender_unique_matching"] = deals_firms[
    "gvkey_lender_unique_matching"
].fillna(deals_firms["gvkey_first_of_duplicates"])
deals_firms["gvkey_lender_unique_matching"] = deals_firms[
    "gvkey_lender_unique_matching"
].fillna(deals_firms["gvkey_second_of_duplicates"])
deals_firms["gvkey_lender_unique_matching"] = deals_firms[
    "gvkey_lender_unique_matching"
].fillna(deals_firms["gvkey_third_of_duplicates"])


# fill the gaps with the linking table from chava
gvkey_matches_dict = gvkey_matches.set_index("bcoid")["gvkey"].to_dict()
# Add the new column "RSSD" to deals_firms DataFrame using the mapping dictionary
deals_firms["gkvey_lcoid_chavas"] = deals_firms["lenderid"].map(gvkey_matches_dict)
deals_firms["gvkey_lender_unique_matching"] = deals_firms[
    "gvkey_lender_unique_matching"
].fillna(deals_firms["gkvey_lcoid_chavas"])
deals_firms = deals_firms.dropna(subset=["gvkey_lender_unique_matching"])
# Connect to the bank dataset with the gvkeys
banks_data_pivot = pd.read_csv(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/pivot_banks.csv",
)
banks_data_pivot_dict = banks_data_pivot.set_index("gvkey")["treated"].to_dict()
deals_firms["treated"] = deals_firms["gvkey_lender_unique_matching"].map(
    banks_data_pivot_dict,
)
deals_firms["treated_duplis_1"] = deals_firms["gvkey_first_of_duplicates"].map(
    banks_data_pivot_dict,
)
deals_firms["treated_duplis_2"] = deals_firms["gvkey_second_of_duplicates"].map(
    banks_data_pivot_dict,
)
deals_firms["treated_duplis_3"] = deals_firms["gvkey_third_of_duplicates"].map(
    banks_data_pivot_dict,
)
deals_firms["treated_chava_banks"] = deals_firms["gvkey_lender_unique_matching"].map(
    banks_data_pivot_dict,
)

deals_firms["treated"] = deals_firms["treated"].fillna(deals_firms["treated_duplis_1"])
deals_firms["treated"] = deals_firms["treated"].fillna(deals_firms["treated_duplis_2"])
deals_firms["treated"] = deals_firms["treated"].fillna(deals_firms["treated_duplis_3"])
deals_firms["treated"] = deals_firms["treated"].fillna(
    deals_firms["treated_chava_banks"],
)


deals_firms["treated"].sum()
deals_firms.to_excel(
    "/Users/luisenriquekaiser/Documents/Master Thesis/Data/Processed_data/deals.xlsx",
)
