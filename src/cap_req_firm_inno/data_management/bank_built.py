# this script 


import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from scipy import stats
import requests


def create_banks_df(df): 
    banks_unique = extract_unique_values(data = df, column="gvkey")
    banks_unique["capr1_percentages_2009"] = np.nan
    banks_unique["capr1_percentages_2010"] = np.nan
    banks_unique["capr1_percentages_2011"] = np.nan
    banks_unique["capr1_percentages_2012"] = np.nan
    banks_unique["capr1_percentages_2013"] = np.nan
    return banks_unique


def calculate_capr1_ratio(df, data_to_search_in): 
    for gvkey, index in enumerate(df["gvkey"]): 
        data = data_to_search_in[data_to_search_in['gvkey'] == gvkey]
        capr1_above_7 = data['capr1'] > 7 
        df.at[index, "capr1_percentages_under_7"] = capr1_above_7.mean() - 1
        capr1_percentages_2013 = data[data["fyear"] == 2013]["capr1"]
        df.at[index,"capr1_percentages_2013"] =  capr1_percentages_2013
    return df 

def extract_unique_values(data, column):
    unique_values = data[column].unique()
    unique_df = pd.DataFrame({column: unique_values})
    return unique_df


def calculate_capr1_ratio(data, df_to_search_in):
    capr1_ratio = []
    capr1_ratio_2013 = []
    
    for gvkey in data['gvkey']:
        gvkey_data = df_to_search_in[df_to_search_in['gvkey'] == gvkey]
        
        capr1_above_7 = len(gvkey_data[gvkey_data['capr1'] > 7])
        capr1_ratio.append(capr1_above_7 / len(gvkey_data))
        
        capr1_2013_data = gvkey_data[gvkey_data['fyear'] == 2013]
        if len(capr1_2013_data) > 0:
            if capr1_2013_data.iloc[0]['capr1'] > 7:
                capr1_ratio_2013.append(1)
            else:
                capr1_ratio_2013.append(0)
        else:
            capr1_ratio_2013.append(None)
    
    data['capr1_ratio'] = capr1_ratio
    data['capr1_ratio_2013'] = capr1_ratio_2013
    
    return data



def process_dataframe(df, columns, threshold):
    # Create a new column 'treated' with default value 0
    df['treated'] = 0

    # Iterate over each row in the dataframe
    for index, row in df.iterrows():
        # Check if any of the predefined columns is below the threshold
        if any(row[col] < threshold for col in columns):
            # Set 'treated' value to 1
            df.at[index, 'treated'] = 1
    
    return df


def create_pivot(data, value):
    data_pivot = pd.pivot_table(data, values=value, index="gvkey", columns='fyear')
    # Reset index to make gvkey a regular column
    data_pivot = data_pivot.reset_index()
    # Rename the columns with 'capr1_' prefix
    data_pivot.columns = ['gvkey'] + [str(value) + "_" + str(year) for year in data_pivot.columns[1:]]
    #banks_data_pivot = process_dataframe(df = banks_data_pivot, columns =["capr1_2007", "capr1_2008", "capr1_2009" ] , threshold = 10)
    return data_pivot

# load in the data 
banks_data = pd.read_csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/banks_data/compustat_yearly_banks.csv",)
banks_data_matching_table = pd.read_excel("/Users/luisenriquekaiser/Documents/Master Thesis/Data/Regulatory_data/Linking_Documents/Dealscan_Lender_Link.xlsx", sheet_name="Compustat")


# clean the data
banks_data = banks_data.dropna(subset=['capr1'])
banks_data = banks_data[banks_data["fic"] == "USA"]
# matching table
banks_data_matching_table_dict = banks_data_matching_table.set_index(['gvkey'])["lcoid"].to_dict()
banks_data['lcoid_banks'] = banks_data['gvkey'].map(banks_data_matching_table_dict)
# Create a pivot table to reshape the data
banks_data_pivot = pd.pivot_table(banks_data, values='capr1', index='gvkey', columns='fyear')
# Reset index to make gvkey a regular column
banks_data_pivot = banks_data_pivot.reset_index()
# Rename the columns with 'capr1_' prefix
banks_data_pivot.columns = ['gvkey'] + ['capr1_' + str(year) for year in banks_data_pivot.columns[1:]]
banks_data_pivot = process_dataframe(df = banks_data_pivot, columns =["capr1_2007", "capr1_2008", "capr1_2009" ] , threshold = 10)

banks_data_pivot.to_csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/pivot_banks.csv",)
banks_data.to_csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_data_processed.csv",)

# create pivot for total assets
banks_data_assets_pivot = create_pivot(data=banks_data, value="at")
banks_data_assets_pivot.to_csv("/Users/luisenriquekaiser/Documents/Master Thesis/Data/processed_data/banks_assets_total_pivot.csv",)
