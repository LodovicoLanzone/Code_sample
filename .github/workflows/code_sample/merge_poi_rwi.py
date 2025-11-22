#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import pyogrio
import seaborn as sns
import numpy as np
import os
import re

# Set the paths to amenity counts and RWI data

path_amenity  = r"G:0806filtered_all_radius_w_municipalilities_gemeindereferenz.csv"
rwi_path = r"rwi2021.gpkg"

# Call the amenity counts and RWI data
amenity_counts = pd.read_csv(path)
amenity_counts = amenity.drop_duplicates(subset=['id'])

rwi = gpd.read_file(rwi_path, engine = 'pyogrio')
rwi = rwi.to_crs('EPSG:3035')
# Merge the Data Frames based on 'id' column, keep only RWI keys
rwi_ext = pd.merge(rwi , radious_counts, how ='left', 
                    on= 'id')

# Create a dictionary to rename the RWI variables 
rename_dic = {'r1_mba_a_haeuser':'house', 'r1_mba_a_haushalt':'hh' , 'r1_mba_a_gewerbe': 'house_com' , 'r1_mba_a_wohngeb' : 'house_priv', 'r1_kkr_w_summe': 'pp',
              'r1_mbe_p_haustyp_1': 'house_type_1' ,'r1_mbe_p_haustyp_2': 'house_type_2' ,'r1_mbe_p_haustyp_3': 'house_type_3' ,'r1_mbe_p_haustyp_4': 'house_type_4',
              'r1_mbe_p_haustyp_5': 'house_type_5' ,'r1_mbe_p_haustyp_6': 'house_type_6' ,'r1_mbe_p_haustyp_7': 'house_type_7' ,'r1_mso_p_ausland' : 'foreign_hh', 
              'r1_mso_p_singles':'single_hh', 'r1_mso_p_paare':'couple_hh', 'r1_mso_p_familien' : 'family_hh', 'r1_mso_w_kinder':'child_per_hh', 'r1_alq_p_quote':'unemp', 
              'r1_eag_p_m00bis03' : 'm00', 'r1_eag_p_m03bis06' : 'm03', 'r1_eag_p_m06bis10' : 'm06', 'r1_eag_p_m10bis15' : 'm10', 'r1_eag_p_m15bis18' : 'm15', 'r1_eag_p_m18bis20' : 'm18',
              'r1_eag_p_m20bis25' : 'm20', 'r1_eag_p_m25bis30' : 'm25', 'r1_eag_p_m30bis35' : 'm30', 'r1_eag_p_m35bis40' : 'm35', 'r1_eag_p_m40bis45' : 'm40', 'r1_eag_p_m45bis50' : 'm45',
              'r1_eag_p_m50bis55' : 'm50', 'r1_eag_p_m55bis60' : 'm55', 'r1_eag_p_m60bis65' : 'm60', 'r1_eag_p_m65bis75' : 'm65',  'r1_eag_p_m75undgr' : 'm75', 
              'r1_eag_p_w00bis03' : 'w00', 'r1_eag_p_w03bis06' : 'w03', 'r1_eag_p_w06bis10' : 'w06', 'r1_eag_p_w10bis15' : 'w10', 'r1_eag_p_w15bis18' : 'w15', 'r1_eag_p_w18bis20' : 'w18',
              'r1_eag_p_w20bis25' : 'w20', 'r1_eag_p_w25bis30' : 'w25', 'r1_eag_p_w30bis35' : 'w30', 'r1_eag_p_w35bis40' : 'w35', 'r1_eag_p_w40bis45' : 'w40', 'r1_eag_p_w45bis50' : 'w45',
              'r1_eag_p_w50bis55' : 'w50', 'r1_eag_p_w55bis60' : 'w55', 'r1_eag_p_w60bis65' : 'w60', 'r1_eag_p_w65bis75' : 'w65',  'r1_eag_p_w75undgr' : 'w75', 'r1_met_p_deutschl' : 'german',
              'r1_met_p_italien' : 'italian', 'r1_met_p_tuerkei' : 'turkish', 'r1_met_p_griechen' : 'greek', 'r1_met_p_spanport' : 'spainport', 'r1_met_p_balkan' : 'balkan', 'r1_met_p_osteurop' : 'easteu',
              'r1_met_p_afrika' : 'africa', 'r1_met_p_islam' : 'islamic', 'r1_met_p_asien' : 'asian', 'r1_met_p_uebrige' : 'other_ethn', 'r1_met_p_spaetaus' : 'lateemig', 'r1_ewa_a_gesamt' : 'pop',
              'r1_mri_p_risiko_6' : 'risk_avg'}

# Use the dictionary to rename the merged Data Frame
rwi_ext = rwi_ext.rename(columns = rename_dic)             
              
# De-Select columns that starts with 'r1', which will leave only the columns of interest
rwi_ext = rwi_ext.loc[:, ~rwi_ext.columns.str.startswith('r1')]  


# Create binary variable columns for each amenity type indicating the positive counts, i.e. takes the value of 1 if the observation is positive and 0 otherwise
columns_to_check = {
    'road_length': 'tpm_road',
    'ps_count': 'tpm_ps',
    'edu_count': 'tpm_edu',
    'park_common_count': 'tpm_park',
    'pec_count': 'tpm_pec',
    'sports_count': 'tpm_sports',
    'pt_count': 'tpm_pt',
    'cyc_count': 'tpm_cyc',
    'town_count': 'tpm_town'
}

for column, tpm_column in columns_to_check.items():
    rwi_ext.loc[:, tpm_column] = rwi_ext[column].apply(lambda x: 0 if x == 0 else 1)

# Merge age groups to get aggregate gender shares
rwi_ext['m018'] = rwi_ext[['m00', 'm03', 'm06', 'm10', 'm15']].sum(axis=1)
rwi_ext['w018'] = rwi_ext[['w00', 'w03', 'w06', 'w10', 'w15']].sum(axis=1)

rwi_ext['m1865'] = rwi_ext[['m18', 'm20', 'm25', 'm30', 'm35',
       'm40', 'm45', 'm50', 'm55', 'm60']].sum(axis=1)
rwi_ext['w1865'] = rwi_ext[['w18', 'w20', 'w25', 'w30', 'w35',
       'w40', 'w45', 'w50', 'w55', 'w60']].sum(axis=1)

rwi_ext['m65'] = rwi_ext[['m65', 'm75']].sum(axis=1)
rwi_ext['w65'] = rwi_ext[['w65', 'w75']].sum(axis=1)

rwi_ext['m'] = rwi_ext[['m018','m1865', 'm65']].sum(axis=1)
rwi_ext['w'] = rwi_ext[['w018','w1865', 'w65']].sum(axis=1)


# Merge different age groups' shares to create 0-18, 18-65, 65+ age groups
rwi_ext['share_018'] = rwi_ext[['m018','w018']].sum(axis=1)
rwi_ext['share_1865'] = rwi_ext[['m1865','w1865']].sum(axis=1)
rwi_ext['share_65plus'] = rwi_ext[['m65','w65']].sum(axis=1)

# Number of people between 0 and 18 
rwi_ext['pop018'] = rwi_ext['share_018'] * rwi_ext['pop']
# Population per household and ensure that it is treated as number
rwi_ext['pop_per_hh'] = rwi_ext['pop'] / rwi_ext['hh']
rwi_ext['pp'] = rwi_ext['pp'].astype('float')

# Calculate the share of residents with non-German background
rwi_ext['non_ger'] = 100 - rwi_ext['german']

# Convert logical variable to numric
rwi_ext['DDR'] = rwi_ext['DDR'].apply(lambda x: 0 if x else 1).astype(int)


# Exclude the anonymized grid cells 
rwi_ext_excl_zero = rwi_ext[rwi_ext['pp'] != -1]
rwi_ext1 = rwi_ext_excl_zero


# Choose the columns that will be normalized by the population
columns_radius =  ['road_length', 'ps_count', 'edu_count', 'pec_count', 'park_common_count',
       'sports_count', 'pt_count', 'cyc_count', 'town_count']


# Selecting columns to divide by the population column
columns_to_divide = rwi_ext1[columns_radius]

# Performing division to get per capita values
per_capita_values = columns_to_divide.div(rwi_ext1['pop']/1000, axis=0)

# Creating new column labels with the suffix '_per_capita'
new_column_labels = [f'{column}_per_capita' for column in columns_radius]

# Assigning the new per capita columns back into the original DataFrame
rwi_ext1[new_column_labels] = per_capita_values

# Calculate per capita purchasing power
rwi_ext1['pp_per_capita'] = (rwi_ext1['pp']) / rwi_ext1['pop']


# Pick columns to check the correlation
columns_correlation = ['pop','pp_per_capita', 'road_length_per_capita', 
       'ps_count_per_capita', 'edu_count_per_capita', 'park_common_count_per_capita',
       'pec_count_per_capita', 'sports_count_per_capita',  'pt_count_per_capita', 
       'cyc_count_per_capita', 'town_count_per_capita']

# Pick columns to be transferred for the regression analysis
columns_correlation_ags = ['id','AGS','GEN','road_length', 'ps_count', 'edu_count', 'pec_count', 'park_common_count',
       'sports_count', 'pt_count', 'cyc_count', 'town_count','ur_binary','pop','pp_per_capita', 'road_length_per_capita', 
       'ps_count_per_capita', 'edu_count_per_capita', 'park_common_count_per_capita',
       'pec_count_per_capita', 'sports_count_per_capita',  'pt_count_per_capita', 
       'cyc_count_per_capita', 'town_count_per_capita', 
        'foreign_hh', 'single_hh', 'couple_hh', 'family_hh', 'child_per_hh', 'unemp', 'german',
        'non_ger', 'risk_avg', 'm', 'w', 'share_018', 'share_1865', 'share_65plus', 'pop_per_hh',
        'DDR', 'tpm_road', 'tpm_ps', 'tpm_edu', 'tpm_park', 'tpm_pec', 'tpm_sports', 'tpm_pt', 'tpm_cyc', 'tpm_town']

# Select columns for correlation
pc1 = rwi_ext1[columns_correlation]


# Plot the correlation map
fig, ax  = plt.subplots(1,1,figsize= (10,10))
sns.heatmap(pc1.corr(method='pearson'),annot=True , ax=ax)
# Optional save the figure - fig.savefig(r'2105_grid_level_per_capita_correlation.jpg' , dpi = 300)

# Select columns for the regression analysis, add leading zeros to AGS codes.
pc1_ags = rwi_ext1[columns_correlation_ags]
pc1_ags_1 = pc1_ags
pc1_ags_1 = pc1_ags_1.dropna()
pc1_ags_1['AGS'] = pc1_ags_1['AGS'].astype('int')
pc1_ags_1['AGS'] = pc1_ags_1['AGS'].astype('str')
def add_leading_zero(ags):
    if len(ags) == 7:
        return '0' + ags
    return ags
pc1_ags_1['AGS'] = pc1_ags_1['AGS'].apply(add_leading_zero)

# Save the Data Frame
pc1_ags_1.to_csv(r"pc1_thousand2.csv", index = False, header = True)