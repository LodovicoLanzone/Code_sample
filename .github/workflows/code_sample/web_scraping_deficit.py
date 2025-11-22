#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 28 15:10:16 2025

@author: lodovico
"""

pip install requests beautifulsoup4 lxml

import requests
from bs4 import BeautifulSoup
import pandas as pd


#put together the code datasets
piemonte = gpd.read_file("piemonte.xlsx")
aosta= gpd.read_file("aosta.xlsx")
lombardia = gpd.read_file("lombardia.xlsx")
veneto = gpd.read_file("veneto.xlsx")
trentino = gpd.read_file("trentino.xlsx")
friuli = gpd.read_file("friuli.xlsx")
emilia = gpd.read_file("emilia.xlsx")
liguria = gpd.read_file("liguria.xlsx")
toscana = gpd.read_file("toscana.xlsx")
marche = gpd.read_file("marche.xlsx")
umbria = gpd.read_file("umbria.xlsx")
lazio = gpd.read_file("lazio.xlsx")
abruzzo = gpd.read_file("abruzzo.xlsx")
molise = gpd.read_file("molise.xlsx")
campania = gpd.read_file("campania.xlsx")
basilicata = gpd.read_file("basilicata.xlsx")
puglia = gpd.read_file("puglia.xlsx")
calabria = gpd.read_file("calabria.xlsx")
sicilia = gpd.read_file("sicilia.xlsx")
sardegna = gpd.read_file("sardegna.xlsx")


dataframes = [piemonte, aosta, lombardia, veneto, trentino, friuli, emilia, liguria, toscana, marche, umbria, lazio, abruzzo, molise, campania, basilicata, puglia, calabria, sicilia, sardegna]
cleaned_dfs = []
for df in dataframes:
    # Check if first row looks like a header accidentally (i.e. column names are in row 0)
    if not str(df.iloc[0, 0]).isdigit():
        # No numeric code in first row → treat it as header row and re-read
        df.columns = df.iloc[0]
        df = df[1:].reset_index(drop=True)
    # Take first two columns
    subset = df.iloc[:, :2].copy()
    # Rename columns consistently
    subset.columns = ['code', 'municipality']
    cleaned_dfs.append(subset)
# Concatenate all
final_df = pd.concat(cleaned_dfs, ignore_index=True)
final_df['code'] = pd.to_numeric(final_df['code'], errors='coerce').astype('Int64')
final_df['code'] = final_df['code'].astype(str)

final_df.to_csv("indices.csv", index=False)

#automatize
import requests
from bs4 import BeautifulSoup
import pandas as pd
import geopandas as gpd
from concurrent.futures import ThreadPoolExecutor, as_completed
import time

final_df = pd.read_csv("indices.csv", dtype={0: str})
ente_codes = final_df.iloc[:, 0].dropna().astype(str).tolist()


scrape_config = {
    "02": {  # Income
        0: [2, 7, 13],
        1: [3],
        4: [2],
        5: [2],
    },
    "03": {  # Expenditures
        0: [1, 2, 3, 5, 6],
    }
}
def scrape_codice_ente(codice_ente, max_retries=3):
    base_url = 'https://finanzalocale.interno.gov.it/apps/floc.php/certificati/index'
    row_data = {"codice_ente": codice_ente}

    for quadro, table_row_map in scrape_config.items():
        url = f"{base_url}/codice_ente/{codice_ente}/cod/4/anno/1998/md/0/cod_modello/CCOU/tipo_modello/U/cod_quadro/{quadro}"

        # Retry mechanism
        for attempt in range(max_retries):
            try:
                response = requests.get(url, timeout=10)
                if response.status_code != 200:
                    raise Exception(f"HTTP {response.status_code}")
                soup = BeautifulSoup(response.content, 'html.parser')
                break
            except Exception as e:
                print(f"Error for {codice_ente} quadro {quadro} attempt {attempt+1}: {e}")
                time.sleep(2)
        else:
            print(f"Failed to fetch {url} after {max_retries} attempts, skipping.")
            continue

        table_divs = soup.find_all(class_='table-responsive')

        for table_index, target_rows in table_row_map.items():
            if table_index >= len(table_divs):
                for row_index in target_rows:
                    colname = f"{quadro}_{table_index}_{row_index}"
                    row_data[colname] = pd.NA
                continue

            table = table_divs[table_index].find('table')
            if not table:
                for row_index in target_rows:
                    colname = f"{quadro}_{table_index}_{row_index}"
                    row_data[colname] = pd.NA
                continue

            rows = table.find_all('tr')

            for row_index in target_rows:
                colname = f"{quadro}_{table_index}_{row_index}"

                if row_index >= len(rows):
                    row_data[colname] = pd.NA
                    continue

                row = rows[row_index]
                cols = row.find_all('td')

                if len(cols) < 2:
                    row_data[colname] = pd.NA
                    continue

                # Keep only the value from the second column
                row_data[colname] = cols[1].get_text(strip=True)

    print(f"Finished scraping codice_ente {codice_ente}")
    return row_data

def scrape_all_codes(ente_codes, max_workers=5):
    results = []
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(scrape_codice_ente, code): code for code in ente_codes}
        for future in as_completed(futures):
            code = futures[future]
            try:
                data = future.result()
                results.append(data)
            except Exception as exc:
                print(f"Exception for codice_ente {code}: {exc}")
    return pd.DataFrame(results)

# === Usage ===
if __name__ == "__main__":
    df = scrape_all_codes(ente_codes, max_workers=5)
    df.to_csv("debt.csv", index=False)
    print(df.head())

