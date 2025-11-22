#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 22 13:15:33 2025

@author: lodovico
"""

import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import matplotlib.colors as mcolors
import matplotlib.ticker as mticker

# Load the shapefile
data_geo_fra = gpd.read_file("data_fra.gpkg")

# Create figure and axis
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
norm = mcolors.LogNorm(vmin=data_geo_fra["income_per_capita"].min(), vmax=data_geo_fra["income_per_capita"].max())
data_geo_fra.plot(column="income_per_capita", ax=ax, cmap="viridis", norm=norm)
sm = plt.cm.ScalarMappable(cmap="viridis", norm=norm)
cbar = fig.colorbar(sm, ax=ax)
num_ticks = 7  # Adjust to control how many labels appear
tick_values = np.geomspace(data_geo_fra["income_per_capita"].min(), data_geo_fra["income_per_capita"].max(), num_ticks)
cbar.set_ticks(tick_values)  # Set specific tick locations
cbar.set_ticklabels([f"{int(val):,}" for val in tick_values])
cbar.ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f"{int(x):,}"))
cbar.ax.yaxis.set_minor_formatter(mticker.NullFormatter())  # Remove minor ticks
ax.set_axis_off()
plt.savefig("income_fra.png", bbox_inches='tight', dpi=300)
plt.close()



#AMENITIES
def categorize_amenity(val):
    if val == 0:
        return "0"
    elif val == 1:
        return "1"
    elif 2 <= val <= 5:
        return "2–5"
    elif 6 <= val <= 10:
        return "6–10"
    else:
        return "10+"

data_geo_fra = data_geo_fra.copy()
data_geo_fra["ps_count_cat"] = data_geo_fra["ps_count"].apply(categorize_amenity)
data_geo_fra["edu_count_cat"] = data_geo_fra["edu_count"].apply(categorize_amenity)
data_geo_fra["pt_count_cat"] = data_geo_fra["pt_count"].apply(categorize_amenity)
data_geo_fra["park_common_count_cat"] = data_geo_fra["park_common_count"].apply(categorize_amenity)
data_geo_fra["pec_count_cat"] = data_geo_fra["pec_count"].apply(categorize_amenity)
data_geo_fra["sports_count_cat"] = data_geo_fra["sports_count"].apply(categorize_amenity)
data_geo_fra["cyc_count_cat"] = data_geo_fra["cyc_count"].apply(categorize_amenity)


# Step 2: Define category colors with desired styling
categories = ["0", "1", "2–5", "6–10", "10+"]
colors = ["#ffff00",  # bright yellow for 0
          "#ff0000",  # light red for 1
          "#8b0000",  # dark red for 2-5
          "#8000ff",  # purple for 6-10
          "#4b0082"]
cmap = mcolors.ListedColormap(colors)
bounds = np.arange(len(categories) + 1)
norm = mcolors.BoundaryNorm(boundaries=bounds, ncolors=len(colors))

# Step 3: Plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
data_geo_fra.plot(column='cyc_count_cat', ax=ax, cmap=cmap, linewidth=0.1, edgecolor='black', legend=False)
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=colors[i], edgecolor='black', label=categories[i]) for i in range(len(categories))]
ax.legend(handles=legend_elements, loc='center left', bbox_to_anchor=(1, 0.5), fontsize=10)
ax.set_axis_off()
plt.savefig("cyc_fra_map.png", bbox_inches='tight', dpi=300)
plt.close()



#PARIS
inner_suburbs_departments = ['75', '92', '93', '94']
data_paris = data_geo_fra[data_geo_fra["INSEE_DEP"].isin(["75", "92", "93", "94"])].copy()


# Create the plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
data_paris.plot(ax=ax, facecolor='white', edgecolor='red', linewidth=0.5)
ax.set_axis_off()  # Similar to theme_minimal()
plt.savefig("paris_grid.png", bbox_inches='tight', dpi=300)
plt.close()

#income/pop
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
norm = mcolors.LogNorm(vmin=data_paris["pop"].min(), vmax=data_paris["pop"].max())
data_paris.plot(column="pop", ax=ax, cmap="viridis", norm=norm)
sm = plt.cm.ScalarMappable(cmap="viridis", norm=norm)
cbar = fig.colorbar(sm, ax=ax)
num_ticks = 5  # Adjust to control how many labels appear
tick_values = np.geomspace(data_paris["pop"].min(), data_paris["pop"].max(), num_ticks)
cbar.set_ticks(tick_values)  # Set specific tick locations
cbar.set_ticklabels([f"{int(val):,}" for val in tick_values])
cbar.ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f"{int(x):,}"))
cbar.ax.yaxis.set_minor_formatter(mticker.NullFormatter())  # Remove minor ticks
ax.set_axis_off()
plt.savefig("paris_pop_map.png", bbox_inches='tight', dpi=300)
plt.close()

#Amenities for which i don't have to change the categorization: cyc, pec, ps
categories = ["0", "1", "2–5", "6–10", "10+"]
colors = [
    "#440154",  # Deep purple for 0
    "#31688e",  # Dark blue for 1
    "#87CEFA",  # Light blue for 2–5
    "#3CB371",  # Teal green for 6–10
    "#FFFF33"   # Bright yellow for 10+
]
cmap = mcolors.ListedColormap(colors)
bounds = np.arange(len(categories) + 1)
norm = mcolors.BoundaryNorm(boundaries=bounds, ncolors=len(colors))

# Step 3: Plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
data_paris.plot(column='ps_count_cat', ax=ax, cmap=cmap, linewidth=0.1, edgecolor='black', legend=False)
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=colors[i], edgecolor='black', label=categories[i]) for i in range(len(categories))]
ax.legend(handles=legend_elements, loc='center left', bbox_to_anchor=(1, 0.5), fontsize=10)
ax.set_axis_off()
plt.savefig("paris_ps_map.png", bbox_inches='tight', dpi=300)
plt.close()

#amenities with different categorization
def categorize_amenity(val):
        if val <= 5:
            return "<5"
        elif 5 < val <= 15:
            return "5-15"
        elif 15 < val <= 30:
            return "15–30"
        elif 30 < val <= 60:
            return "30–60"
        else:
            return "60+"
data_paris.loc[:, "park_common_count_cat"] = data_paris["park_common_count"].apply(categorize_amenity)
categories = ["<5", "5-15", "15-30", "30-60", "60+"]
colors = [
    "#440154",  # Deep purple for 0
    "#31688e",  # Dark blue for 1
    "#87CEFA",  # Light blue for 2–5
    "#3CB371",  # Teal green for 6–10
    "#FFFF33"   # Bright yellow for 10+
]
cmap = mcolors.ListedColormap(colors)
bounds = np.arange(len(categories) + 1)
norm = mcolors.BoundaryNorm(boundaries=bounds, ncolors=len(colors))
# Step 3: Plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
data_paris.plot(column="park_common_count_cat", ax=ax, cmap=cmap, linewidth=0.1, edgecolor='black', legend=False)
# Step 4: Custom legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=colors[i], edgecolor='black', label=categories[i]) for i in range(len(categories))]
ax.legend(handles=legend_elements, loc='center left', bbox_to_anchor=(1, 0.5), fontsize=10)
ax.set_axis_off()
plt.savefig("paris_parks_map.png", bbox_inches='tight', dpi=300)
plt.close()

def categorize_amenity(val):
        if val <= 100:
            return "<100"
        elif 100 < val <= 200:
            return "100-200"
        elif 200 < val <= 300:
            return "200–300"
        elif 300 < val <= 400:
            return "300–400"
        else:
            return "400+"
data_paris.loc[:, "pedy_count_cat"] = data_paris["edu_count"].apply(categorize_amenity)
categories = ["<100", "100-200", "80-150", "150-300", "300+"]
colors = [
    "#440154",  # Deep purple for 0
    "#31688e",  # Dark blue for 1
    "#87CEFA",  # Light blue for 2–5
    "#3CB371",  # Teal green for 6–10
    "#FFFF33"   # Bright yellow for 10+
]
cmap = mcolors.ListedColormap(colors)
bounds = np.arange(len(categories) + 1)
norm = mcolors.BoundaryNorm(boundaries=bounds, ncolors=len(colors))
# Step 3: Plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
data_paris.plot(column="edu_count_cat", ax=ax, cmap=cmap, linewidth=0.1, edgecolor='black', legend=False)
# Step 4: Custom legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=colors[i], edgecolor='black', label=categories[i]) for i in range(len(categories))]
ax.legend(handles=legend_elements, loc='center left', bbox_to_anchor=(1, 0.5), fontsize=10)
ax.set_axis_off()
plt.savefig("paris_edu_map.png", bbox_inches='tight', dpi=300)
plt.close()

###############################
#alternative with rasterization
###############################
import numpy as np
import numpy.ma as ma
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.ticker as mticker
import rasterio.features
from rasterio.transform import from_origin

res = 750  # meters per pixel, adjust as needed
bounds = data_paris.total_bounds  # [minx, miny, maxx, maxy]
width = int((bounds[2] - bounds[0]) / res)
height = int((bounds[3] - bounds[1]) / res)
transform = from_origin(bounds[0], bounds[3], res, res)
raster = rasterio.features.rasterize(
    ((geom, value) for geom, value in zip(data_paris.geometry, data_paris["sports_count_con"])),
    out_shape=(height, width),
    transform=transform,
    fill=0,
    dtype="float32",
    all_touched=True  # <---- this helps fill pixels touched by polygons
)
viridis = plt.cm.get_cmap("viridis", 256)
newcolors = viridis(np.linspace(0, 1, 256))
white = np.array([1, 1, 1, 1])  # RGBA for white
newcolors[0] = white
new_cmap = mcolors.ListedColormap(newcolors)
norm = mcolors.Normalize(vmin=data_paris["sports_count_con"].min(), vmax=data_paris["sports_count_con"].max())
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
im = ax.imshow(raster, cmap=new_cmap, norm=norm, origin="upper")
sm = plt.cm.ScalarMappable(cmap=new_cmap, norm=norm)
sm.set_array([])
cbar = fig.colorbar(sm, ax=ax)
num_ticks = 7
tick_values = np.linspace(data_paris["sports_count_con"].min(), data_paris["sports_count_con"].max(), num_ticks)
cbar.set_ticks(tick_values)
cbar.set_ticklabels([f"{int(val):,} +" if i == num_ticks else f"{int(val):,}" for i, val in enumerate(tick_values)])
max_val = data_paris["sports_count_con"].max()
cbar.ax.yaxis.set_major_formatter(mticker.FuncFormatter(
    lambda x, _: f"{int(x):,}+" if x == max_val else f"{int(x):,}"
))
cbar.ax.yaxis.set_minor_formatter(mticker.NullFormatter())
ax.set_axis_off()
plt.savefig("paris_cyc_map.png", bbox_inches='tight', dpi=300)
plt.close()

#amenities
def categorize_continuous(val):
    if val == 0:
        return 0.1
    elif val <= 10:
        return val
    else:
        return min(val, 12)
    
def categorize_continuous2(val):
    if val == 0:
        return 0.1
    elif val <= 10:
        return val
    else:
        return min(val, 320)

def categorize_continuous3(val):
    if val == 0:
        return 0.1
    elif val <= 10:
        return val
    else:
        return min(val, 420)

def categorize_continuous4(val):
    if val == 0:
        return 0.1
    elif val <= 10:
        return val
    else:
        return min(val, 90)

def categorize_continuous5(val):
    if val == 0:
        return 0.1
    elif val <= 10:
        return val
    else:
        return min(val, 65)

        
data_paris["ps_count_con"] = data_paris["ps_count"].apply(categorize_continuous)
data_paris["edu_count_con"] = data_paris["edu_count"].apply(categorize_continuous2)
data_paris["pt_count_con"] = data_paris["pt_count"].apply(categorize_continuous3)
data_paris["park_common_count_con"] = data_paris["park_common_count"].apply(categorize_continuous5)
data_paris["pec_count_con"] = data_paris["pec_count"].apply(categorize_continuous)
data_paris["sports_count_con"] = data_paris["sports_count"].apply(categorize_continuous4)
data_paris["cyc_count_con"] = data_paris["cyc_count"].apply(categorize_continuous)
#use them in the previous code


#for cyc, that was problematic with the other coding
res = 750  # meters per pixel, adjust as needed
bounds = data_paris.total_bounds  # [minx, miny, maxx, maxy]
width = int((bounds[2] - bounds[0]) / res)
height = int((bounds[3] - bounds[1]) / res)
transform = from_origin(bounds[0], bounds[3], res, res)
raster = rasterio.features.rasterize(
    ((geom, value) for geom, value in zip(data_paris.geometry, data_paris["ps_count_con"])),
    out_shape=(height, width),
    transform=transform,
    fill=0,
    dtype="float32",
    all_touched=True  # <---- this helps fill pixels touched by polygons
)
viridis = plt.cm.get_cmap("viridis", 256)
new_cmap = viridis
masked_raster = ma.masked_where(raster == 0, raster)
tick_values = [0.1, 1, 2, 3, 4]
max_val = max(tick_values)

# Set norm with max_val to ensure top of legend matches top tick
norm = mcolors.Normalize(vmin=data_paris["cyc_count_con"].min(), vmax=max_val)

# Mask raster where value == 0
masked_raster = ma.masked_where(raster == 0, raster)

# Plot
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
im = ax.imshow(masked_raster, cmap=new_cmap, norm=norm, origin="upper")
sm = plt.cm.ScalarMappable(cmap=new_cmap, norm=norm)
sm.set_array([])
cbar = fig.colorbar(sm, ax=ax)
cbar.set_ticks(tick_values)
cbar.ax.yaxis.set_major_formatter(mticker.FuncFormatter(
    lambda x, _: f"{int(x):,}+" if np.isclose(x, max_val) else f"{int(x):,}"
))
cbar.ax.yaxis.set_minor_formatter(mticker.NullFormatter())

ax.set_axis_off()
plt.savefig("paris_cyc_map.png", bbox_inches='tight', dpi=300)
plt.close()