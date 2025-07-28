
library(sf)
library(dplyr)
library(readr)

# Step 1: Read and subset NYC GeoJSON shapefile
# ----------------------------------------

# Load the full New York State PUMA shapefile
ny_pumas <- st_read("NewYork2022.json")  # Make sure this file exists in your working directory

# Convert PUMA column to numeric for filtering
ny_pumas$PUMA <- as.numeric(as.character(ny_pumas$PUMA))

# Define the list of NYC PUMA codes (based on census definitions)
nyc_puma_list <- c(
  3701:3711,   # Bronx
  3801:3814,   # Brooklyn
  3901:3907,   # Manhattan
  4001:4019,   # Queens
  4101:4114    # Staten Island
)

# Filter NYC PUMAs only
nyc_geo <- ny_pumas %>%
  filter(PUMA %in% nyc_puma_list)

# Save NYC-specific GeoJSON file for mapping (optional)
st_write(nyc_geo, "nyc_only.geojson")  # You can upload this to Datawrapper or QGIS

# Step 2: Clean and prepare PUMS data
# ----------------------------------------

# Load PUMS data (assumed to be previously loaded as `pums`)

# Check PUMA10 variable characteristics (to ensure formatting consistency)
str(pums$PUMA10)
unique(nchar(as.character(pums$PUMA10)))

# Format and filter PUMS data to retain only NYC records
nyc_pumas_str <- as.character(c(3701:3711, 3801:3814, 3901:3907, 4001:4019, 4101:4114))

pums_cleaned <- pums %>%
  filter(!is