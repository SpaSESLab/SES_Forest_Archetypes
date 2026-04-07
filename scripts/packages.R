# =============================================================
# Install Required Packages
# =============================================================

# STEP 1: Specify Required Packages
# -------------------------------------------------------------
# Below is the list of required packages for running all of the scripts....

required_packages <- c(
  "tidyverse",   # For...is it redundant to have this and dplyr and stringr?
  "dplyr",       # For data manipulation
  "sf",          # For working with simple features and vector data
  "raster",      # For working with rasters
  "terra",       # Slightly faster processing and more flexible than raster
  "sdped",       # For For creating spatial weights matrix objects
  "stars",       # For working with spatial-temporal arrays (data cubes)
  "tigris",      # For downloading state, county, and census tract boundaries
  "stringr",     # For working with strings
  "Rcurl",       # For writing general HTTP requests
  "gstat",       # For geostatistical modeling
  "ggplot2",     # For data visualization
  "here",        # For constructing relative file paths
  "rFIA",        # For working with Forest Inventory and Analysis (FIA) data
  "haven",       # For reading in SPSS, Strata, or SAS files
  "readxl",      # For reading in Excel files
  "geodata",     # For downloading geographic data, specifically the WorldClim,
                 # travel time, and elevation data
  "dismo",       # For calculating the biovars from the WorldClim data
  "purrr",       # For functional programming
  "progress",    # For configuring and utilizing progress bars
  "patchwork",   # For viz...
  "MetBrewer",   # For viz...
  "ggcharts",    # For viz...
  "gghighlight", # For viz...
  "geocmeans"    # For running fuzzy c-means models
)

# STEP 2: Install Required Packages
# -------------------------------------------------------------
# Install the packages listed in `required_packages`.
install.packages(required_packages)