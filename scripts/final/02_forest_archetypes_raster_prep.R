################################################################################
# SCRIPT TO PROCESS RASTER DATA (TREE COVER, TREE AGE, AND THE FED RICH) TO 3KM#
# 1. Load the data                                                            ##
# 2. For the tree cover. Reproject. Subset for layers/bands 254:255, resample ##
#    NOTE: 254 is the band with the % tree cover                              ##
# 3. For the stand age. Reproject, resample and fill missing data with 0      ##
#    (no trees, so no tree age)                                               ##
# 4. Save the rasters                                                         ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(haven)
library(tigris)
library(readxl)
library(spdep)
library(gstat)
library(stars)
library(here)

# Set the projection
projection <- "epsg:5070"

# Load the reference raster
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# 1. Load the data
#-------------------------------------------------------------------------------
tree_cover <- rast(here::here("data/original/tree_cover/nlcd_tcc_conus_2016_v2021-4.tif"))
tree_age <- rast(here::here("data/original/tree_age/conus_age06_1km.tif"))

# 2. For the tree cover. Reproject. Subset for layers/bands 254:255, resample
#-------------------------------------------------------------------------------
tree_cover_proj <- project(tree_cover, projection)
tree_cover_proj_subs <- subst(tree_cover_proj, 254:255, 0) 
tree_cover_proj_resamp_ave <- resample(tree_cover_proj_subs, ref_rast, "average", threads = TRUE)
tree_cover_proj_crop <- crop(tree_cover_proj_resamp_ave, ref_rast, mask = TRUE)
# quick check of plot
plot(tree_cover_proj_crop)

# 3. For the stand age. Reproject, resample and fill missing data with 0 
#    (no trees = no tree age)
#-------------------------------------------------------------------------------
tree_age_proj <- project(tree_age, projection)
tree_age_resamp <- resample(tree_age_proj, ref_rast, "bilinear")
tree_age_resamp[is.na(tree_age_resamp)] <- 0
tree_age_resamp_crop <- crop(tree_age_resamp, ref_rast, mask = TRUE)
# quick check of plot
plot(tree_age_resamp_crop)

# 4. Save the rasters
#-------------------------------------------------------------------------------
writeRaster(tree_cover_proj_crop, here::here(paste0("data/processed/variables/conus_tree_cover_crop_", 
                                         Sys.Date(), ".tif")))

writeRaster(tree_age_resamp_crop, here::here(paste0("data/processed/variables/conus_tree_age_crop_", 
                                         Sys.Date(), ".tif")))






