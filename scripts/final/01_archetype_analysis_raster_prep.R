################################################################################
# SCRIPT TO PROCESS RASTER DATA (TREE COVER, TREE AGE, AND THE FED RICH) TO 3KM#
# 1. Load the data                                                            ##
# 2. For fed_rich fill NAs with 0, then aling and crop to reference raster    ##
# 3. For the tree cover. Reproject. Subset for layers/bands 254:255, resample ##
# 4. For the stand age. Reproject, resample and fill missing data with 0      ##
#    (no trees, so no tree age)                                               ##
# 5. Save the rasters                                                         ##
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

# Set the projection
projection <- "epsg:5070"

# Load the reference raster
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/merged/conus_whp_3km_agg_interp_crop_2024-09-27.tif")

# 1. Load the data
#-------------------------------------------------------------------------------
fed_rich <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_2024-06-12.tif")
# fed rich from ~/Analysis/Archetype_Analysis/scripts/test/00_test_area_rich_fed_padus.R
tree_cover <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/nlcd_tcc_conus_2016_v2021-4.tif")
tree_age <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/NA_TreeAge_1096/data/conus_age06_1km.tif")

# 2. For fed_rich fill NAs with 0, then aling and crop to reference raster
#-------------------------------------------------------------------------------
fed_rich[is.na(fed_rich)] <- 0
fed_rich_align <- resample(fed_rich, ref_rast_proj, "near")
fed_rich_crop <- crop(fed_rich_align, ref_rast_proj, mask = TRUE)

# 3. For the tree cover. Reproject. Subset for layers/bands 254:255, resample
#-------------------------------------------------------------------------------
tree_cover_proj <- project(tree_cover, projection)
tree_cover_proj_subs <- subst(tree_cover_proj, 254:255, 0)
tree_cover_proj_resamp_ave <- resample(tree_cover_proj_subs, ref_rast_proj, "average", threads = TRUE)
#tree_cover_proj_resamp_ave[is.na(tree_cover_proj_resamp_ave)] <- 0
tree_cover_proj_crop <- crop(tree_cover_proj_resamp_ave, ref_rast_proj, mask = TRUE)
# quick check of plot
plot(tree_cover_proj_crop)

# 4. For the stand age. Reproject, resample and fill missing data with 0 (no trees so no tree age)
#-------------------------------------------------------------------------------
tree_age_proj <- project(tree_age, projection)
tree_age_resamp <- resample(tree_age_proj, ref_rast_proj, "bilinear")
tree_age_resamp[is.na(tree_age_resamp)] <- 0
tree_age_resamp_crop <- crop(tree_age_resamp, ref_rast_proj, mask = TRUE)
# quick check of plot
plot(tree_age_resamp_crop)

# 5. Save the rasters
#-------------------------------------------------------------------------------
writeRaster(fed_rich_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_fed_rich_crop_", 
                                  Sys.Date(), ".tif"))

writeRaster(tree_cover_proj_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_", 
                                         Sys.Date(), ".tif"))

writeRaster(tree_cover_proj_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/conus_tree_cover_crop_", 
                                         Sys.Date(), ".tif"))

# quick check that all rasters have the same number of rows as the ref raster
nrow(as.data.frame(ref_rast))
nrow(as.data.frame(tree_age_resamp_crop))
nrow(as.data.frame(tree_cover_proj_resamp_ave))
nrow(as.data.frame(tree_cover_proj_crop))





