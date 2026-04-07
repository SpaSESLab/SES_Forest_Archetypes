################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS Climate and Economic Justice Screening Tool  ##
# (CEJST) DATA TO 3KM                                                         ##
# THIS INCLUDES POP WITH LESS THAN HIGH SCHOOL EDUCATION, HOUSING BURDEN,     ##
# ENERGY BURDEN, AND FINE PARTICULATE MATTER (PM2.5). THIS IS A SHAPEFILE.    ##
# 1. Load the data                                                            ##
#  1.1 Filter for state in the contiguous United States and select variables  ##
#  1.2 Check that the new geometries are valid and not empty                  ##
# 2. Fill in the missing data                                                 ##
#  2.1 Create an empty raster with 3km resolution                             ##
#  2.2 Use a custom function to interpolate the missing data                  ##
#      Crop and mask the raster prediction to the ref_raster                  ##
# 3. Save the rasters                                                         ##
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
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif")

# Read in the custom functions
source(here::here("scripts/functions/data_processing_custom_functions.R"))

# 1. Load the data
#-------------------------------------------------------------------------------

cejst <- st_read("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/original/cejst/usa/usa.shp")

## 1.1
## Filter states for CONUS
filt_cejst <- cejst %>%
  filter(SF != c("Hawaii", "Alaska", "Puerto Rico",
                 "Northern Mariana Islands", "Guam", "American Samoa"))

## Select variables: less high school, housing burden, energy burden, and PM2.5
cejst_vars <- filt_cejst %>% 
  dplyr::select(geometry, HSEF, HBF_PFS, EBF_PFS, PM25F_PFS)

## 1.2
## check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(cejst_vars)))
  cejst_vars <- st_make_valid(cejst_vars)

cejst_vars <- cejst_vars %>%
  filter(!st_is_empty(.))

cejst_vars_proj <- cejst_vars %>%
  st_transform(projection)

# 2. Fill in missing data
#-------------------------------------------------------------------------------
## 2.1 Create a template raster for the shapefiles
XMIN <- ext(ref_rast_proj)$xmin
XMAX <- ext(ref_rast_proj)$xmax
YMIN <- ext(ref_rast_proj)$ymin
YMAX <- ext(ref_rast_proj)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast_proj))

grd <- st_as_stars(templateRas)

## 2.2 Use the idw_preds function to rasterize and fill in missing data using 
##     inverse distance weigthing (idw) and then crop to the reference raster

### Less high school
lesshs.preds <- idw_preds(cejst_vars_proj, templateRas, "HSEF", grd)
lesshs_crop <- crop(lesshs.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(lesshs_crop)

### Housing burden 
hsburd.preds <- idw_preds(cejst_vars_proj, templateRas, "HBF_PFS", grd)
houseburd_crop <- crop(hsburd.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(houseburd_crop)

### Energy burden 
engburd.preds <- idw_preds(cejst_vars_proj, templateRas, "EBF_PFS", grd)
engburd_crop <- crop(engburd.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(engburd_crop)

### PM2.5 exposure 
pm25.preds <- idw_preds(cejst_vars_proj, templateRas, "PM25F_PFS", grd)
pm25_crop <- crop(pm25.preds$pred.rst, ref_rast_proj, mask = TRUE)
plot(pm25_crop)

# 3. Save the rasters
#-------------------------------------------------------------------------------
writeRaster(lesshs_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/cejst_lesshs_3km_pred_crop_", 
                                Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(houseburd_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/cejst_houseburd_3km_pred_crop_", 
                                   Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(engburd_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/cejst_engburd_3km_pred_crop_", 
                                 Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(pm25_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/cejst_pm25_3km_pred_crop_", 
                              Sys.Date(), ".tif"), overwrite = TRUE)