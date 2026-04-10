################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS US FISH AND WILDLIFE CRITICAL HABITAT DATA TO##
# A DISTANCE TO CRITICAL HABITAT METRIC AT 3KM RESOLUTION                     ##
# 1. Load the data (this is a shapefile)                                      ##
#  1.1 Check that the geometries are valid and not empty                      ##
# 2. Rasterize the data                                                       ##
#  2.1 Create an empty raster grid for the data                               ##
# 3. Calculate the distance to critical habitat                               ## 
# 4. Crop the distance raster to the reference raster and rename the variable ##
# 5. Save the raster                                                          ##   
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(stringr)
library(sf)
library(terra)
library(tidyverse)
library(spdep)
library(gstat)
library(stars)

# Set the projection
projection <- "epsg:5070"

# Load the reference raster
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif")

# 1. Load the data Critical Habitat shapefile 
#-------------------------------------------------------------------------------
# Can be downloaded from https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip
crithab <- st_read("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/original/crithab/crithab_poly.shp")

## 1.1 check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(crithab)))
  crithab <- st_make_valid(crithab)

crithab <- crithab %>%
  filter(!st_is_empty(.))

crithab_proj <- crithab %>%
  st_transform(projection)

# 2. Rasterize the data
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

# Rasterize the critical habitat areas shapefile
crithab_rast <- rasterize(vect(crithab_proj), templateRas)

# 3. Calculate the distance from wilderness areas
#-------------------------------------------------------------------------------
# Use terra::distance() to calculate the distance to critical habitat 
crithab_dist_rast <- terra::distance(crithab_rast)


# 4. Crop the distance raster to the reference raster and rename the variable
#-------------------------------------------------------------------------------
crithab_dist_crop <- crop(crithab_dist_rast, ref_rast_proj, mask = TRUE)
plot(crithab_dist_crop) # quickly check the plot

## Rename the distance variable
names(crithab_dist_crop) <- "distance_to_crithab_m"

# 5. Save the raster
#-------------------------------------------------------------------------------
writeRaster(crithab_dist_crop, here::here(paste0("data/processed/variables/dist_to_crithab_3km_pred_crop_", 
                               Sys.Date(), ".tif")), overwrite = TRUE)


