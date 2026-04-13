################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS US FOREST SERVICE WILDERNESS AREA DATA TO    ##
# A DISTANCE TO WILDERNESS AREA METRIC AT 3KM RESOLUTION                      ##
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
library(haven)
library(tigris)
library(readxl)
library(spdep)
library(gstat)
library(stars)

# Set the projection
projection <- "epsg:5070"

# Load the reference raster
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# 1. Load the Wilderness Areas shapefile
#-------------------------------------------------------------------------------
# Can be downloaded from https://data.fs.usda.gov/geodata/edw/edw_resources/shp/BdyDesg_LSRS_Wilderness.zip
wild <- st_read("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/original/distwild/S_USA.Wilderness.shp")

## 1.1 check for validity, remove empty geometries, and reproject 
if (!all(st_is_valid(wild)))
  wild <- st_make_valid(wild)

wild <- wild %>%
  filter(!st_is_empty(.))

wild_proj <- wild %>%
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

# Rasterize the wilderness areas shapefile
wild_rast <- rasterize(vect(wild_proj), templateRas)

# 3. Calculate the distance from wilderness areas
#-------------------------------------------------------------------------------
# Use terra::distance() to calculate the distance from wilderness areas
wild_dist_rast <- terra::distance(wild_rast)

# 4. Crop the distance raster to the reference raster and rename the variable
#-------------------------------------------------------------------------------
wild_dist_crop <- crop(wild_dist_rast, ref_rast_proj, mask = TRUE)
plot(wild_dist_crop)# quickly check the plot

## Rename the distance variable
names(wild_dist_crop) <- "distance_to_wilderness_m"

# 5. Save the raster
#-------------------------------------------------------------------------------
writeRaster(wild_dist_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/dist_to_wild_3km_pred_crop_", 
                                 Sys.Date(), ".tif"), overwrite = TRUE)


