################################################################################
# SCRIPT TO CROP THE RASTER STACK TO THE NATIONAL FOREST + 50KM BUFFERS       ##
# 1. Load the UNSCALED full attribute raster stack, the USFS regional boundaries
#   and the national forest with buffers shapefile                            ##
#  1.1 Filter the regional boundaries (remove Region 10), reproject, and then ##
#      crop the boundaries to extent of the raster stack                      ##
#  1.2 For some reason the original nf_buffers shapefile is not cropped to Conus
#      Need to create a new national forest buffers shape using st_intersection#
#  2. Crop the raster to the nf_buffers                                       ##
#  2.1 Scale using global min-max.                                            ##
# 3. Save the cropped and the cropped then scaled raster                      ##
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(here)

## Set the projection
projection <- "epsg: 5070"

# 1. Load the attribute raster stack and forest + buffer shape file
#-------------------------------------------------------------------------------
## Attribute raster stack
df_attri <- rast("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/rast_stack_all_attributes_2024-10-08.tif")

## Load in forest with 50km buffer shape file
nf_sf <- read_sf(here::here("data/processed/nf_buffers_50k_2024-10-22.shp"))

## Load in regional USFS boundaries
fs_reg <- st_read("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/original/S_USA.AdministrativeRegion.shp")

## 1.1
## Filter out Region 10 and crop to the raster stack
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(df_attri))

## 1.2 
## For some reason the original nf_buffers shapefile is not cropped to Conus
## Need to create a new national forest buffers shape using st_intersection
nf_buffers <- st_intersection(nf_sf, fs_reg.crop)

# 2. Crop the raster to the nf_buffers. Then scale using global min-max.
#-------------------------------------------------------------------------------
attri_crop <- crop(df_attri, nf_buffers, mask = TRUE)

## 2.1
## Scale using global min-max
attri_crop_sc <- (attri_crop - global(attri_crop, "min", na.rm=TRUE)[,1])/(global(attri_crop, "max", na.rm=TRUE)[,1] - global(attri_crop, "min", na.rm=TRUE)[,1])
## Because this is cropped. I should look at the variance of the values. 
## the z-score may be appropriate once restricted to the forest buffers.
## UPDATE: I tested this as well as rank scale. Global is still the most 
## appropriate. 

# 3. Save for the cropped and the cropped than scaled raster stack
#-------------------------------------------------------------------------------
writeRaster(attri_crop, 
            filename = here::here(paste0("data/processed/nf_buffers_all_attributes_", 
                                                     Sys.Date(), ".tif")))

writeRaster(attri_crop_sc, 
            filename = here::here(paste0("data/processed/nf_buffers_all_attributes_cropped_then_scaled_", 
                                                        Sys.Date(), ".tif")), overwrite = TRUE)



