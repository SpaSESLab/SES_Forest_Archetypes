################################################################################
# SCRIPT TO CROP THE RASTER STACK TO THE NATIONAL FOREST + 50KM BUFFERS       ##
# 1. Load the UNSCALED full attribute raster stack and the USFS regional      ##
#   boundaries                                                                ##
#  1.1 Filter the regional boundaries (remove Region 10), reproject, and then ##
#      crop the boundaries to extent of the raster stack                      ##
# 2. Crop the raster stack to the reg ##
#  k = 27, m = 1.2, alpha = 0.1, beta = 0.1, window size = 3x3                ##
#  2.1 Save the model as an RDS                                               ##
# 3. Extract and plot the raster of the cluster groups                        ##
#  3.1 Save the cluster groups raster                                         ##
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(geocmeans)
library(sf)
library(here)
library(viridis)

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

## create a national forest buffers shape using st_intersection
nf_buffers <- st_intersection(nf_sf, fs_reg.crop)

# 2.Crop the raster to the nf_buffers. Then scale using global min-max.
## Because this is cropped. I should look at the variace of the values. 
## the z-score may be appropriate once restricted to the forest buffers.

attri_crop <- crop(df_attri, nf_buffers, mask = TRUE)
attri_crop_sc <- (attri_crop - global(attri_crop, "min", na.rm=TRUE)[,1])/(global(attri_crop, "max", na.rm=TRUE)[,1] - global(attri_crop, "min", na.rm=TRUE)[,1])

## I'M NOT SURE IS THIS IS APPROPRIATE OR NOT ##
attri_crop_sc[is.na(attri_crop_sc)] <- -999 
plot(attri_crop_sc$aip)

## save for the next scripts (this could be its own small script)
writeRaster(attri_crop, filename = here::here(paste0("data/processed/nf_buffers_all_attributes_", 
                                                     Sys.Date(), ".tif")))

writeRaster(attri_crop_sc, filename = here::here(paste0("data/processed/nf_buffers_all_attributes_cropped_then_scaled_", 
                                                        Sys.Date(), ".tif")), overwrite = TRUE)
