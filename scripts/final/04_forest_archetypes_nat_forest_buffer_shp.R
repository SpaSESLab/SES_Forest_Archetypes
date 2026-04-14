################################################################################
# SCRIPT TO CREATE THE NATIONAL FOREST + 50KM BUFFERS SHAPEFILE               ##
# 1. Load the full attribute raster stack, the USFS regional and forest       ##
#   boundaries                                                                ##
# 2. Process the USFS administrative boundaries                               ##
#    Filter out Region 10 and Forests in Region 10, transform to projection, and
#    crop to the raster stack extent                                          ##
# 3. Create buffers around each forest                                        ##
#  3.1 Create a function that takes the area with national forests and the    ## 
#      distance of the buffer in meters. Use st_buffer() to create a buffer   ## 
#      around each forest                                                     ##
#  3.2 Use the function to create 50km buffers around each forest             ##
# 4. Save the shapefile                                                       ##
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)

## Set the projection
projection <- "epsg: 5070"

# 1. Load the data
#-------------------------------------------------------------------------------
## Load the scaled (or unscaled, doesn't really matter) attribute raster stack
rst_sc <- rast(here::here("data/processed/raster_stacks/rast_stack_all_attributes_scaled_2024-10-08.tif"))

## Load the USFS boundaries
fs_nf <- st_read(here::here("data/original/usfs_admin_boundaries/S_USA.AdministrativeForest.shp"))
fs_reg <- st_read(here::here("data/original/usfs_admin_boundaries/S_USA.AdministrativeRegion.shp"))

# 2. Process the USFS admin boundaries
#-------------------------------------------------------------------------------
## Filter out Region 10 and Forests in Region 10, transform to projection, and
## crop to the raster stack extent

## For the forest boundaries
fs_nf.proj <- fs_nf %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_nf.crop <- st_crop(fs_nf.proj, ext(rst_sc))

## And the region boundaries
fs_reg.proj <- fs_reg %>% 
  filter(REGION != "10") %>%
  st_transform(., crs=projection)
fs_reg.crop <- st_crop(fs_reg.proj, ext(rst_sc))

 
# 3. Create buffers around each forest
#-------------------------------------------------------------------------------
## 3.1
## Create a function that takes the area with national forests and the distance
## of the buffer in meters. Use st_buffer() to create a buffer around each forest
nf_create_buffers <- function(area_with_nf, dist_m){
  nf_buffs <- data.frame()
  for (nf in 1:109) {
    tmp_nf <- area_with_nf %>%
      filter(FORESTORGC == area_with_nf$FORESTORGC[nf])
    tmp_nf_buf <- st_buffer(tmp_nf, dist = dist_m)
    nf_buffs <- rbind(nf_buffs, tmp_nf_buf)
  }
  return(nf_buffs)
}

## 3.2
## Use the function to create 50km buffers around each forest
nf_buffers <- nf_create_buffers(fs_nf.crop, 50000)

# 4. Save the shapefile
#-------------------------------------------------------------------------------
write_sf(nf_buffers, here::here(paste0("data/processed/nf_buffers_50k_", 
                                       Sys.Date(), 
                                       ".shp")))