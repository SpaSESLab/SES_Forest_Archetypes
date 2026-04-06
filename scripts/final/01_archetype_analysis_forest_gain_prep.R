################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS FOREST GAIN DATA TO 3KM                      ##
# 1. Create the list of tiles (granules) to download from the Hansen et al 2023#
#    Forest Gain data.                                                        ##
#https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html
#  1.1 Download the granules from the website                                 ##
# 2. Create a function to aggregate the data to 3km                           ##
#  2.1 Using a list of file names from the downloaded tiles, aggregate the data#
# 3. Merge the aggregated data into a single raster for the contiguous US     ##
# 4. Reproject, resample, mask, and crop to the reference raster              ##
# 5. Save the raster                                                          ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# Set the projection
projection = "epsg:5070"

# Load reference raster
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# 1. Create the list of tiles (granules) to download from the Hansen et al 2023
#    Forest Gain data.
#-------------------------------------------------------------------------------
#Note:
# This data is downloaded from 
# https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/download.html
# one has to download the desired 10 x 10 degree boxes
# For the contiguous US this includes 
# 50N, 130W; 50N, 120W; 50N, 110W; 50N, 100W; 50N, 90W; 50N, 80W; 50N, 70W
# 40N, 130W; 40N, 120W; 40N, 110W; 40N, 100W; 40N, 90W; 40N, 80W
# 30N, 120W; 30N, 110W; 30N, 100W; 30N, 90W; 30N, 80W
#-------------------------------------------------------------------------------

granules <- c("50N_130W", "50N_120W", "50N_110W", "50N_100W", "50N_090W", 
              "50N_080W", "50N_070W", "40N_130W", "40N_120W", "40N_110W",
              "40N_100W", "40N_090W", "40N_080W", "30N_120W", "30N_110W",
              "30N_100W", "30N_090W", "30N_080W")

## 1.1 Create a function to download the data
download_forgain <- function(grans){    
  exdir <- here::here("data/original/forest_gain/")
  exfile <- paste0(exdir, "Hansen_GFC-2023-v1.11_gain_", grans, ".tif")
  gain.url <- paste0("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_gain_", grans, ".tif")
  download.file(gain.url, exfile)
  fnames <- list.files(exdir)
  return(fnames)
}

## Download the data
for (gran in granules){
  download_forgain(gran)
}

# 2. Create a function to aggregate the data to 3km (3000m)
#-------------------------------------------------------------------------------
## from the website: "This global dataset is divided into 10x10 degree tiles, 
## consisting of seven files per tile. All files contain unsigned 8-bit values 
## and have a spatial resolution of 1 arc-second per pixel, or approximately 
## 30 meters per pixel at the equator."
#-------------------------------------------------------------------------------

agg_forgain <- function(ogrst, fac, res){
  rasters <- rast(ogrst)
  fnames.process <- paste0("data/processed/forest_gain/", names(rasters), "_", res, ".tif")
  rasters.agg <- aggregate(rasters, fact=fac, cores = 2)
  writeRaster(rasters.agg, fnames.process, overwrite=TRUE)
  return(fnames.process) 
}

## 2.1 Using a list of file names from the downloaded tiles, aggregate the data

## Create a list of file names for the downloaded data
fnames_list <- list.files(here::here("data/original/forest_gain"), full.names = TRUE)

## Use a for loop to aggregate all of the tiles to 3km
for (rst in fnames_list) {
  agg_forgain(rst, 100, "3000m")
}

# 3. Create a function to merge the aggregated data over the contiguous US
#-------------------------------------------------------------------------------
## Note: This function will return a raster file to the 
##       data/process/forest_gain/forest_gain_merged directory
#-------------------------------------------------------------------------------

merge_all_rst <- function(res){
  file.list <- list.files(here::here("data/processed/forest_gain/"), pattern = res, full.names = TRUE)
  rasters <- lapply(file.list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_merge", res, ".tif")
  writeRaster(m, filename = paste0("data/processed/forest_gain/forest_gain_merged/", Sys.Date(), "_", fnames.merge), overwrite=TRUE)
  return(paste0("data/processed/forest_gain/", fnames.merge))
}

## set the prefix and resolution
prefix <- "forestgain"
res <- "3000m" 

## Use a for loop to merge all of the tiles
for (r in res) {
  merge_all_rst(r)
}

# 4. Create a function to merge the aggregated data over the contiguous US
#-------------------------------------------------------------------------------
## Read in merged raster
forest_gain <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/forest_gain/forest_gain_merged/forestgain_merge3000m.tif")

## Reproject the merged forest_gain raster
forest_gain_proj <- project(forest_gain, projection)

## Resample the merged forest_gain raster to the reference raster
forest_gain_resamp <- resample(forest_gain_proj, ref_rast, "bilinear")

## Crop and mask the merged forest_gain raster to the reference raster
forest_gain_crop <- crop(forest_gain_resamp, ref_rast, mask = TRUE)

## Quick check of the final forest gain raster
#plot(forest_gain_crop)

# 5. Save the raster
#-------------------------------------------------------------------------------
writeRaster(forest_gain_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/forest_gain_3km_crop_",
                                     Sys.Date(), ".tif"))
