################################################################################
# SCRIPT TO GENERATE THE NATIONAL FOREST LEVEL SGFCM CLUSTERS                 ##
# 1. Load the data                                                            ##
#  1.1 Format the data for use in geocmeans                                   ##
# 2. Use the spatial generalized fuzzy c-means (SGFCM) to create the clusters ##
#  k = 27, m = 1.2, alpha = 0.1, beta = 0.1, window size = 3x3                ##
#  2.1 Save the model as an RDS                                               ##
# 3. Extract and plot the raster of the cluster groups                        ##
#  3.1 Save the cluster groups raster                                         ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(here)


# 1. Load the data
#-------------------------------------------------------------------------------

nf_crop_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))
nf_crop <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))

## 1.1 Format for use in geocmeans
dataset <- lapply(names(nf_crop_sc), function(n){
  aband <- nf_crop_sc[[n]]
  return(aband)
})
names(dataset) <- names(nf_crop_sc)

# 2. Use Spatial Generalized Fuzzy C-Means clustering 
#-------------------------------------------------------------------------------

## Set the window size
w <- matrix(1, nrow = 3, ncol = 3)


SGFCM_result <- SGFCMeans(dataset, k = 27, m = 1.2, standardize = FALSE,
                              lag_method = "mean",
                              window = w, alpha = 0.1, beta = 0.1,
                              seed = 6891, tol = 0.001, verbose = TRUE, init = "kpp")
## 2.1 Save the model as an RDS
saveRDS(SGFCM_result, here::here(paste0("/outputs/SGFCM_nf_buffers_k27_", 
                             Sys.Date(), ".rds")))
# 3. Extract and plot the rasters of the resulting clusters
#-------------------------------------------------------------------------------
map_SGFCM_result <- rast(SGFCM_result$rasters)
plot(map_SGFCM_result[["Groups"]])


## 3.1 Save the cluster raster as a tif
writeRaster(map_SGFCM_result[["Groups"]], 
            filename = here::here(paste0("/outputs/SGFCM_nf_buffers_k27_", 
                                         Sys.Date(), ".tif")))
