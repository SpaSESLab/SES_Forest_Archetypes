################################################################################
# SCRIPT TO CALCULATE THE ENTROPY VALUES OF THE FINAL SGFCM CLUSTERS          ##
# 1. Load the SGFCM cluster results raster from                               ##
#    15_forest_archetyes_nf_level_final_sgfcm_clusters.R                      ##
# 2. Calculate the entropy                                                    ##
#  2.1 Subset the belongings from the results raster                          ##
#  2.2 Make the raster into a dataframe                                       ##
#  2.3 Use the calcUncertaintyIndex() to determine the entropy                ##
#  2.4 Combine the raster dataframe and the entropy values and make into a    ##
#      raster                                                                 ##
# 3. Quick check of the plots                                                 ##
# 4. Save the entropy raster                                                  ##
################################################################################


# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(future)
library(spdep)
library(classInt)

# 1. Load the SGFCM cluster result from 15_forest_archetyes_nf_level_final_sgfcm_clusters.R
#-------------------------------------------------------------------------------
## This should be the raster with the belongings values
rast <- rast(here::here("outputs/nf_level/SGFCM_nf_buffers_k27_belong_2026-04-14.tif"))

## Extract the cluster group rasters from the data
arch_all_rst <- rast(SGFCM_result$rasters)

# 2. Calculate the entropy
#-------------------------------------------------------------------------------

## 2.1 
## Subset the belongings
arch_all_rst_conus_belong <- subset(rast, 1:27)

## 2.2
## Make the raster into a dataframe
arch_all_rst_conus_df <- as.data.frame(rast, xy = TRUE)

## 2.3 
## Use the calcUncertaintyIndex() to determine the entropy 
all_ent_conus <- calcUncertaintyIndex(as.matrix(as.data.frame(arch_all_rst_conus_belong)))

## 2.4 
## Combine the raster dataframe and the entropy values
## and make into a raster
all_ent_conus <- cbind(arch_all_rst_conus_df, all_ent_conus)
all_ent_rst_conus <- rast(all_ent_conus)

# 3. Quick check of the plots
#-------------------------------------------------------------------------------
plot(all_ent_rst_conus$all_ent_conus)
plot(arch_all_rst_conus_belong[[1:16]], legend=FALSE)
plot(arch_all_rst_conus_belong[[17:27]], legend=FALSE)

# 4. Save the entropy raster
#-------------------------------------------------------------------------------
