################################################################################
# SCRIPT TO GENERATE ELBOW PLOTS TO DETERMINE THE OPTIMAL NUMBER OF K OR A    ##
# RANGE OF K TO TEST FOR THE FUZZY CMEANS PARAMETERIZATION                    ##
# 1. Load the scaled data created in name_of_script.R                         ##
# 2. Reformat the raster as a data frame and (as a precaution) remove any NAs ##
#  2.1 Optional: Select different sets of attributes                          ##
# 3. Use the elbow_plots() function from parameterization_functions.R         ##
#    to generate elbow plots to determine the optimal number of k cluster     ##
#    centers or a range of k values to test further.                          ##
#    NOTE: This function will automatically save the plots.                   ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(raster)
library(ggplot2)

# Read in the custom functions
source(here::here("scripts/functions/parameterization_functions.R"))

# 1. Load the scaled data created in name_of_script.R
# ------------------------------------------------------------------------------
nfbuff_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))
nfbuff <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))

# 2. Reformat the raster as a data frame and (as a precaution) remove any NAs
# ------------------------------------------------------------------------------
df_na <- as.data.frame(nfbuff_sc, na.rm=FALSE) # Extract values including NAs
df_na_complete <- na.omit(df_na)           # Remove rows with any NA values

## 2.1 Optional: Select different sets of attributes
df_na_bio <- df_na_complete %>%
  dplyr::select(treecov, forprod, forgain, treeage, tempseas, precseas, rough, whp)

df_na_comm <- df_na_complete %>%
  dplyr::select(lesshs, hsbrd, engbrd, travtime, pm25, aip, netmig, comm_cap, 
                pct_forpay, pct_delmill)

df_na_rules <- df_na_complete %>%
  dplyr::select(distcrit, distwild, fedrich)


# 3. Use the elbow_plots() function from parameterization_functions.R
# ------------------------------------------------------------------------------
## elbow_plots(data, num_k, "file_name", option = "R2" or "INERT")
## This function will take the raster data as a data frame (data), the 
## maximum number of cluster centers, k, to test. k = 2:(num_k). 
## The function will print the plot and save the plot based on the file_name 
## and num_k. 
## The options let you select elbow plots based on:
##            r2 values (between cluster sum of squares / total sum of scares) 
##            inertia values (total within cluster sum of squares)

elbow_plots(df_na_rules, 5, "test_elbow_plots_func", option = "R2")
elbow_plots(df_na_rules, 5, "test_elbow_plots_func", option = "INERT")


