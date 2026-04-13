################################################################################
# SCRIPT TO CREATE THE RASTER STACK(S) FOR USE IN THE CLUSTER ANALYSIS        ##
# THIS SCRIPT DOES NOT INCLUDE THE PROPRITARY FORISK MILL DATA                ##
# 1. Load the rasters that are cropped to the reference raster and at 3km res ##
# 2. Check for NAs or unequal lengths of data                                 ##
# 3. Stack the rasters which will also check that they are all aligned        ##
# 4. Update the variable names                                                ##
# 5. Scale the data from 0-1 (min-max scaling)                                ##
# 6. Save the rasters                                                         ##
# Optional: Repeat steps 3-6 for subsets of the variables                     ##
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)


# 1. Load the rasters that are cropped to the reference raster and at 3km res
#-------------------------------------------------------------------------------
aip_crop <- rast(here::here("data/processed/variables/aip_3km_pred_crop_2024-09-30.tif"))
bric_crop <- rast(here::here("data/processed/variables/bric_commcap_3km_pred_crop_2024-09-30.tif"))
netmig_crop <- rast(here::here("data/processed/variables/net_mig_2023_3km_pred_crop_2024-09-30.tif"))
forpay_crop <- rast(here::here("data/processed/variables/pct_forpay_3km_pred_crop_2024-09-30.tif"))
forprod_crop <- rast(here::here("data/processed/variables/fia_for_prod_3km_pred_crop_2024-09-30.tif"))
lesshs_crop <- rast(here::here("data/processed/variables/cejst_lesshs_3km_pred_crop_2024-09-30.tif"))
hsburd_crop <- rast(here::here("data/processed/variables/cejst_houseburd_3km_pred_crop_2024-09-30.tif"))
engburd_crop <- rast(here::here("data/processed/variables/cejst_engburd_3km_pred_crop_2024-09-30.tif"))
pm25_crop <- rast(here::here("data/processed/variables/cejst_pm25_3km_pred_crop_2024-09-30.tif"))
travtime_crop <- rast(here::here("data/processed/variables/trav_time_3000m_2024-10-03.tif"))
rough_crop <-  rast(here::here("data/processed/variables/roughness_3000m_2024-10-07.tif"))
precseas_crop <-  rast(here::here("data/processed/variables/prec_seas_3000m_2024-10-07.tif"))
tempseas_crop <-  rast(here::here("data/processed/variables/temp_seas_3000m_2024-10-07.tif"))
distwild_crop <-  rast(here::here("data/processed/variables/dist_to_wild_3km_pred_crop_2024-10-01.tif"))
distcrit_crop <-  rast(here::here("data/processed/variables/dist_to_crithab_3km_pred_crop_2024-10-01.tif"))
fedrich_crop <-  rast(here::here("data/processed/variables/conus_fed_rich_crop_2024-10-01.tif"))
treecov_crop <-  rast(here::here("data/processed/variables/conus_tree_cover_crop_2024-10-08.tif"))
treeage_crop <-  rast(here::here("data/processed/variables/conus_tree_age_crop_2024-10-08.tif"))
forgain_crop <- rast(here::here("data/processed/variables/forest_gain_3km_crop_2024-10-01.tif"))
whp_crop <-  rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

## NOTE TO MATT/LILY: Now that these variables all reside in the 
## data/processed/variables directory there is probably a more efficient way 
## to load them all in while excluding the FORISK data.

# 2. Check for NAs or unequal lengths of data
#-------------------------------------------------------------------------------
## This is how I figured out I still had missing data and used the focal windows
## in the while loop for the prec_seas, temp_seas, and roughness

nrow(as.data.frame(aip_crop)) #874789
nrow(as.data.frame(bric_crop)) #874789
nrow(as.data.frame(forpay_crop)) #874789
nrow(as.data.frame(forgain_crop)) #874789
nrow(as.data.frame(forprod_crop)) #874789
nrow(as.data.frame(netmig_crop)) #874789
nrow(as.data.frame(lesshs_crop)) #874789
nrow(as.data.frame(treeage_crop)) #874789
nrow(as.data.frame(treecov_crop)) #874789
nrow(as.data.frame(hsburd_crop)) #874789
nrow(as.data.frame(engburd_crop)) #874789
nrow(as.data.frame(whp_crop)) #874789
nrow(as.data.frame(rough_crop)) #874789
nrow(as.data.frame(travtime_crop)) #874789
nrow(as.data.frame(precseas_crop)) #874789
nrow(as.data.frame(tempseas_crop)) #874789
nrow(as.data.frame(fedrich_crop)) #874789
nrow(as.data.frame(distwild_crop)) #874789
nrow(as.data.frame(distcrit_crop)) #874789
nrow(as.data.frame(pm25_crop)) #874789

## NOTE TO MATT/LILY: There is probably some clever way to do this in less 
## lines of code, but this works for now


# 3. Stack the rasters which will also check that they are all aligned
#-------------------------------------------------------------------------------
## If the rasters can't stack, then there is an issue with the data. 
rast_stack <- c(aip_crop, bric_crop, forpay_crop, forprod_crop, 
                lesshs_crop, hsburd_crop, engburd_crop, pm25_crop,
                travtime_crop, rough_crop, precseas_crop, tempseas_crop,
                distwild_crop, distcrit_crop, fedrich_crop,
                treecov_crop, treeage_crop, forgain_crop, whp_crop, 
                netmig_crop)

# 4. Update the variable names
#-------------------------------------------------------------------------------
names(rast_stack) <- c("aip", "comm_cap", "pct_forpay", "forprod", 
                       "lesshs", "hsbrd", "engbrd", "pm25", "travtime",
                       "rough", "precseas", "tempseas", "distwild", "distcrit", 
                       "fedrich", "treecov", "treeage", "forgain",
                       "whp", "netmig")

# 5. Scale the data from 0-1 (min-max scaling)
#-------------------------------------------------------------------------------
rast_stack_sc <- (rast_stack - global(rast_stack, "min", na.rm=TRUE)[,1])/(global(rast_stack, "max", na.rm=TRUE)[,1] - global(rast_stack, "min", na.rm=TRUE)[,1])

# 6. Save the rasters
#-------------------------------------------------------------------------------
writeRaster(x = rast_stack, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_no_forisk_", 
                                              Sys.Date(), ".tif")), 
            overwrite = TRUE)

writeRaster(x = rast_stack_sc, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_no_forisk_scaled_", 
                                              Sys.Date(), ".tif")), 
            overwrite = TRUE)


## Optional: Repeat steps 3-6 for different subsets of variables


## 3. Stack the ecological variables, scale, and save the rasters
eco_rast_stack <- c(forprod_crop, rough_crop, 
                    precseas_crop, tempseas_crop,
                    treecov_crop, treeage_crop,
                    forgain_crop, whp_crop)

## 4. Rename the variables
names(eco_rast_stack) <- c("forproc", "rough", "precseas", "tempseas",
                           "treecov", "treeage", "forgain", "whp")

## 5. Scale the data from 0-1 (min-max scaling)
eco_rast_stack_sc <- (eco_rast_stack - global(eco_rast_stack, "min", na.rm=TRUE)[,1])/(global(eco_rast_stack, "max", na.rm=TRUE)[,1] - global(eco_rast_stack, "min", na.rm=TRUE)[,1])

## 6. Save the rasters
writeRaster(x = eco_rast_stack, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_eco_attributes_", 
                                              Sys.Date(), ".tif")), 
            overwrite = TRUE)

writeRaster(x = eco_rast_stack_sc, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_eco_attributes_scaled_", 
                                                 Sys.Date(), ".tif")),
            overwrite = TRUE)

## 3. Stack the social variables, scale, and save the rasters
soc_rast_stack <- c(aip_crop, bric_crop,
                    forpay_crop, lesshs_crop, 
                    hsburd_crop, engburd_crop,
                    pm25_crop, travtime_crop,
                    distwild_crop, distcrit_crop, 
                    fedrich_crop, netmig_crop)

## 4. Rename the variables
names(soc_rast_stack) <- c("aip", "comm_cap", 
                           "pct_forpay",  "lesshs", 
                           "hsbrd", "engbrd", 
                           "pm25",  "travtime", 
                           "distwild", "distcrit", 
                           "fedrich", "netmig")

## 5. Scale the data from 0-1 (min-max scaling)
soc_rast_stack_sc <- (soc_rast_stack - global(soc_rast_stack, "min", na.rm=TRUE)[,1])/(global(soc_rast_stack, "max", na.rm=TRUE)[,1] - global(soc_rast_stack, "min", na.rm=TRUE)[,1])

## 6. Save the rasters
writeRaster(x = soc_rast_stack, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_soc_attributes_no_forisk_", 
                                                  Sys.Date(), ".tif")),
            overwrite = TRUE)

writeRaster(x = soc_rast_stack_sc, 
            filename = here::here(paste0("data/processed/raster_stacks/rast_stack_soc_attributes_no_forisk_scaled_", 
                                                     Sys.Date(), ".tif")),
            overwrite = TRUE)


