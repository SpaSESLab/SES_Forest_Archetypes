################################################################################
# Using the rFIA package calculate county level forest productivity           ##
# Stanke H, Finley AO, Weed AS, Walters BF, Domke GM (2020).                  ## 
# “rFIA: An R package for estimation of forest attributes with the US Forest  ## 
# Inventory and Analysis database.” Environmental Modelling & Software, 127,  ##
# 104664. https://doi.org/10.1016/j.envsoft.2020.104664.                      ##
# 1. Download the state counties using tigris                                 ##
#  1.1 Download the FIA COND tables for states in the contiguous US           ##
# 2. Create a GEOID for the FIA data                                          ##
# 3. Update the productivity code with real number                            ##
# 4. Summarise the productivity                                               ##
# 5. Join to counties and make a simple feature (sf)                          ##
# 6. Validate geometries                                                      ##
# 7. Fill in the missing data                                                 ##
#  7.1 Create an empty raster with 3km resolution                             ##
#  7.2 Use a custom function to interpolate the missing data                  ##
# 8. Crop to reference raster                                                 ##
# 9. Save the raster                                                          ##
################################################################################

# 0. Load libraries and extent timeout
#-------------------------------------------------------------------------------
library(rFIA)
library(tidyverse) 
library(tigris)
library(sf)
library(sp)
library(terra)
library(raster)
library(stringr)
library(ggplot2)
library(spdep)
library(gstat)
library(stars)

options(timeout=6000)

# Set the projection
projection <- "epsg:5070"

# Load the reference raster
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# Read in the custom functions
source(here::here("scripts/functions/data_processing_custom_functions.R"))

# 1. Load the county boundaries and FIA data
#-------------------------------------------------------------------------------
## Load county boundaries from tigris
counties <- tigris::counties(year = 2020)
##Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

counties <- counties %>%
  filter(STATEFP %in% continental.states$FIPS) %>%
  dplyr::select(GEOID, COUNTYFP, STATEFP, geometry)

## 1.1 Using a for loop, download FIA COND table for all states

## create a list of the states for the loop
states_list <- continental.states$state

## for loop to download the data
for (s in states_list){
  getFIA(s, dir = here::here("data/original/fia/"), tables = "COND", load = TRUE)
}

## read in the fia data
fia <- readFIA(here::here("data/original/fia"), tables = "COND", inMemory = TRUE)

## from the COND table select the STATECD, COUNTYCD, SITECLCD
conus_prod <- dplyr::select(fia$COND, STATECD, COUNTYCD, SITECLCD)

# 2. Create a GEOID column
#-------------------------------------------------------------------------------
## create a GEOID of the of the county and state codes (make the FIPS code) to 
## easily join to counties and to better group and summarise
conus_prod <- conus_prod %>%
  mutate(GEOID = paste0(str_pad(as.character(STATECD), 2, pad = "0"), str_pad(COUNTYCD, 3, pad = "0")))

# Need to replace GEOID 46113 with 46102
conus_prod$GEOID <- str_replace_all(conus_prod$GEOID, "46113", "46102")

# 3. Update the productivity code
#-------------------------------------------------------------------------------
# change the SITECD (productivity code) to a real number 
# for code 1 = 225 cuf/ac/yr, 
# but every other code is the ((max - min)/2) + min 
# see https://www.fs.usda.gov/rm/pubs/rmrs_gtr245.pdf 
# USDA Forest Service Gen. Tech. Rep. RMRS-GTR-245. 2010 pg 53 

conus_prod <- conus_prod %>%
  mutate(siteprod = case_when(
    SITECLCD == 1 ~ 225,
    SITECLCD == 2 ~ 194.5,
    SITECLCD == 3 ~ 142,
    SITECLCD == 4 ~ 102, 
    SITECLCD == 5 ~ 67,
    SITECLCD == 6 ~ 34.5, 
    SITECLCD == 7 ~ 9.5
  ))

# 4. Summarise the data
#-------------------------------------------------------------------------------
## group the data by GEOID (FIPS code)
## there will be warning for rows with no data
conus_prod_grp <- conus_prod %>%
  group_by(GEOID) %>%
  summarise(max_prodcd = max(as.numeric(SITECLCD), na.rm= TRUE), 
            min_prodcd = min(as.numeric(SITECLCD), na.rm= TRUE),
            mean_prod = mean(as.numeric(siteprod), na.rm = TRUE))

# 5. Join to the county geometries and make it an sf
#-------------------------------------------------------------------------------
conus_prod_sf <- st_as_sf(left_join(counties, conus_prod_grp, by = "GEOID"))

# 6. check for validity, remove empty geometries, and reproject 
#-------------------------------------------------------------------------------
if (!all(st_is_valid(conus_prod_sf)))
  conus_prod_sf <- st_make_valid(conus_prod_sf)

conus_prod_sf <- conus_prod_sf %>%
  filter(!st_is_empty(.))

forprod_proj <- conus_prod_sf %>%
  st_transform(projection)

# 7. Rasterize and fill in missing data with IDW
#-------------------------------------------------------------------------------
## 7.1 Create a template raster for the shapefiles
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

grd <- st_as_stars(templateRas)

## 7.2 Use the idw_preds function to rasterize and fill in missing data using 
##     inverse distance weigthing (idw)

forprod.preds <- idw_preds(forprod_proj, templateRas, "mean_prod", grd)

# 8. Crop to reference raster
#-------------------------------------------------------------------------------
forprod_crop <- crop(forprod.preds$pred.rst, ref_rast_proj, mask = TRUE)
# quick check that it cropped
plot(forprod_crop)

# 9. Save the raster
#-------------------------------------------------------------------------------
writeRaster(forprod_crop, here::here(paste0("data/processed/variables/fia_for_prod_3km_pred_crop_", 
                                         Sys.Date(), ".tif")))
