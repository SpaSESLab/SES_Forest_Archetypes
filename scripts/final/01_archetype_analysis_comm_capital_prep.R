################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS BASELINE RESILIENCE INDICATORS FOR           ##
# COMMUNITIES (BRIC) DATA TO 3KM                                              ##
# THIS IS COUNTY LEVEL DATA AND REFLECTS THE COMMUNITY CAPITAL SEE:           ##
# https://sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/data_and_resources/bric/
# FOR MORE INFORMATION                                                        ##
# 1. Load the data                                                            ##
#  1.1 Filter and select variables                                            ##
#  1.2 This is county level data so need to download county boundaries from   ##
#      tigris using the year 2020                                             ##
# 2. Join the BRIC data to the county geometeries                             ##
#  2.1 Check that the new geometries are valid and not empty                  ##
# 3. Fill in the missing data                                                 ##
#  3.1 Create an empty raster with 3km resolution                             ##
#  3.2 Use a custom function to interpolate the missing data                  ##
# 4. Crop and mask the raster prediction to the ref_raster                    ##
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
ref_rast <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif")

# Read in the custom functions
source(here::here("scripts/functions/custom_functions.R"))

# 1. Load the data
#-------------------------------------------------------------------------------
# https://sc.edu/study/colleges_schools/artsandsciences/centers_and_institutes/hvri/documents/bric/bric2020_us_forweb.xlsx
bric <- read_excel("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/original/bric/bric2020_us.xlsx")

## 1.1
## Select variables and rename GEOID to FIPS and COMM CAPITAL to COMM_CAP
bric_2020 <- bric %>% # needs 2020 counties
  dplyr::select("GEOID", "COMM CAPITAL") %>%
  rename("FIPS" = "GEOID", 
         "COMM_CAP" = "COMM CAPITAL")

## 1.2
## Load county boundaries from tigris
### Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

### Set the year to account for changes to FIPS codes
counties_2020 <- tigris::counties(year = 2020) 
counties_2020 <- counties_2020 %>%
  filter(STATEFP %in% continental.states$FIPS) %>%
  dplyr::select(GEOID, geometry)

## Use the update_fips function to pad all FIPS codes in your data with 0s for
## a total of 5 characters 
bric_fips <- update_fips(bric_2020)

# 2. Join the data to the county geometries
#-------------------------------------------------------------------------------
## join to the 2020 counties
bric_county <- left_join(counties_2020, bric_fips,
                      by = c("GEOID" = "FIPS"))

## 2.1
## Check for validity, remove empty geometries, and reproject  
if (!all(st_is_valid(bric_county)))
  bric_county <- st_make_valid(bric_county)

bric_county <- bric_county %>%
  filter(!st_is_empty(.))

bric_proj <- bric_county %>%
  st_transform(projection)

# 3. Fill in missing data
#-------------------------------------------------------------------------------
## 3.1 Create a template raster for the shapefiles
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

## 3.2 Use the idw_preds function to rasterize and fill in missing data using 
##     inverse distance weigthing (idw)
bric.preds <- idw_preds(bric_proj, templateRas, "COMM_CAP", grd)

# 4. Crop to reference raster
#-------------------------------------------------------------------------------
bric_crop <- crop(bric.preds$pred.rst, ref_rast_proj, mask = TRUE)

# quick check that it cropped
plot(bric_crop)

# 5. Save the raster
#-------------------------------------------------------------------------------
#writeRaster(bric_crop, paste0("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/bric_commcap_3km_pred_crop_", 
#                                         Sys.Date(), ".tif"))
