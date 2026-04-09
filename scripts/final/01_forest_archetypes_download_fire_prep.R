################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS WILDFIRE HAZARD POTENTIAL DATA TO 3KM        ##
# The raster produced in this step will act as your "reference raster" for    ##
# the rest of the data preprocessing steps.                                   ##
# 1. Set up a function to download the WHP data for each state                ##
#  1.1 Create a list of states within the contiguous US (CONUS)               ##
# 2. With a for loop over the list of states, use the function to download    ##
#    the data for each state using the list of states                         ##
# 3. Create a function to gather the WHP state tifs and merge them together   ##
# 4. Aggregate the merged data to 3km                                         ##
# 5. Using inverse distance weighting, fill in any missing data               ##
# 6. Crop the raster to the CONUS state outlines from tigris                  ##
# 7. Save the raster                                                          ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stringr)
library(RCurl)
library(gstat)

# Set the timeout to 100 minutes (6000 seconds)
options(timeout=6000)

# 1. Write a function to download the data
#-------------------------------------------------------------------------------
# Only want the wildfire hazard potential (WHP)
download_fire <- function(st){    
  tmp <- tempfile()
  fs.url <- paste0("https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2020-0016/RDS-2020-0016_",st,".zip")
  download.file(fs.url, tmp)
  tmp2 <- tempfile()
  unzip(zipfile=tmp, exdir = tmp2 )
  dir.name <- list.files(tmp2)
  rast.file <- list.files(paste0(tmp2,"/", dir.name), pattern="*.tif$", full.names = TRUE)
  whp.rast.file <- rast.file[grepl("WHP", rast.file)]
  rasters <- rast(whp.rast.file)
  fnames <- paste0("data/original/",names(rasters), ".tif")
  print(fnames)
  writeRaster(rasters, filename = fnames, overwrite = TRUE)
  return(fnames)
}

## 1.1 Create state list, excluding Alaska, DC, HI, and territories
states <- st_drop_geometry(states())
st_list <- states %>%
  dplyr::select(GEOID, NAME) %>%
  mutate(NAME = gsub(" ", "", NAME)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID != 2 & GEOID != 11 & GEOID != 15 & GEOID < 60) %>%
  dplyr::select(NAME)

# 2. Download the data. NOTE: may need to complete in chunks by indexing st_list
#-------------------------------------------------------------------------------
for (state in st_list[31:48,]) {
  download_fire(state)
}

# 3. Merge the state .tifs before aggregating to avoid gaps between the states
#-------------------------------------------------------------------------------
## Set the prefix
prefix <- "WHP"

## Create a function to merge the state .tifs
merge_all_rst <- function(prefix){
  fnames_list <- list.files(here::here("data/original/fire"), pattern = "WHP", full.names = TRUE)
  rasters <- lapply(fnames_list, function(x) rast(x))
  rst.sprc <- sprc(rasters)
  m <- merge(rst.sprc)
  names(m) <- prefix
  fnames.merge <- paste0(prefix, "_", Sys.Date(), "_merged.tif")
  writeRaster(m, filename = paste0("data/processed/merged/", fnames.merge), overwrite=TRUE)
  return(paste0("data/processed/merged/", fnames.merge))
}

## Run the merge_all_rst function
merge_all_rst(prefix)

# 4. Aggregate the newly merged raster to 3km
#-------------------------------------------------------------------------------
## Read in the new raster
whp_merged <- rast(here::here("data/processed/merged/WHP_2024-08-09_merged.tif"))

## Aggregate to 3km
conus_whp_3km_agg <- aggregate(whp_merged,
                               fact = 100,
                               cores = 2)

## Save the newly merged and aggregated WHP raster
writeRaster(conus_whp_3km_agg, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_", Sys.Date(), ".tif"))

# 5. Fill in missing data through inverse distance weighting (IDW)
#-------------------------------------------------------------------------------
## Read in the merged and aggregated WHP raster
conus_whp_3km_agg <- rast(here::here("data/processed/merged/conus_whp_3km_agg_2024-08-09.tif"))

## Turn it into a dataframe for use with gstat
whp_df <- as.data.frame(conus_whp_3km_agg, xy=TRUE)

## Use gstat to create an IDW model
mod <- gstat(id = "WHP", formula = WHP~1, locations = ~x+y, data = whp_df,
            nmax = 7, set = list(idp = 0.5))

## Interpolate the merged and aggregated raster with the IDW model
whp_interp <- interpolate(conus_whp_3km_agg, mod, debug.level = 0, index = 1)
writeRaster(whp_interp, paste0(here::here("data/processed/merged/"), "conus_whp_3km_agg_interp_", Sys.Date(), ".tif"))

# 6. Crop interpolated raster to Conus states
#-------------------------------------------------------------------------------
## Set the projection
projection <- "epsg:5070"

## Load the states from tigris
states <- tigris::states(cb = TRUE)

## Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

## Filter tigris states for conus states and set crs to crs of raster
conus_states <- states %>%
  filter(STUSPS %in% continental.states$state) %>%
  dplyr::select(STUSPS, GEOID, geometry) %>%
  st_transform(., crs = crs(projection))

## Crop the interpolated whp raster to conus states. Make sure mask = TRUE
whp_interp_crop <- crop(whp_interp, conus_states, mask = TRUE)

# 7. Save the cropped interpolated raster 
#-------------------------------------------------------------------------------
writeRaster(whp_interp_crop, paste0(here::here("data/processed/variables/"), "conus_whp_3km_agg_interp_crop_", Sys.Date(), ".tif"))

# As a quick check, read in the raster and plot it
rast_check <- rast("/Users/katiemurenbeeld/Analysis/SES_Forest_Archetypes/data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif")
plot(rast_check)
