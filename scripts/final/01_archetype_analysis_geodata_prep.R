################################################################################
# SCRIPT TO DOWNLOAD AND PROCESS PRECIPITATION, TEMPERATURE, ELEVATION, AND   ##
# TRAVEL TIME TO URBAN CENTERS >20K POPULATION TO 3KM                         ##
# 1. Download the precipitation (precip_), minimum and maximum temperature,   ##
#    elevation and travel time to urban areas with >20k population using the  ## 
#    geodata package                                                          ##
# 2. Create an outline of the contiguous US (Conus) for cropping the rasters  ##
# NOTE: All of the rasters were global except for elevation. Cropping to Conus##
#       helps with efficiency.                                                ##
# 3. For travel time:                                                         ##
#  3.1 Crop to conus and reproject to the ref_raster                          ##
#  3.2 Fill in missing data using a focal window in a while loop              ##
#  3.3 Crop and mask to the ref_raster                                        ##
# 4. For elevation:                                                           ##
#  4.1 Reproject elevation to the ref_raster projection                       ##
#  4.2 Calculate roughness from the elevation using terra::terrain()          ##
#  4.3 Fill in missing data using a focal window in a while loop              ##
#  4.4 Then crop and mask to the ref_raster                                   ##         
# 5. For the climate data you will calculate the temperature seasonality and  ##
#    the precip seasonality                                                   ##
#  5.1 Crop to conus and reproject prec, t_min, and t_max to the ref_raster   ##
#      projection                                                             ##
#  5.2 Use dismo::biovars to to calculate the temp seasonality (biovar4) and  ## 
#      the precip seasonality (biovar15)                                      ##
#  5.3 Fill in missing data using a focal window in a while loop              ##
#  5.4 Then crop and mask to the ref_raster                                   ##
# 6. Save the rasters                                                         ##
# Focal while loop from stack overflow:                                       ##
# https://stackoverflow.com/questions/73271223/                               ##
# how-to-fill-na-gaps-by-idw-using-focal-r-raster-terra                       ##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(sf) # for working with vector data
library(raster) # for working with rasters
library(sp) # for working with spatial (vector) data
library(geodata) # this package removed from CRAN May 27, 2024...
library(terra) # for working with rasters
library(tigris) # needed for state/CONUS boundaries
library(dplyr) # for manipulating dataframes 
library(dismo) # needed to calculate biovars
library(gstat)
library(stars)

# Set the projection
# NOTE: Projection will distort the raw data because it is in seconds 
#       (and so not perfect squares)
projection = "epsg:5070"

# Load reference raster
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# 1. Download the data using the geodata package
#-------------------------------------------------------------------------------
## NOTE: This can take a few minutes and the files are large
#r_prec <- geodata::worldclim_country(country = "US", var = "prec", res = 0.5, path = here::here("data/original/climate/"))
#r_tmin <- geodata::worldclim_country(country = "US", var = "tmin", res = 0.5, path = here::here("data/original/climate/"))
#r_tmax <- geodata::worldclim_country(country = "US", var = "tmax", res = 0.5, path = here::here("data/original/climate/"))
#r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/elevation/"))
#r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/travel/"))

r_prec <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_prec.tif"))
r_tmin <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmin.tif"))
r_tmax <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmax.tif"))
r_ele <- rast(here::here("data/original/elevation/USA_elv_msk.tif"))
r_tt <- rast(here::here("data/original/travel/travel_time_to_cities_u7.tif"))

# 2. Crop to CONUS, because the climate and travel time data are global
#    it is more efficient to crop to the extant of conus states first
#-------------------------------------------------------------------------------
## NOTES: To be more reproducible you will need to use the resulting conus shape
## for cropping the travel time, precip, and temp rasters. 

## Using tigris, download the state boundaries
### Load the states from tigris
states <- tigris::states(cb = TRUE)
### Get Continental US list
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] #only CONUS

### Filter tigris states for conus states and set crs to one of the downloaded
### rasters. conus_for_crop is needed for the climate variables, but not the 
### travel time or elevation data. 
conus_for_crop <- states %>%
  filter(STUSPS %in% continental.states$state) %>%
  dplyr::select(STUSPS, GEOID, geometry) %>%
  st_transform(., crs = crs(r_tt))

# 3. Travel time
#-------------------------------------------------------------------------------
## 3.1 
## Crop and mask to conus then project to reference raster
tt_crop <- crop(r_tt, conus_for_crop, mask = TRUE)
tt_proj <- project(tt_crop, ref_rast)

## 3.2 
## Use a while loop to fill in NAs at edges of Conus with focal window
tt_w <- 1 # create a starting window size of 1
tt_filled <- tt_proj # just in case you want to keep the original
tt_to_fill <- any(is.na(values(tt_filled))) # define the area to fill

## The focal window size will continue to expand until all NA values are filled
while(tt_to_fill) {
  tt_w <- tt_w + 2  
  tt_filled <- focal(tt_filled, 
                     w = tt_w, 
                     fun = mean, 
                     na.policy = "only", 
                     na.rm = T) 
  tt_to_fill <- any(is.na(values(tt_filled)))
}
print(tt_w) # show final window size

## 3.3 
## Crop and mask to the ref_raster
tt_filled_crop <- crop(tt_filled, ref_rast, mask = TRUE)

# quickly check raster plot and that nrows matches nrows for the ref_raster
plot(tt_filled_crop)
nrow(as.data.frame(tt_filled_crop))
nrow(as.data.frame(ref_rast))

# 4. Roughness
#-------------------------------------------------------------------------------
## 4.1 
## Reproject to reference raster
ele_proj <- project(r_ele, ref_rast)

## 4.2 
## Calculate roughness from the elevation using terra::terrain()
roughness <- terrain(ele_proj, v = "roughness")

## 4.3 
## Fill in missing data using a focal window in a while loop
rough_w <- 1
rough_filled <- roughness 
rough_to_fill <- any(is.na(values(rough_filled)))

while(rough_to_fill) {
  rough_w <- rough_w + 2  
  rough_filled <- focal(rough_filled, 
                        w = rough_w, 
                        fun = mean, 
                        na.policy = "only", 
                        na.rm = T) 
  rough_to_fill <- any(is.na(values(rough_filled)))
}
print(rough_w) # show final window size

## 4.4 
## Then crop and mask to the ref_raster
rough_filled_crop <- crop(rough_filled, ref_rast, mask = TRUE)

# quickly check raster plot and that nrows matches nrows for the ref_raster
plot(rough_filled_crop)
nrow(as.data.frame(rough_filled_crop))
nrow(as.data.frame(ref_rast))

# 5. For the climate data you will calculate the temperature seasonality and
#    the precip seasonality 
#-------------------------------------------------------------------------------
## 5.1 Crop to conus and reproject prec, t_min, and t_max to the ref_raster
##     projection 
### precip
prec_crop <- crop(r_prec, conus_for_crop, mask = TRUE)
prec_proj <- project(prec_crop, ref_rast)
### t_max
tmax_crop <- crop(r_tmax, conus_for_crop, mask = TRUE)
tmax_proj <- project(tmax_crop, ref_rast)
### t_min
tmin_crop <- crop(r_tmin, conus_for_crop, mask = TRUE)
tmin_proj <- project(tmin_crop, ref_rast)

## 5.2 
## Use dismo::biovars to to calculate the temp seasonality (biovar4) and 
## the precip seasonality (biovar15)  

## To use biovars the raster must be a brick
bio_calcs <- biovars(brick(prec_proj), brick(tmin_proj), brick(tmax_proj))

temp_seas <- rast(bio_calcs$bio4)
prec_seas <- rast(bio_calcs$bio15)

## 5.3 
## Fill in missing data using a focal window in a while loop
### temp seasonality 
temp_w <- 1
temp_filled <- temp_seas 
temp_to_fill <- any(is.na(values(temp_filled)))

while(temp_to_fill) {
  temp_w <- temp_w + 2  
  temp_filled <- focal(temp_filled, 
                       w = temp_w, 
                       fun = mean, 
                       na.policy = "only", 
                       na.rm = T) 
  temp_to_fill <- any(is.na(values(temp_filled)))
}
print(temp_w) # show final window size

### precip seasonality
prec_w <- 1
prec_filled <- prec_seas # just in case you want to keep the original
prec_to_fill <- any(is.na(values(prec_filled)))

while(prec_to_fill) {
  prec_w <- prec_w + 2  
  prec_filled <- focal(prec_filled, 
                       w = prec_w, 
                       fun = mean, 
                       na.policy = "only", 
                       na.rm = T) 
  prec_to_fill <- any(is.na(values(prec_filled)))
}
print(prec_w) # show final window size

## 5.4 
## Then crop and mask to the ref_raster
temp_filled_crop <- crop(temp_filled, ref_rast, mask = TRUE)

# quickly check raster plot and that nrows matches nrows for the ref_raster
plot(temp_filled_crop)
nrow(as.data.frame(temp_filled_crop))
nrow(as.data.frame(ref_rast))

prec_filled_crop <- crop(prec_filled, ref_rast, mask = TRUE)

# quickly check raster plot and that nrows matches nrows for the ref_raster
plot(prec_filled_crop)
nrow(as.data.frame(prec_filled_crop))
nrow(as.data.frame(ref_rast))

# 6. Save the rasters
#-------------------------------------------------------------------------------
## travel time
writeRaster(tt_filled_crop, here::here(paste0("data/processed/variables/trav_time_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE))
## roughness
writeRaster(rough_filled_crop, here::here(paste0("data/processed/variables/roughness_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE))
## precip seasonality
writeRaster(prec_filled_crop, here::here(paste0("data/processed/variables/prec_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE))
## temp seasonality
writeRaster(temp_filled_crop, here::here(paste0("data/processed/variables/temp_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE))

