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

## Download and process raster data of temperature and precipitation
## Download and process travel time data
## Download elevation data and calculate a topographic complexity index
## focal while loop from stack overflow: https://stackoverflow.com/questions/73271223/how-to-fill-na-gaps-by-idw-using-focal-r-raster-terra

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
#r_prec <- geodata::worldclim_country(country = "US", var = "prec", res = 0.5, path = tempdir(), mask=TRUE)
#r_tmin <- geodata::worldclim_country(country = "US", var = "tmin", res = 0.5, path = here::here("data/original"))
#r_tmax <- geodata::worldclim_country(country = "US", var = "tmax", res = 0.5, path = here::here("data/original"))
#r_ele <- geodata::elevation_30s(country = "US", path = here::here("data/original/"))
#r_tt <- geodata::travel_time(to = "city", size = 7, up = TRUE, path = here::here("data/original/"))

r_prec <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_prec.tif"))
r_tmin <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmin.tif"))
r_tmax <- rast(here::here("data/original/climate/wc2.1_country/USA_wc2.1_30s_tmax.tif"))
r_ele <- rast(here::here("data/original/elevation/USA_elv_msk.tif"))
r_tt <- rast(here::here("data/original/travel/travel_time_to_cities_u7.tif"))


# 2. Crop to CONUS, because the files are global, it is more efficient to crop to the extant of conus states first
#-------------------------------------------------------------------------------
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

# Crop and mask to conus then project to reference raster
tt_crop <- crop(r_tt, conus_for_crop, mask = TRUE)
#tt_proj <- project(tt_crop, ref_rast)
tt_proj <- project(r_tt, ref_rast)
plot(tt_proj)

# use a while loop to fill in NAs at edges of Conus with focal window
tt_w <- 1
tt_filled <- tt_proj # just in case you want to keep the original
tt_to_fill <- any(is.na(values(tt_filled)))

while(tt_to_fill) {
  tt_w <- tt_w + 2  
  tt_filled <- focal(tt_filled, w = tt_w, fun = mean, na.policy = "only", na.rm = T) 
  tt_to_fill <- any(is.na(values(tt_filled)))
}
print(tt_w)

tt_filled_crop <- crop(tt_filled, ref_rast, mask = TRUE)
plot(tt_filled_crop)
nrow(as.data.frame(tt_filled_crop))
nrow(as.data.frame(ref_rast))

ele_proj <- project(r_ele, ref_rast)

# use a while loop to fill in NAs at edges of Conus with focal window
ele_w <- 1
ele_filled <- ele_proj # just in case you want to keep the original
ele_to_fill <- any(is.na(values(ele_filled)))

while(ele_to_fill) {
  ele_w <- ele_w + 2  
  ele_filled <- focal(ele_filled, w = ele_w, fun = mean, na.policy = "only", na.rm = T) 
  ele_to_fill <- any(is.na(values(ele_filled)))
}
print(ele_w)

ele_filled_crop <- crop(ele_filled, ref_rast, mask = TRUE)
plot(ele_proj)
plot(ele_filled_crop)
nrow(as.data.frame(ele_filled_crop))
nrow(as.data.frame(ref_rast))

roughness <- terrain(ele_proj, v = "roughness")
# use a while loop to fill in NAs at edges of Conus with focal window
rough_w <- 1
rough_filled <- roughness # just in case you want to keep the original
rough_to_fill <- any(is.na(values(rough_filled)))

while(rough_to_fill) {
  rough_w <- rough_w + 2  
  rough_filled <- focal(rough_filled, w = rough_w, fun = mean, na.policy = "only", na.rm = T) 
  rough_to_fill <- any(is.na(values(rough_filled)))
}
print(rough_w)

rough_filled_crop <- crop(rough_filled, ref_rast, mask = TRUE)
plot(roughness)
plot(rough_filled_crop)
nrow(as.data.frame(rough_filled_crop))
nrow(as.data.frame(ref_rast))

# Calculate the biovars using prec, tmax, and tmin

prec_crop <- crop(r_prec, conus_for_crop, mask = TRUE)
prec_proj <- project(prec_crop, ref_rast)
#prec_proj <- project(r_prec, ref_rast)

tmax_crop <- crop(r_tmax, conus_for_crop, mask = TRUE)
tmax_proj <- project(tmax_crop, ref_rast)
#tmax_proj <- project(r_tmax, ref_rast)

tmin_crop <- crop(r_tmin, conus_for_crop, mask = TRUE)
tmin_proj <- project(tmin_crop, ref_rast)
#tmin_proj <- project(r_tmin, ref_rast)

# I think they all need to be cropped to conus in order to make a raster brick?
# or not.
# They need to be cropped to conus in order to perfectly match the rasters made
# in Oct 2024.
bio_calcs <- biovars(brick(prec_proj), brick(tmin_proj), brick(tmax_proj))

temp_seas <- rast(bio_calcs$bio4)
prec_seas <- rast(bio_calcs$bio15)

# use focal while loop to fill in NAs around the edge of conus
temp_w <- 1
temp_filled <- temp_seas # just in case you want to keep the original

temp_to_fill <- any(is.na(values(temp_filled)))

while(temp_to_fill) {
  temp_w <- temp_w + 2  
  temp_filled <- focal(temp_filled, w = temp_w, fun = mean, na.policy = "only", na.rm = T) 
  temp_to_fill <- any(is.na(values(temp_filled)))
}
print(temp_w)

temp_filled_crop <- crop(temp_filled, ref_rast, mask = TRUE)
plot(temp_seas)
plot(temp_filled_crop)
nrow(as.data.frame(temp_filled_crop))
nrow(as.data.frame(ref_rast))

# use focal while loop to fill in NAs around the edge of conus
prec_w <- 1
prec_filled <- prec_seas # just in case you want to keep the original

prec_to_fill <- any(is.na(values(prec_filled)))

while(prec_to_fill) {
  prec_w <- prec_w + 2  
  prec_filled <- focal(prec_filled, w = prec_w, fun = mean, na.policy = "only", na.rm = T) 
  prec_to_fill <- any(is.na(values(prec_filled)))
}
print(prec_w)

prec_filled_crop <- crop(prec_filled, ref_rast, mask = TRUE)
plot(prec_filled_crop)
plot(prec_seas)
nrow(as.data.frame(prec_filled_crop))
nrow(as.data.frame(ref_rast))

# Save the rasters (during the resample they were aggregated to 3km)
writeRaster(tt_filled_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/trav_time_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(rough_filled_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/roughness_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(ele_filled_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/elevation_3000m_", 
                                      Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(prec_filled_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/prec_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)
writeRaster(temp_filled_crop, paste0("/Users/katiemurenbeeld/Analysis/Archetype_Analysis/data/processed/temp_seas_3000m_", 
                               Sys.Date(), ".tif"), overwrite = TRUE)



## Check that the new rasters (without cropping to conus_for_crop) are the same
## as the old rasters

tt_filled_crop_old <- rast(here::here("data/processed/variables/trav_time_3000m_2024-10-03.tif"))
tt_filled_crop == tt_filled_crop_old
plot(tt_filled_crop)
plot(tt_filled_crop_old)
all.equal(tt_filled_crop, tt_filled_crop_old)
compareGeom(tt_filled_crop, tt_filled_crop_old)

rough_filled_crop_old <- rast(here::here("data/processed/variables/roughness_3000m_2024-10-07.tif"))
rough_filled_crop == rough_filled_crop_old
plot(rough_filled_crop)
plot(rough_filled_crop_old)
all.equal(rough_filled_crop, rough_filled_crop_old, tolerance = 1e-4)
compareGeom(rough_filled_crop, rough_filled_crop_old)
rough_e <- rough_filled_crop == rough_filled_crop_old
plot(rough_e)
# I think this is close enough given that the range of values is from 0 to >2000 m
# Also elevation from Oct 2024 was not cropped to conus_for_crop

prec_filled_crop_old <- rast(here::here("data/processed/variables/prec_seas_3000m_2024-10-07.tif"))
prec_filled_crop == prec_filled_crop_old
plot(prec_filled_crop)
plot(prec_filled_crop_old)
compareGeom(prec_filled_crop, prec_filled_crop_old)
all.equal(prec_filled_crop, prec_filled_crop_old, tolerance = 2e-6) 
# tolerance = 0.0005 when not cropped to conus_for_crop
# tolerance = 0.000002 when cropped to conus_for_crop

temp_filled_crop_old <- rast(here::here("data/processed/variables/temp_seas_3000m_2024-10-07.tif"))
temp_filled_crop == temp_filled_crop_old
plot(temp_filled_crop)
plot(temp_filled_crop_old)
compareGeom(temp_filled_crop, temp_filled_crop_old)
all.equal(temp_filled_crop, temp_filled_crop_old)
# tolerance = 0.0003 when not cropped to conus_for_crop
# tolerance = 0.000002 when cropped to conus_for_crop
