################################################################################
#----Distance to mills and Mill Capacity "hotspots"
# Pseudocode:
# 1. Load the data (FORISK and county boundaries)
# 2. Align the data
# 3. Rasterize data
# 4. Aggregate to 1.5km and 3km
# 5. Calculate distance from mill points
# 6. Mill capacity metric
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)
library(tmap)
library(patchwork)
library(units)
library(spdep)
library(spatstat)
library(gstat)

# Set the projection
projection <- "epsg:5070"

# load and reproject the raster data
ref_rast <- rast(here::here("data/processed/variables/conus_whp_3km_agg_interp_crop_2024-09-27.tif"))

# 1. Load the data and filter for states in the contiguous US (conus)
#-------------------------------------------------------------------------------
mill_sf <- read_sf(here::here("data/original/forisk_private/Forisk_NA_FI_Capacity_DB_2024_Q1.shp"))


## Get Continental US list of states
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name))
colnames(us.states) <- c("state","STATENAME")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI" & us.states$state != "DC",] #only CONUS

## From the mills shapefile remove the Canadian regions and filter for states in
## the continental US
conus_mills <- mill_sf %>%
  filter(Region != "Canada West") %>%
  filter(Region != "Canada East") %>%
  # need to keep all mills in order to calculate change in mill capacity for 
  # mills that have closed
  filter(State_Prov %in% continental.states$state)

# 2. Calculate the change in mill capacity from 2019-2023 (5 years)
#-------------------------------------------------------------------------------
## For mills that came "online" set the CPT_2019 to 1.0
conus_mills$CPT_2019[conus_mills$CPT_2019 == 0.0] <- 1.0 

conus_mills <- conus_mills %>%
  mutate(millcap_5yr = (CPT_2023 - CPT_2019) / CPT_2019)

## Check for missing data
print ("Row and Col positions of NA values") 
which(is.na(conus_mills$millcap_5yr), arr.ind = TRUE)

# 3. Interpolate change in mill capacity
#-------------------------------------------------------------------------------

## 3.1 
## Create a template raster for the shapefiles
XMIN <- ext(ref_rast)$xmin
XMAX <- ext(ref_rast)$xmax
YMIN <- ext(ref_rast)$ymin
YMAX <- ext(ref_rast)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
templateRas <- rast(ncol=NCOLS, nrow=NROWS, 
                    xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                    vals=1, crs=crs(ref_rast))
grd <- st_as_stars(templateRas)

## Create a tesselation from the template grid
nodes <- st_make_grid(grd,
                       n = c(100, 100),
                       what = "centers")

## remove mills that have the same lat, lon
conus_mills_nodeupe <- conus_mills[!duplicated(conus_mills[,22:23]),]
conus_mills_nodeupe_proj <- st_transform(conus_mills_nodeupe, projection)

## 
dist <- distance(vect(nodes), vect(conus_mills_nodeupe_proj))
nearest_conus <- apply(dist, 1, function(x) which(x == min(x)))
millcap5.nn <- conus_mills_nodeupe_proj$millcap_5yr[nearest_conus]
preds <- st_as_sf(nodes)
preds$millcap5 <- millcap5.nn
preds <- as(preds, "Spatial")
sp::gridded(preds) <- TRUE
preds.rast <- rast(preds)
plot(preds.rast$millcap5)

## Create the gstat object that will be used for geostatistical prediction
mc5sf2 <- gstat(id = "millcap_5yr", 
                formula = millcap_5yr~1, 
                data=conus_mills_nodeupe_proj,  
                nmax=7, 
                set=list(idp = 2))

## Create a custom function to use in interpolate() that will 
## turn the coordiantes to spatial points, predict the values at those points,
## and return the predictions in a data frame
interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

## Use terra::interpolate() with the custom function
zmc5sf2 <- interpolate(preds.rast, 
                       mc5sf2, 
                       debug.level=0, 
                       fun=interpolate_gstat, 
                       crs=crs(projection), 
                       index=1)

# 4. Rasterize the interpolated 
#-------------------------------------------------------------------------------
## Resample and crop the mill capacity change to the reference raster
zmc5sf2_resamp <- resample(zmc5sf2, ref_rast, "bilinear")
zmc5sf2_crop <- crop(zmc5sf2_resamp, ref_rast, mask = TRUE)

# quickly check the plot and that the nrows matches the nrows of the ref_raster
plot(zmc5sf2_crop)
nrow(as.data.frame(zmc5sf2_crop))

# 5. Save the raster
#-------------------------------------------------------------------------------

writeRaster(zmc5sf2_crop, here::here(paste0("data/processed/variables/millchange_interp-2_",
                                 Sys.Date(), ".tif")), overwrite = TRUE)

