################################################################################
# SCRIPT TO DETEMINE THE NUMBER OF UNIQUE FEDERAL AGENCIES IN A 3KM GRID CELL ##
# USING THE PADUS (PROTECTED AREAS DATABASE OF THE US) GEODATABASE            ##
# 1. Load the data                                                            ##
#  1.1 Create a list of states in the contiguous US (conus)                   ##
#  1.2 Filter the data for conus and management type == Federal               ##
# 2. Create a 3km empty grid shapefile                                        ##
#  2.1 Download the county boundaries for state in conus                      ##
#  2.2 From the county boundaries create a 3km grid                           ##
#  2.3 Add a unique cell if for the empty grid shapefile                      ##
#  2.4 From the grid shapefile create an empty raster which will be used at   ##
#      the end to rasterize the data                                          ##
# 3. Clean up the PADUS data                                                  ##
#    Make the sf package use planar geometry, reporject, and make multisurface##
#    into multipolygons                                                       ##
#  3.1 Check for and fix invalid or empty geometries                          ##
# 4. Calculate Federal Richness (number of unique Federal agencies in a 3km   ##
#    pixel)                                                                   ##
#  4.1 Subset the data                                                        ##
#  4.2 Find where the padus data intersects with the empty grid shapefile     ##
#  4.3 For each unique grid cell get the intersections of the empty grid to the# 
#      PADUS shapefile                                                        ##
#  4.4 Summarise the data, for each grid cell get the number of unique agencies#
# 5. Rasterize the data                                                       ##
# 6. Save the raster                                                          ##
################################################################################

# 0. Load the libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(tigris)
library(tmap)
library(raster)
library(units)
library(purrr)
library(progress)

## set the projection
projection = "epsg:5070"

# 1. Load padus data and get the Fee layer
#-------------------------------------------------------------------------------
fed <- st_read(here::here("data/original/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb"),
               layer = "PADUS4_0Fee") 

## 1.1 
## create the list of CONUS states 
us.abbr <- unique(fips_codes$state)[1:51]
us.name <- unique(fips_codes$state_name)[1:51]
us.fips <- unique(fips_codes$state_code)[1:51]

us.states <- as.data.frame(cbind(us.abbr, us.name, us.fips))
colnames(us.states) <- c("state", "STATENAME", "FIPS")
us.states$state <- as.character(us.states$state)
us.states$STATENAME <- as.character(us.states$STATENAME)
### only CONUS
continental.states <- us.states[us.states$state != "AK" & us.states$state != "HI",] 

## 1.2
## Filter the data for conus and management type == Federal
conus_fed <- fed %>%
  filter(State_Nm %in% continental.states$state) %>%
  filter(Mang_Type == "FED")

## clear memory
rm(fed)

# 2. Create a 3km grid for conus
#-------------------------------------------------------------------------------
## 2.1
## Download the county boundaries and filter by states
counties <- tigris::counties(state = continental.states$state, cb = TRUE)

## reproject
counties_proj <- st_transform(counties, crs = projection)

## 2.2
## use st_make_grid to create a 3km grid
conus_cells <- st_make_grid(counties_proj, cellsize = 3000)
conus_cells_sf <- st_sf(conus_cells) 

## 2.3
## add a unique cell id which is needed to keep track of intersections between
## the PADUS data and the empty grid shapefile we are creating in this section
conus_cells_sf <- conus_cells_sf %>% 
  mutate(GRIDCELL_REFID = as.character(row_number()))

## 2.4
## Create a template raster using the conus_cells_sf
## This will be used to rasterize the data at the end
XMIN <- ext(conus_cells_sf)$xmin
XMAX <- ext(conus_cells_sf)$xmax
YMIN <- ext(conus_cells_sf)$ymin
YMAX <- ext(conus_cells_sf)$ymax
aspectRatio <- (YMAX-YMIN)/(XMAX-XMIN)
cellSize <- 3000
NCOLS <- as.integer((XMAX-XMIN)/cellSize)
NROWS <- as.integer(NCOLS * aspectRatio)
conus_cells_rst <- rast(ncol=NCOLS, nrow=NROWS, 
                        xmin=XMIN, xmax=XMAX, ymin=YMIN, ymax=YMAX,
                        vals=1, crs=projection)

# 3. Clean up padus data
#-------------------------------------------------------------------------------
# this helps to solve some geometry issues, makes the sf package use 
# lat/lon (planar geometry)
sf_use_s2(FALSE)   

# Reproject the conus_fed
conus_fed_proj <- conus_fed %>% st_transform(., crs = projection)

# Make the multisurface into multipolygons
conus_fed_proj <- conus_fed_proj %>% st_cast("MULTIPOLYGON")

## 3.1
## Check for invalid geometries and fix
all(st_is_valid(conus_fed_proj))
conus_fedp_val <- st_make_valid(conus_fed_proj)
all(st_is_valid(conus_fedp_val))

## Check for and remove empty geometries
all(st_is_empty(conus_fedp_val))

# 4. Calculate Federal Richness (number of unique Federal agencies in a 3km 
#    pixel)
#-------------------------------------------------------------------------------
## 4.1
## Subset the data to calculate Federal Richness (number of unique Federal
## agencies in a 3km pixel)
conus_fed_proj_name <- conus_fedp_val %>%
  dplyr::select(Mang_Name) # for Fed richness

## 4.2
## Find where the padus data intersects with the empty grid shapefile
intersections_rich <- st_intersects(conus_cells_sf, conus_fed_proj_name)

### save in case you need to start over from here
saveRDS(intersections_rich, 
        here::here("data/processed/intersections_rich.RDS")) 

### For the next step, use a progress bar to track the progress
pb2 <- progress_bar$new(format = "[:bar] :current/:total (:percent)", 
                        total = dim(conus_cells_sf)[1])

## 4.3
## For each unique grid cell get the intersections of the empty grid to the 
## PADUS shapefile
intersectFeatures_rich <- map_dfr(1:dim(conus_cells_sf)[1], function(ix){
  pb2$tick()
  st_intersection(x = conus_cells_sf[ix,], 
                  y = conus_fed_proj_name[intersections_rich[[ix]],])
})

### save in case you need to start over from here
saveRDS(intersectFeatures_rich, 
        here::here("data/processed/fed_rich/intersectFeatures_rich.RDS"))

## 4.4
## Summarise the data, for each grid cell get the number of unique agencies 
intersectFeatures_richness <- intersectFeatures_rich %>%
  group_by(., GRIDCELL_REFID) %>%
  summarise(., numfed = n())

# 5. Rasterize the data
#-------------------------------------------------------------------------------
intersectFeatures_rich_rst <- rasterize(intersectFeatures_richness, 
                                        conus_cells_rst, 
                                        field = "numfed")

## quickly check a plot of the raster
plot(intersectFeatures_rich_rst)

# 6. Save the raster
#-------------------------------------------------------------------------------
writeRaster(intersectFeatures_rich_rst, 
            here::here(paste0("data/processed/fed_rich/conus_fed_rich_", 
                              Sys.Date(), 
                              ".tif")), 
            overwrite = TRUE)
