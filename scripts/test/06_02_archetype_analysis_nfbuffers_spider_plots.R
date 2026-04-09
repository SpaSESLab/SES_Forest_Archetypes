################################################################################
# SCRIPT TO GENERATE THE RADAR CHARTS ("SPIDER PLOTS") AND CONDENSE CLUSTERS  ##
# 1. Load the data                                                            ##                                   ##
# 2. 
################################################################################


# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(geocmeans)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)
library(spdep)
library(tmap)

spdep::set.mcOption(FALSE)
spdep::set.coresOption(1L)


# 1. Load data (we need the data and the model result)
#-------------------------------------------------------------------------------
## Load the scaled attribute data
Data_nf <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))
## the data needs to be a data frame
Data <- as.data.frame(Data_nf, xy = FALSE)

## Load the final nation forest level model
SGFCM <- readRDS(here::here("outputs/SGFCM_nf_buffers_k27_2026-03-25.rds"))

# 2. From the SGFCM belongings matrix, define the variables needed for the function
#-------------------------------------------------------------------------------
# Optional: can quickly plot all of the spider (radial) plots for each cluster 
spiderPlots(Data, SGFCM$Belongings)

belongmatrix <- SGFCM$Belongings
data <- Data
Groups <- ncol(belongmatrix)

Values <- do.call(rbind, lapply(1:Groups, function(i) {
  W <- belongmatrix[,i]
  return(apply(data,2, function(row){return(weighted.mean(row,W))}))
}))

Mins <- apply(Values, 2, min)
Maxs <- apply(Values, 2, max)

# 3. Create the function (or call in the function)
#-------------------------------------------------------------------------------
# This needs more commenting/documentation

create_poly <- function(group_number) {
  # This creates the data frame for use in the radarchart code
  # which requires the min, max, and weighted average score 
  # for each variable for each cluster/group
  Scores <- Values[group_number,]
  names(Scores) <- names(data)
  datam <- data.frame(rbind(Maxs, Mins, Scores))
  
  # code for getting the points for the radarchart
  n <- length(datam)
  theta <- seq(90, 450, length = n + 1)* (pi/180)
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(FALSE, 0, 1) #geocmeans uses centerzero=FALSE
  # This creates the guidelines for the radarchart
  # This is needed to scale the values to the radarchart
  # because CGap is FALSE we need to set up the segments and some scaling
  seg <- 4 # usually 4 segments labeled 25%, 50%, 75%, 100%
  series <- length(datam[[1]])
  SX <- series-2
  #df <- datam
  # sets up the scaling
  for (i in 3:series) {
    #print(i)
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg+CGap)+(datam[i,]-datam[2,])/(datam[1,]-datam[2,])*seg/(seg+CGap)
  }
  # calculate the x and y for each variable, scaled to the radarchart
  for (j in 1:n) {
    xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(datam[i, j]-datam[2, j])/(datam[1, j]-datam[2, j])*seg/(seg+CGap)
    yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(datam[i, j]-datam[2, j])/(datam[1, j]-datam[2, j])*seg/(seg+CGap)
  }
  
  # create a matrix of the scaled coordinate points for the resulting polygon
  ply_mat <- cbind(xxs, yys)
  # need to close the polygon so add the first rows coords to the bottom
  ply_mat_closed <- rbind(ply_mat, ply_mat[1,])
  # create a list object
  ply_list <- list(ply_mat_closed)
  
  # from the polygon list object create sf polygons
  st_geom <- st_polygon(ply_list)
  st_sfc_geom <- st_sfc(st_geom)
  clusters_df <- data.frame(cluster = group_number)
  sf_polygon <- st_as_sf(clusters_df, geometry = st_sfc_geom)
  
  # return the final sf_polygon
  return(sf_polygon) 
}

# 4. Use the function to create the sf polygons and calculate the overlapping
##   areas for the polyons
#-------------------------------------------------------------------------------
## Here, create an empty matrix and use a for loop to fill it in the 
## % area overlap for each combination 

## Create empty matrix with nrow and ncol = to the number of groups or clusters
my_matrix_all <- matrix(data = NA, nrow = 27, ncol = 27)
print("Initial Matrix:")
print(my_matrix_all)

## Use a for loop to fill in the matrix with the pct area covered by the 
## area of intersection for each polygon pair

for (i in 1:nrow(my_matrix_all)){
  for (j in 1:ncol(my_matrix_all)) {
    print(c(i, j))
    # create the sf polygon
    poly_tmp <- create_poly(i)
    # create another sf polygon
    poly_tmp2 <- create_poly(j) 
    # calculate the overlap
    overlap_tmp <- st_intersection(poly_tmp, poly_tmp2) 
    # calculate the area of the overlap
    area_tmp <- st_area(overlap_tmp) 
    # calculate the % area of first polygon covered by the area of intersection
    pct_covered <- area_tmp / st_area(poly_tmp) * 100 
    # fill in the matrix with the % area covered
    my_matrix_all[i,j] <- pct_covered
  }
}

print(my_matrix_all)

# 5. Save the matrix as a csv
#-------------------------------------------------------------------------------
write.csv(as.data.frame(my_matrix_all),
            file = here::here("outputs/sgfcm_nfbuffers_spider_plot_overlap_matrix_test.csv"))

# 6. Create a matrix with only the instances where [i,j] & [j,i] are both >75%
#-------------------------------------------------------------------------------
# from this matrix I want to find the combinations where
# both [i,j] and [j,i] are over 75%

## create a boolean mask
mask <- matrix(data = NA, nrow = 27, ncol = 27)

## decide on the threshold
t <- 80

for (i in 1:27){
  for (j in 1:27){
    print(c(i,j))
    i_j_j_i_true <- my_matrix_all[i,j] > t & my_matrix_all[j,i] > t
    mask[i,j] <- i_j_j_i_true
  }
}

## apply the mask to the matrix
masked_matrix <- my_matrix_all
masked_matrix[!mask] <- 0
print(masked_matrix)

## save the masked matrix as a csv
write.csv(as.data.frame(masked_matrix),
          file = here::here("outputs/sgfcm_nfbuffers_spider_plot_overlap_matrix_test_mask.csv"))


