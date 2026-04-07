# Load the required packages
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(geocmeans)
#library(future)
#library(foreach)
#library(doParallel)

#options(future.globals.maxSize = 768 * 1024^2)
#options(future.globals.onReference = "error")

# Load the spatraster. Note that geocmeans automatically ignores NA raster pixels for the analysis
r <- rast("/bsuhome/katiemurenbeeld/scratch/archetype_analysis/data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif")

# Format the data for use in geocmeans
dataset <- lapply(names(r), function(n){
  aband <- r[[n]]
  return(aband)
})
names(dataset) <- names(r)

# ------------------------------------------------------------------------------
## Set the number of workers since we will use the multicore option of the
## select_parameters.mc() function
#future::plan(future::multisession(workers = 45))

## Because we are using scaled data, set standardize = FALSE,
## Make sure to set a seed
## Use k = 27, m = 1.2, and select a range of alpha and beta values.
## Select the Xie-Beni. Explained Inertia, Negentropy, and Silhouette Index 
## as cluster evaluation metrics.
## spconsist = TRUE because this is a spatial model.

# Set a SLURM array task ID to aid in the parallelization of the parameter 
# selection grid search.

#task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
#if (task_id == "") {
#  task_id <- 42
#} else {
#        task_id <- as.numeric(task_id)
#        task_id <- task_id + 42  
#}

#print(task_id)

# k = 27, m = 1.2, beta = seq(0.1,0.5,0.1) alpha = seq(0.1,0.5,0.1)
# window sizes = 3x3, 5x5, 7x7

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SGFCMvalues <- select_parameters.mc(algo = "SGFCM", data = dataset, standardize = FALSE,
                                  k = 27, 
				  m = 1.2, 
				  beta = seq(0.1,0.5,0.1),
				  alpha = seq(0.1,0.5, 0.1), 
				  window = w3,
				  spconsist = TRUE,
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index",
                                              "Negentropy.index", "DaviesBoulin.index"),
                                  seed = 1234, verbose = TRUE)

dict <- data.frame(
		   w = c(1,2,3),
		   window = c("3x3", "5x5", "7x7")
		   )

SGFCMvalues$window <- dict$window[match(SGFCMvalues$window, dict$w)]

write_csv(SGFCMvalues, paste0("/bsuhome/katiemurenbeeld/scratch/archetype_analysis/outputs/nfbuffers_sgfcm_all_attri_param_indices_crop_then_scale_k27_w3_",  Sys.Date(), ".csv"), append = FALSE)
