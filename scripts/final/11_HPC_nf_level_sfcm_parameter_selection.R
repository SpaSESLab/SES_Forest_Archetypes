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
## Select a range of k, m, and beta from the GFCM results
## Select the Xie-Beni. Explained Inertia, Negentropy, and Silhouette Index 
## as cluster evaluation metrics.
## spconsist = TRUE because this is a spatial model.

# Set a SLURM array task ID to aid in the parallelization of the parameter 
# selection grid search.

task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (task_id == "") {
  task_id <- 42
} else {
        task_id <- as.numeric(task_id)
        task_id <- task_id + 42  
}

print(task_id)

# k = 19:30, m = 1.2, beta = 0.1:0.2 (no beta for SFCM! only for SGFCM!)
# k = 19:30, m = 1.3, beta = 0.5:0.6 (no beta for SFCM! only for SGFCM!)
# k = 43:53, m = 1.3, beta = 0.2 (no beta for SFCM! only for SGFCM!)
# k = 65:70, m = 1.3, beta = 0.2 (no beta for SFCM! only for SGFCM!)
# k = 97:102, m = 1.3, beta = 0.2 (no beta for SFCM! only for SGFCM!)
# high k, k = 97:102
# alpha = seq(0.1,2.0,0.1)
# window sizes = 3x3, 5x5, 7x7

w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

SFCMvalues <- select_parameters.mc(algo = "SFCM", data = dataset, standardize = FALSE,
                                  k = task_id, 
				  m = seq(1.2,1.3,0.1), 
				  #beta = seq(0.1,0.6,0.1),
				  alpha = seq(0.1, 2.0, 0.1), 
				  window = w1,
				  spconsist = TRUE,
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index",
                                              "Negentropy.index", "DaviesBoulin.index"),
                                  seed = 1234, verbose = TRUE)

dict <- data.frame(
		   w = c(1,2,3),
		   window = c("3x3", "5x5", "7x7")
		   )

SFCMvalues$window <- dict$window[match(SFCMvalues$window, dict$w)]

write_csv(SFCMvalues, paste0("/bsuhome/katiemurenbeeld/scratch/archetype_analysis/outputs/nfbuffers_sfcm_all_attri_param_indices_crop_then_scale_array_sbatch_",
                                  task_id, "_w1_",  Sys.Date(), ".csv"), append = FALSE)
