# Load the required packages
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(geocmeans)
library(future)
library(foreach)
library(doParallel)


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
future::plan(future::multisession(workers = 48))

## Because we are using scaled data, set standardize = FALSE,
## Make sure to set a seed
## Select a range of k from the elbow plots, a sequence of m values from 
## 1.1 to 1.5 with steps of 0.1, and a sequence of beta values from 0.1-1.0,0.1
## Select the Xie-Beni, Explained Inertia, Negentropy, and Silhouette Index 
## as cluster evaluation metrics.
## spconsist = FALSE because this is a non-spatial model.

# Set a SLURM array task ID to aid in the parallelization of the parameter 
# selection grid search.

task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (task_id == "") {
  task_id <- 1
} else {
        task_id <- as.numeric(task_id)
        task_id <- task_id + 50  
}

print(task_id)

# k = 2:108

FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, standardize = FALSE,
                                  k = task_id, m = seq(1.1,2,0.1), spconsist = FALSE,
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Silhouette.index",
                                              "Negentropy.index", "DaviesBoulin.index"),
                                  seed = 1234, verbose = TRUE)

write_csv(FCMvalues, paste0("/bsuhome/katiemurenbeeld/scratch/archetype_analysis/outputs/nfbuffers_fcm_all_attri_param_indices_crop_then_scale_array_sbatch_",
                                  task_id, "_",  Sys.Date(), ".csv"), append = FALSE)
