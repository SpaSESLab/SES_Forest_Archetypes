################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NF LEVEL SPATIAL DATA ##
# 1. Download and combine all nfbuffers_sgfcm_*.csv results                   ##
# 2. Filter the results for explained inertia and silhouette index values     ##
# 3. Write the table or csv with the final parameter options                  ##
################################################################################

# 0. Load libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)
library(scales)

# 1. Download and combine the parameter selection results
#    for some reason the csvs don't include a column for the beta value. 
#    this happens with the national level data as well.
#-------------------------------------------------------------------------------
## Get a list of all the files in the directory
file_list <- list.files(path = here::here("outputs/nf_level/sgfcm/"),
                        # the window sizes are mislabeled in the csvs but
                        # source_file 1 is w1, source_file 2 is w2, etc.
                        pattern = "w1", 
                        full.names = TRUE)

## Read all files and combine into a dataframe
nf_sgfcm_df <- map_dfr(file_list, read_csv, .id = "source_file")

# 2. Filter the results for explained inertia and silhouette index values
#-------------------------------------------------------------------------------

## Explained inertia >= 0.50 (rounded)
df_ei05 <- nf_sgfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2)) %>%
  filter(Explained.inertia >= 0.5)

## Explained inertia >= 0.50 and silhouette index >= 0.3
df_ei05_si03 <- nf_sgfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3)

## Explained inertia >= 0.50, silhouette index >= 0.3, and spatial inconsistency
## < 1.0
df_ei05_si03_sp <- nf_sgfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2), 
         spConsistency = round(spConsistency, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3 & spConsistency < 20.0)

## Explained inertia >= 0.60 and silhouette index >= 0.3
df_ei06_si03 <- nf_sgfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.6 & Silhouette.index >= 0.3)

## We don't actually need plots here since there is only one option where
## EI >=0.6, SI>=0.3 and Spatial Inconsistency is <Inf

# 3. Write out the table with the final parameter values
#-------------------------------------------------------------------------------
write_csv(df_ei06_si03, here::here(paste0("outputs/nf_level/tables/nf_level_sgfcm_w1_parma_ei06_si03_",
                                          Sys.Date(), ".csv")))


