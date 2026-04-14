################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NF LEVEL SPATIAL DATA ##
# 1. Download and combine all nfbuffers_fcm_*.csv results                     ##
# 1.1 Note that the xxx.R script was run on a high performance computer       ##
# 2. Filter for parameter combinations with explained inertia >= 0.5          ##
# 3. Plot the evaluation metrics by k with a facet wrap by m.                 ##
# 4. Save the figure                                                          ##
#                                                                             ##
################################################################################

# 0. Load libraries
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)


# 1. Download and combine the parameter selection results
#-------------------------------------------------------------------------------
## Get a list of all the files in the directory
file_list <- list.files(path = here::here("outputs/nf_level/fcm/"), 
                        pattern = "*.csv", 
                        full.names = TRUE)

## Read all files and combine into a dataframe
nf_fcm_df <- map_dfr(file_list, read_csv, .id = "source_file")

# 2. Filter for Explained Inertia >= 0.50 (rounded)
#-------------------------------------------------------------------------------

df_ei05 <- nf_fcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2)) %>%
  filter(Explained.inertia >= 0.5)

df_ei05_si03 <- nf_fcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3)

# 3. Plot Explained Inertia and Silhouette Index y with k 
# on x and facet wrapped by m
#-------------------------------------------------------------------------------

df <- df_ei05_si03

ei <- ggplot(df, aes(k, Explained.inertia)) + 
  geom_point() +
  facet_wrap(~m) +
  labs(title = "FCM Evaluation Metrics: Explained Inertia >= 0.5 & Silhouette Index >= 0.3")

si <- ggplot(df, aes(k, Silhouette.index)) + 
  geom_point() +
  facet_wrap(~m)

ei
si

ei / si

# 4. Save the figure
#-------------------------------------------------------------------------------

fmc_plot <- ei / si 

ggsave(here::here(paste0("outputs/nf_level/plots/nf_buffers_fcm_params_ei05_si03_",
                         Sys.Date(), ".jpeg")),
       width = 8, height = 6, dpi = 300)












