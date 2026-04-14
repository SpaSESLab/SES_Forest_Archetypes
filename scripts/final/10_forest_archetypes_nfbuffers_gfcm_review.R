################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NF LEVEL SPATIAL DATA ##
# 1. Download and combine all nfbuffers_gfcm_*.csv results                     ##
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
library(scales)

# 1. Download and combine the parameter selection results
#-------------------------------------------------------------------------------
## Get a list of all the files in the directory
file_list <- list.files(path = here::here("outputs/nf_level/gfcm/"), 
                        pattern = "*.csv", 
                        full.names = TRUE)

## Read all files and combine into a dataframe
nf_gfcm_df <- map_dfr(file_list, read_csv, .id = "source_file")

# 2. Filter for Explained Inertia >= 0.50 (rounded)
#-------------------------------------------------------------------------------

df_ei05 <- nf_gfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2)) %>%
  filter(Explained.inertia >= 0.5)

df_ei05_si03 <- nf_gfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.5 & Silhouette.index >= 0.3)

df_ei06_si03 <- nf_gfcm_df %>%
  mutate(Explained.inertia = round(Explained.inertia, digits = 2),
         Silhouette.index = round(Silhouette.index, digits = 2)) %>%
  filter(Explained.inertia >= 0.6 & Silhouette.index >= 0.3)

# 3. Plot Explained Inertia and Silhouette Index y with k 
# on x and facet wrapped by m
#-------------------------------------------------------------------------------

df <- df_ei06_si03

ei <- ggplot(df, aes(k, Explained.inertia)) + 
  geom_point(size = 0.5, color="red") +
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "GFCM Evaluation Metrics: Explained Inertia >= 0.6 \n & Silhouette Index >= 0.3")

si <- ggplot(df, aes(k, Silhouette.index)) + 
  geom_point(size = 0.5, color="blue") +
  facet_grid(rows = vars(beta), cols = vars(m))

ei
si

ei / si

# 4. Save the figure
#-------------------------------------------------------------------------------

gfmc_plot <- ei / si 

ggsave(here::here(paste0("outputs/nf_level/plots/nf_buffers_gfcm_params_ei06_si03_",
                         Sys.Date(), ".jpeg")), plot = gfmc_plot,
       width = 8, height = 8, dpi = 300)

# 5. Function and code for plotting with 2 y-axis
#-------------------------------------------------------------------------------
# From https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales/66055331#66055331
# Function factory for secondary axis transforms
train_sec <- function(primary, secondary, na.rm = TRUE) {
  # Thanks Henry Holm for including the na.rm argument!
  from <- range(secondary, na.rm = na.rm)
  to   <- range(primary, na.rm = na.rm)
  # Forward transform for the data
  forward <- function(x) {
    rescale(x, from = from, to = to)
  }
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    rescale(x, from = to, to = from)
  }
  list(fwd = forward, rev = reverse)
}

sec1 <- with(df_ei05, train_sec(Explained.inertia, Silhouette.index))
sec2 <- with(df_ei05_si03, train_sec(Explained.inertia, Silhouette.index))
sec3 <- with(df_ei06_si03, train_sec(Explained.inertia, Silhouette.index))

ei05_si_scaled_y <- ggplot(df_ei05, aes(k)) +
  geom_point(aes(y = Explained.inertia), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = sec1$fwd(Silhouette.index)), colour = "red", alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~sec1$rev(.), name = "Silhouette.index")) + 
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM: Explained Inertia >= 0.5")


ei05_si03_scaled_y <- ggplot(df_ei05_si03, aes(k)) +
  geom_point(aes(y = Explained.inertia), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = sec2$fwd(Silhouette.index)), colour = "red", alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~sec2$rev(.), name = "Silhouette.index")) + 
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM: Explained Inertia >= 0.5 & \nSilhouette Index >= 0.3")

ei06_si03_scaled_y <- ggplot(df_ei06_si03, aes(k)) +
  geom_point(aes(y = Explained.inertia), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = sec3$fwd(Silhouette.index)), colour = "red", alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~sec3$rev(.), name = "Silhouette.index")) + 
  facet_grid(rows = vars(beta), cols = vars(m)) +
  labs(title = "National Level GFCM: Explained Inertia >= 0.6 & \nSilhouette Index >= 0.3")

# 6. Save the figures with the dual y-axis
#-------------------------------------------------------------------------------

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_gfcm_parma_ei05_si_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei05_si_scaled_y,
       width = 10, height = 8, dpi = 300)

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_gfcm_parma_ei05_si03_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei05_si03_scaled_y,
       width = 7, height = 7, dpi = 300)

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_gfcm_parma_ei06_si03_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei06_si03_scaled_y,
       width = 7, height = 7, dpi = 300)











