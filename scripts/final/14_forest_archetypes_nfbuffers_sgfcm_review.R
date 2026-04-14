################################################################################
# REVIEW FUZYY C-MEANS PARAMETER SELECTION RESULTS FROM NF LEVEL SPATIAL DATA ##
# 1. Download and combine all nfbuffers_sgfcm_*.csv results                    ##
# 2. Filter the results for explained inertia and silhouette index values     ##
# 3. Plot the evaluation metrics with k on x and a facet grid of alpha and win##
# 4. Save the figures                                                         ##
# 5. Create a plot with 2 y-axis                                              ##
#  5.1 Create a function to help with plotting                                ##
#  5.2 Using the function in 5.1 create the different secondary axis          ##
#  5.3 Generate the 2 y-axis plots                                            ##
# 6. Save the plots with the dual y-axis                                      ##
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

# 3. Plot Explained Inertia and Silhouette Index y with k 
# on x and facet wrapped by m
#-------------------------------------------------------------------------------

df <- df_ei05

ei <- df %>% 
  ggplot(aes(k, Explained.inertia)) + 
  geom_point(size = 0.5, color="red") +
  facet_grid(rows = vars(alpha), cols = vars(beta)) +
  #facet_wrap(~k) +
  labs(title = "SFCM Evaluation Metrics: Explained Inertia >= 0.5")
      # \n & Silhouette Index >= 0.3")

si <-  
  ggplot(df, aes(k, Silhouette.index)) + 
  geom_point(size = 0.5, color="blue") +
  facet_grid(rows = vars(alpha), cols = vars(beta))
  #facet_wrap(~k)
  
ei
si
ei / si

# 4. Save the figure
#-------------------------------------------------------------------------------

sgfmc_plot <- ei / si 

ggsave(here::here(paste0("outputs/nf_level/plots/nf_buffers_sgfcm_params_w1_ei05_",
                         Sys.Date(), ".jpeg")), plot = sgfmc_plot,
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
    scales::rescale(x, from = from, to = to)
  }
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    scales::rescale(x, from = to, to = from)
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
  facet_grid(rows = vars(alpha), cols = vars(beta)) +
  labs(title = "National Level SGFCM 3x3 window: \nExplained Inertia >= 0.5")
ei05_si_scaled_y

ei05_si03_scaled_y <- ggplot(df_ei05_si03, aes(k)) +
  geom_point(aes(y = Explained.inertia), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = sec2$fwd(Silhouette.index)), colour = "red", alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~sec2$rev(.), name = "Silhouette.index")) + 
  facet_grid(rows = vars(alpha), cols = vars(m)) +
  labs(title = "National Level SGFCM 3x3 window: \nExplained Inertia >= 0.5 & Silhouette Index >= 0.3")
ei05_si03_scaled_y

ei06_si03_scaled_y <- ggplot(df_ei06_si03, aes(k)) +
  geom_point(aes(y = Explained.inertia), colour = "blue", alpha = 0.5) +
  geom_point(aes(y = sec3$fwd(Silhouette.index)), colour = "red", alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~sec3$rev(.), name = "Silhouette.index")) + 
  facet_grid(rows = vars(alpha), cols = vars(m)) +
  labs(title = "National Level SGFCM: \nExplained Inertia >= 0.6 & Silhouette Index >= 0.3")
ei06_si03_scaled_y

# 6. Save the figures with the dual y-axis
#-------------------------------------------------------------------------------

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_sgfcm_parma_w1_ei05_si_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei05_si_scaled_y,
       width = 10, height = 8, dpi = 300)

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_sfcm_parma_w3_ei05_si03_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei05_si03_scaled_y,
       width = 7, height = 7, dpi = 300)

ggsave(here::here(paste0("outputs/nf_level/plots/nf_level_sfcm_parma_w3_ei06_si03_scaled_y_",
                         Sys.Date(), ".jpeg")), plot = ei06_si03_scaled_y,
       width = 7, height = 7, dpi = 300)











