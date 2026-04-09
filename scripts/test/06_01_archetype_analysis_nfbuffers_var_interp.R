################################################################################
# SCRIPT TO GENERATE THE "BAR CODE" PLOTS NATIONAL FOREST LEVEL SGFCM CLUSTERS##                 ##
# 1. Load the data                                                            ##                                   ##
# 2. Format the data for creating the bar code plots                          ##
# 3. Create bar plots with the mean, median, and standard deviation of values ##
# 4. Create bar plots where the IQR does not overlap with 0                   ##
# 4.1 Write a function to calculate the 25th and 75th percentiles and check   ##
#     if the IQR range overlaps 0                                             ##
# 4.2 Format the data for creating the bar plots
# 3. Extract and plot the raster of the cluster groups                        ##
#  3.1 Save the cluster groups raster                                         ##
# NOTE TO SELF: INSTEAD OF OSTROM SES NEED TO RENAME TO VARIABLES TO IAD CLASS##
################################################################################

# 0. Load the required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)
library(geocmeans)
library(RColorBrewer)
library(viridis)
library(ggpattern)
library(distributional)
library(ggdist)
library(ggsci)
library(tigris)
library(forcats)

# 1. Load the data
#-------------------------------------------------------------------------------
## Load the attribute data (unscaled and scaled)
nfbuffers_data <- rast(here::here("data/processed/nf_buffers_all_attributes_2025-11-06.tif"))
nfbuffers_data_sc <- rast(here::here("data/processed/nf_buffers_all_attributes_cropped_then_scaled_2025-11-06.tif"))

## Load the resulting cluster raster
nfbuffer_sgfcm_result <- rast(here::here("outputs/SGFCM_nf_buffers_k27_2026-03-25.tif"))

plot(nfbuffer_sgfcm_result)

## Load the final nation forest level model
nfbuffer_sgfcm_result_mod <- readRDS(here::here("outputs/SGFCM_nf_buffers_k27_2026-03-25.rds"))


# 2. Format the data for creating the bar code plots (variable interpretation plots)
#-------------------------------------------------------------------------------
data <- as.data.frame(nfbuffers_data_sc)
#data <- as.data.frame(scale(nfbuffers_data))
data$groups <- nfbuffer_sgfcm_result_mod$Groups

data$groups <- gsub('V', 'A', data$groups)

## create a long form data frame
long_df <- data %>%
  pivot_longer(!groups, names_to = "var_name")

## get the mean, standard deviation, and median values
means_long <- data %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "mean")

sd_long <- data %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "sd")

med_long <- data %>%
  group_by(groups) %>%
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  pivot_longer(!groups, names_to = "var_name", values_to = "median")

## Combine into one data frame with the aggregate values for each group (cluster)
df_long <- left_join(means_long, sd_long)
df_long <- left_join(df_long, med_long)


## reorder the variables
long_reorder <- df_long %>% 
  mutate(var_name = fct_relevel(var_name, 
                            "treecov", "forprod", "tempseas", 
                            "precseas", "rough", "whp", 
                            "forgain", "distcrit", "distwild",
                            "pm25", "fedrich", 
                            "treeage", "pct_forpay", "pct_delmill",
                            "netmig", "comm_cap", "aip", 
                            "travtime", "hsbrd", "engbrd", "lesshs"))

long_reorder <- long_reorder %>%
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))

# 3. Create bar plots with the mean, standard deviation, and medians
#-------------------------------------------------------------------------------
var_interp <- ggplot(long_reorder, aes(x = var_name, y = mean, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

var_interp
#ggsave(paste0("~/Analysis/SES_Forest_Archetypes/figures/sgfcm_nfbuffers_var_interp_", Sys.Date(), ".png"), 
#       plot = var_interp, width = 12, height = 8, dpi = 300) 

var_interp_sd <- ggplot(long_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

var_interp_sd
#ggsave(paste0("~/Analysis/SES_Forest_Archetypes/figures/sgfcm_nfbuffers_var_interp_sd_", Sys.Date(), ".png"), 
#       plot = var_interp_sd, width = 12, height = 8, dpi = 300) 

var_interp_med <- ggplot(long_reorder, aes(x = var_name, y = median, fill = ostrom)) +
  geom_col() +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

var_interp_med
#ggsave(paste0("~/Analysis/SES_Forest_Archetypes/figures/sgfcm_nfbuffers_var_interp_med_", Sys.Date(), ".png"), 
#       plot = var_interp_med, width = 12, height = 8, dpi = 300)


# 4. Create bar plots of the variables where IQR != 0
#-------------------------------------------------------------------------------
## 4.1
## Create a function that calculates the 25th and 75th percentiles and checks if
## the IQR range overlaps with 0
check_iqr_overlap <- function(x) {
  # Calculate the 25th and 75th percentiles
  lower <- quantile(x, 0.25, na.rm = TRUE)
  upper <- quantile(x, 0.75, na.rm = TRUE)
  
  # Check if the IQR range overlaps 0
  return(lower <= 0 & upper >= 0)
}

## Create a dataframe of the overlapping variables 
overlap <- long_df %>% 
  group_by(groups, var_name) %>% 
  summarise(overlap = check_iqr_overlap(value), .groups="drop")

## 4.2
## Reformat the data to create the bar plots
long_join <- long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == FALSE)

## reorder the variables
long_overlap_reorder <- long_join %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                "distcrit", 
                                "distwild",
                                "pm25", "fedrich", 
                                "treeage", "pct_forpay", "pct_delmill",
                                "netmig", "comm_cap", "aip",
                                "travtime", "hsbrd", "engbrd",
                                "lesshs"
                                ))
## Reclassify the variables
long_overlap_reorder <- long_overlap_reorder %>% # need to remove dist to critical habitat and less hs
  mutate(ostrom = case_when(var_name == "treecov" | var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distcrit" | var_name == "distwild" | var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "pm25" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "pct_delmill" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))

# 5. Create bar plots with no overlapping IQR 
#-------------------------------------------------------------------------------
iqr_no_overlap <- ggplot(data=long_overlap_reorder, mapping = aes(x=var_name, y=value, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups))

iqr_no_overlap
#ggsave(paste0("~/Analysis/SES_Forest_Archetypes/outputs/sgfcm_nfbuffer_k27_var_interp_iqr_no_overlap_", Sys.Date(), ".png"), 
#       plot = iqr_no_overlap, width = 12, height = 8, dpi = 300)

# 6. Create bar plots of the standard deviation for the values that do overlap 
#    with 0
#-------------------------------------------------------------------------------


long_join_overlap_true <- long_df %>% 
  left_join(overlap) %>% 
  filter(overlap == TRUE)

sd_overlap <- long_join_overlap_true %>% 
  group_by(groups, var_name) %>% 
  summarise(sd = sd(value), .groups="drop")

# reorder the variables
sd_overlap_reorder <- sd_overlap %>% 
  mutate(var_name = fct_relevel(var_name, 
                                "treecov", 
                                "forprod", "tempseas", 
                                "precseas", "rough", "whp", 
                                "forgain", 
                                "distcrit", 
                                "distwild",
                                "pm25", 
                                "fedrich", 
                                "treeage", "pct_forpay", 
                                "pct_delmill",
                                "netmig", "comm_cap", "aip", 
                                "travtime", "hsbrd", "engbrd", "lesshs"))

sd_long_overlap_reorder <- sd_overlap_reorder %>% # need to remove treecover, pm25, and pct_delmill
  mutate(ostrom = case_when(var_name == "forprod" | var_name == "tempseas" | var_name == "precseas" | var_name == "rough" | var_name == "whp" ~ "resource system",
                            var_name == "distwild" | var_name == "distcrit" |  var_name == "forgain" ~ "resource unit",
                            var_name == "fedrich" | var_name == "treeage" | var_name == "pct_forpay" | var_name == "netmig" | var_name == "comm_cap" | var_name == "aip" | var_name == "travtime" | var_name == "hsbrd" | var_name == "engbrd" | var_name == "lesshs" ~ "users"))

# 7. Plot the SD for variables with IQRs that do overlap with 0
#-------------------------------------------------------------------------------
sd_overlap <- ggplot(sd_long_overlap_reorder, aes(x = var_name, y = sd, fill = ostrom)) +
  geom_col() +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  facet_wrap(~groups) +
  theme(text = element_text(size = 20),
        legend.position = "right", 
        axis.title.y = element_blank()) 

sd_overlap
#ggsave(paste0("~/Analysis/Archetype_Analysis/figures/sgfcm_all_k6_var_interp_sd_overlap_", Sys.Date(), ".png"), 
#       plot = k6_sd_overlap, width = 12, height = 8, dpi = 300)


ggplot(data=long_overlap_reorder, mapping = aes(x=var_name, y=sd, fill=ostrom)) +
  geom_boxplot(outliers = FALSE, coef=0) +
  geom_hline(yintercept = 0, linetype=2) +
  #scale_fill_manual(values=c15)+
  coord_flip()+
  theme_bw()+
  facet_wrap(vars(groups_k6))