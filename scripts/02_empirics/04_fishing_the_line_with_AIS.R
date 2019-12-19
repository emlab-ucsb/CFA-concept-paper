####################################################################################
#####                              fishing the line                             ####
####################################################################################
#
# This script download the gridded effort table created with the SQL script
# and then produces a plot of "fishing the line" effect.
# 
####################################################################################
## SETUP
# Load packages
library(here)
library(bigrquery)
library(cowplot)
library(tidyverse)

## Load helper functions
source(here("scripts", "00_helpers.R"))

ucsb_project <- "emlab-gcp" # Name of our project

# Establish a connection to the data
effort_table <-
  bq_table(project = ucsb_project,
           dataset = "ocean_halos_v2",
           table = "gridded_effort_by_gear_and_year_dist_to_mpa")

# Download the data (and export it)
effort_data <- bigrquery::bq_table_download(x = effort_table)
write.csv(x = effort_data,
          file = here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.csv"),
          row.names = F)

# Modify the data for plotting purposes
# We will calculate the AVERAGE fishing hours
# for each 5 Km increments.

increment <- 5 # Increment, in kilometers

fishing_in_5k_increments <- 
  effort_data %>% 
  filter(!wdpaid %in% c(555556875)) %>%
  filter(best_vessel_class == "drifting_longlines") %>%
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(iucn_cat = case_when(iucn_cat %in% c(1, 2) ~ "I - II",
                              iucn_cat %in% c(3, 4) ~ "II - III",
                              T ~ "Others (IV - VI)")) %>% 
  group_by(iucn_cat, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T),
            sd = sd(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  mutate(inside = dist <= 0) %>%
  ungroup()

# Create figure
fishing_the_line_plot <- 
  ggplot(data = fishing_in_5k_increments,
         aes(x = dist, y = fishing)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(fill = "steelblue", color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap( ~ iucn_cat) +
  plot_theme() +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) +
  guides(fill = guide_legend("Year")) +
  labs(x = "Distance from border (Km)",
       y = "Mean fishing effort (hours)")

fishing_the_line_plot


# Export figure
lazy_ggsave(plot = fishing_the_line_plot,
            filename = "fishing_the_line_plot",
            height = 3, width = 6)

# END OF SCRIPT 



# Cherry picking
effort_data %>% 
  filter(!wdpaid %in% c(555556875)) %>%
  filter(best_vessel_class %in% c("tuna_purse_seines", "drifting_longlines")) %>%
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(iucn_cat = case_when(wdpaid %in% c(309888, 400011, 	220201) ~ "PIPA, PRINMS, PNMS",
                              wdpaid %in% c(555629385, 11753) ~ "Galapagos, Revillagigedo",
                              T ~ "Others (IV - VI)")) %>% 
  filter(! iucn_cat == "Others (IV - VI)") %>% 
  group_by(iucn_cat, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T),
            sd = sd(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  mutate(iucn_cat_2 = case_when(iucn_cat == 1 ~ "Ia",
                                iucn_cat == 2 ~ "II",
                                iucn_cat == 3 ~ "III",
                                iucn_cat == 4 ~ "IV",
                                iucn_cat == 5 ~ "V",
                                iucn_cat == 6 ~ "VI", 
                                iucn_cat == 7 ~ "Others"),
         inside = dist <= 0) %>% 
  ungroup() %>% 
  ggplot(aes(x = dist, y = fishing)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(fill = "steelblue", color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  facet_grid( ~ iucn_cat, scales = "free_y") +
  plot_theme() +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) +
  guides(fill = guide_legend("Year")) +
  labs(x = "Distance from border (Km)",
       y = "Mean fishing effort (hours)")
















