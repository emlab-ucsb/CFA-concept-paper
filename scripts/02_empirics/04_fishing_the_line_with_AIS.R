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
  # filter(best_vessel_class == "drifting_longlines") %>%
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(iucn_cat = case_when(iucn_cat %in% c(1, 2) ~ "I - II",
                              iucn_cat %in% c(3, 4) ~ "III - IV",
                              T ~ "Others (IV - VI)")) %>% 
  group_by(best_vessel_class, iucn_cat, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T),
            sd = sd(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  mutate(inside = dist <= 0) %>%
  ungroup()

# Create figure
fishing_the_line_plot <- 
  ggplot(data = fishing_in_5k_increments,
         aes(x = dist, y = fishing,
             fill = best_vessel_class)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap( ~ iucn_cat) +
  plot_theme() +
  guides(fill = guide_legend("Gear")) +
  labs(x = "Distance from border (Km)",
       y = "Mean fishing effort (hours)")

# Export figure
lazy_ggsave(plot = fishing_the_line_plot,
            filename = "fishing_the_line_plot",
            height = 3, width = 6)

# END OF SCRIPT 



# Cherry picking
fishing_the_line_select_plot <- effort_data %>% 
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  filter(wdpaid %in% c(309888, 555629385, 400011,	220201, 11753)) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(name = case_when(wdpaid == 309888 ~ "PIPA",
                          wdpaid == 555629385 ~ "Revillagigedo",
                          wdpaid == 400011 ~ "PRINMS",
                          wdpaid == 220201 ~ "PNMS",
                          wdpaid == 11753 ~ "Galapagos"),
         name = fct_relevel(name, c("Galapagos", "Revillagigedo"), after = Inf),
         best_vessel_class = str_replace_all(best_vessel_class, "_", " ")) %>% 
  group_by(best_vessel_class, name, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T),
            sd = sd(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  group_by(best_vessel_class, name) %>% 
  mutate(n = n()) %>%  
  ungroup() %>% 
  filter(n > 10) %>% 
  mutate(inside = dist <= 0) %>% 
  ggplot(aes(x = dist, y = fishing,
             fill = best_vessel_class)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~ name, scales = "free_y") +
  plot_theme() +
  theme(legend.justification = c(0, 1),
        legend.position = c(2/3, 0.45)) +
  guides(fill = guide_legend(title = "Gear")) +
  labs(x = "Distance from border (Km)",
       y = "Mean fishing effort (hours)")

lazy_ggsave(plot = fishing_the_line_select_plot,
            filename = "fishing_the_line_select_plot",
            height = 3, width = 6)















