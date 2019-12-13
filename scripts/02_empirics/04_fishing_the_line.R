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

night_lights_table <- 
  bq_table(project = ucsb_project,
           dataset = "ocean_halos_v2",
           table = "gridded_night_lights_by_year_dist_to_mpa")

# Download the data (and export it)
effort_data <- bigrquery::bq_table_download(x = effort_table)
write.csv(x = effort_data,
          file = here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.csv"),
          row.names = F)

night_lights_data <- bigrquery::bq_table_download(x = night_lights_table)
write.csv(x = night_lights_data,
          file = here("data", "gridded_night_lights_by_year_dist_to_mpa.csv"),
          row.names = F)

# Modify the data for plotting purposes
# We will calculate the AVERAGE fishing hours
# for each 5 Km increments.

increment <- 5 # Increment, in kilometers

fishing_in_5k_increments <- effort_data %>% 
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  group_by(best_vessel_class, year, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  mutate(vessel_class = case_when(
    best_vessel_class == "drifting_longlines" ~ "Drifting longlines",
    best_vessel_class == "trawlers" ~ "Trawlers",
    best_vessel_class == "tuna_purse_seines" ~ "Tuna purse seines"
    ),
    year = as.factor(year))

night_lights_in_10k_increments <- night_lights_data %>% 
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  mutate(dist = round(distance / (10 * 1e3)) * 10) %>%  # Mutate the distance to group by a common bin
  group_by(year, dist) %>%                                            # Define grouping variables
  summarize(rad = sum(sum_rad, na.rm = T)) %>%                        # Calculate total
  ungroup() %>% 
  mutate(year = as.factor(year))

# Create figure
fishing_the_line_plot <- 
  ggplot(data = fishing_in_5k_increments,
         aes(x = dist, y = fishing, color = year)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(aes(fill = year), color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~vessel_class, scales = "free_y", ncol = 1) +
  plot_theme() +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) +
  guides(fill = guide_legend("Year")) +
  labs(x = "Distance from border (Km)",
       y = "Fishing effort (mean fishing hours per bin)")

fishing_the_line_night_lights_plot <- 
  ggplot(data = night_lights_in_10k_increments,
       aes(x = dist, y = rad / 1e4, color = year)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(aes(fill = year), color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme() +
  guides(fill = guide_legend(title = "Year")) +
  labs(x = "Distance from border (Km)",
       y = "Total radiance (x1e4 nW/cm2/sr)")


# Export figure
lazy_ggsave(plot = fishing_the_line_plot,
            filename = "fishing_the_line_plot",
            height = 7, width = 3.5)

lazy_ggsave(plot = fishing_the_line_night_lights_plot,
            filename = "fishing_the_line_night_lights_plot",
            height = 2.5, width = 3.5)

# END OF SCRIPT 

