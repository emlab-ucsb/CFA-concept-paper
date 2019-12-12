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
library(tidyverse)

## Load helper functions
source(here("scripts", "00_helpers.R"))

ucsb_project <- "emlab-gcp" # Name of our project

# Establish a connection to the data
table <- bq_table(project = ucsb_project,
                  dataset = "ocean_halos_v2",
                  table = "gridded_effort_by_gear_and_year_dist_to_mpa")

# Download the data (and export it)
data <- bigrquery::bq_table_download(x = table)
write.csv(x = data,
          file = here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.csv"),
          row.names = F)

# Modify the data for plotting purposes
# We will calculate the AVERAGE fishing hours
# for each 5 Km increments.

increment <- 5 # Increment, in kilometers

fishing_in_5k_increments <- data %>% 
  filter(between(distance, -150e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  group_by(best_vessel_class, year, dist) %>%                         # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  mutate(vessel_class = case_when(
    best_vessel_class == "drifting_longlines" ~ "Drifting longlines",
    best_vessel_class == "trawlers" ~ "Trawlers",
    best_vessel_class == "tuna_purase_seines" ~ "Tuna purse seines")
  )

# Create figure
fishing_the_line_plot <- 
  ggplot(data = fishing_in_5k_increments,
         aes(x = dist, y = fishing, color = year)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(aes(fill = year), color = "black", shape = 21, size = 2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~best_vessel_class, scales = "free_y", ncol = 1) +
  plot_theme() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1.05)) +
  guides(fill = guide_colorbar(title = "Year",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Distance from border (Km)",
       y = "Fishing effort (mean fishing hours per bin)")

# Export figure
lazy_ggsave(plot = fishing_the_line_plot,
            filename = "fishing_the_line_plot",
            height = 7, width = 3.5)

# END OF SCRIPT 

