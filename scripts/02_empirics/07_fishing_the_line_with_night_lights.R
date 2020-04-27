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

# Load helper functions
source(here("scripts", "00_helpers.R"))

# Load data
night_lights_data <- read.csv(here("data", "gridded_night_lights_by_year_dist_to_mpa.csv"),
                              stringsAsFactors = F)


increment <- 5

night_lights_in_10k_increments <- night_lights_data %>%
  filter(between(distance, -100e3, 100e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>%
  mutate(dist = round(distance / (10 * 1e3)) * 10) %>%  # Mutate the distance to group by a common bin
  filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year > 2016) %>% 
  filter(wdpaid %in% c(309888, 555629385, 400011,	220201, 11753)) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(name = case_when(wdpaid == 309888 ~ "PIPA",
                          wdpaid == 555629385 ~ "Revillagigedo",
                          wdpaid == 400011 ~ "PRINMS",
                          wdpaid == 220201 ~ "PNMS",
                          wdpaid == 11753 ~ "Galapagos"),
         name = fct_relevel(name, c("Galapagos", "Revillagigedo"), after = Inf)) %>% 
  group_by(name, dist) %>%                                            # Define grouping variables
  summarize(rad = mean(sum_rad, na.rm = T)) %>%                        # Calculate total
  ungroup() %>% 
  filter(rad < quantile(rad, 0.90))


fishing_the_line_night_lights_plot <- 
  ggplot(data = night_lights_in_10k_increments,
         aes(x = dist, y = rad / 1e4)) +
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(fill = "red", color = "black", shape = 21, size = 2) +
  facet_wrap(~name, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme() +
  guides(fill = guide_legend(title = "Year")) +
  labs(x = "Distance from border (Km)",
       y = "Total radiance (x1e4 nW/cm2/sr)")

lazy_ggsave(plot = fishing_the_line_night_lights_plot,
            filename = "fishing_the_line_night_lights_plot",
            height = 2.5, width = 3.5)

