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

fishing_in_5k_increments <- effort_data %>% 
  # filter(between(distance, -100e3, 150e3)) %>%                        # Keep only data in a 100 Km buffer from the line
  filter(year >= 2016) %>%
  filter(!(year <= 2017 & wdpaid == 555629385)) %>%
  filter(wdpaid %in% c(309888, 555629385, 400011, 11753)) %>% 
  mutate(dist = round(distance / (increment * 1e3)) * increment) %>%  # Mutate the distance to group by a common bin
  mutate(name = case_when(wdpaid == 309888 ~ "A) PIPA",
                          wdpaid == 555629385 ~ "C) Revillagigedo",
                          wdpaid == 400011 ~ "B) PRIMNM",
                          # wdpaid == 220201 ~ "A) PNMS",
                          wdpaid == 11753 ~ "D) Galapagos"),
         name = fct_relevel(name, c("A) PIPA", "C) Revillagigedo", "B) PRIMNM", "D) Galapagos")),
         best_vessel_class = str_to_sentence(str_replace_all(best_vessel_class, "_", " ")),
         best_vessel_class = fct_relevel(best_vessel_class, c("Tuna purse seines", "Drifting longlines"))) %>% 
  group_by(best_vessel_class, name, dist) %>%                  # Define grouping variables
  summarize(fishing = mean(fishing_hours, na.rm = T),
            sd = sd(fishing_hours, na.rm = T)) %>%             # Calculate average
  ungroup() %>% 
  group_by(best_vessel_class, name) %>% 
  mutate(n = n()) %>%  
  ungroup() %>% 
  filter(n > 10) %>% 
  mutate(inside = dist <= 0)


# ### --------------------------------------------------------------------
# # Plot 1: PIPA
# ### --------------------------------------------------------------------

pipa <- fishing_in_5k_increments %>%
  dplyr::filter(name == "A) PIPA") %>%
  ggplot()+
  aes(x = dist, y = fishing, fill = best_vessel_class)+
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  #facet_wrap(~ name, scales = "free_y", ncol = 2) +
  plot_theme() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Gear",
                             title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = "",
       y = "Mean fishing effort (h)")

# ### --------------------------------------------------------------------
# # Plot 2: PRIMNM
# ### --------------------------------------------------------------------

primnm <- fishing_in_5k_increments %>%
  dplyr::filter(name == "B) PRIMNM") %>%
  ggplot()+
  aes(x = dist, y = fishing, fill = best_vessel_class)+
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  #facet_wrap(~ name, scales = "free_y", ncol = 2) +
  plot_theme() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Gear",
                             title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = "Distance from border (km)",
       y = "Mean fishing effort (h)")

# ### --------------------------------------------------------------------
# # Plot 3: PIPA
# ### --------------------------------------------------------------------

rev <- fishing_in_5k_increments %>%
  dplyr::filter(name == "C) Revillagigedo") %>%
  ggplot()+
  aes(x = dist, y = fishing, fill = best_vessel_class)+
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  #facet_wrap(~ name, scales = "free_y", ncol = 2) +
  # facet_wrap(~ year) +
  plot_theme() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Gear",
                             title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = "",
       y = "",
       title = "")

# ### --------------------------------------------------------------------
# # Plot 4: Galapagos
# ### --------------------------------------------------------------------

galapagos <- fishing_in_5k_increments %>%
  dplyr::filter(name == "D) Galapagos") %>%
  ggplot()+
  aes(x = dist, y = fishing, fill = best_vessel_class)+
  geom_smooth(method = "loess", se = F, color = "black") +
  geom_point(color = "black", shape = 21, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA)) +
  #facet_wrap(~ name, scales = "free_y", ncol = 2) +
  plot_theme() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Gear",
                             title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = "Distance from border (km)",
       y = "")


# ### --------------------------------------------------------------------
# # Extract legend and combine
# ### --------------------------------------------------------------------

no_legend <- theme(legend.position = "none")
more_height <- theme(plot.margin = unit(c(5,1,1,1), "mm"))
even_more_height <- theme(plot.margin = unit(c(10,1,1,1), "mm"))

fig3_top_row <- plot_grid(pipa + no_legend + more_height,
                          rev + no_legend + more_height,
                          ncol = 2,
                          align = "vh",
                          labels = c("A) PIPA", "C) Revillagigedo"),
                          vjust = 1.5,
                          hjust = -0.1)

fig3_bottom_row <- plot_grid(primnm + no_legend + even_more_height,
                             galapagos + no_legend + even_more_height,
                             ncol = 2,
                             align = "vh",
                             labels = c("B) PRIMNM", "D) Galapagos"),
                             vjust = 1.5,
                             hjust = -0.1)

combine <- plot_grid(fig3_top_row,
                     fig3_bottom_row,
                     ncol = 1,
                     align = "hv")

legend <- get_legend(
  # create some space to the left of the legend
  pipa + theme(legend.box.margin = margin(5, 0, 5, 0))
)


fig3 <- plot_grid(legend,
                  combine,
                  ncol = 1,
                  rel_heights = c(0.1,1))

lazy_ggsave(plot = fig3,
            filename = "Figure3",
            height = 18, width = 18)















