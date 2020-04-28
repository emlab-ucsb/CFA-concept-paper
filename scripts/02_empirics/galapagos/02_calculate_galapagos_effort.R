############################################################################
#####                   02_calculate_galapagos_effort                  #####
############################################################################
#
# 
############################################################################

## SETUP
# Load packages
library(here)
library(cowplot)
library(raster)
library(sf)
library(tidyverse)

# Load helper functions
source(here("scripts", "00_helpers.R"))

## Load data
# System polygons
gal_system <- 
  st_read(here("data", "galapagos_tuna_polygons.gpkg")) %>% 
  st_transform(4326)

sys_bbox <- st_bbox(gal_system)

# Effort data
effort_data <- 
  readRDS(here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.rds"))

# Night lights data
night_data <- 
  readRDS(here("data", "gridded_night_lights_by_year_dist_to_mpa.rds"))

# Filter to keep only data within the bounding box
gal_effort <- 
  effort_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(fishing_hours = mean(fishing_hours, na.rm = T))

gal_lights <- night_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(radiance = mean(sum_rad, na.rm = T))

effort <- gal_effort %>% 
  full_join(gal_lights, by = c("lon", "lat"))

effort_plot <- effort %>% 
  drop_na(fishing_hours) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_hours)) +
  geom_sf(data = gal_system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Fishing Hours (2016 - 2019)")

lights_plot <- effort %>% 
  drop_na(radiance) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = radiance)) +
  geom_sf(data = gal_system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Radiance (2016 - 2019)")

plot_grid(effort_plot, lights_plot)

gal_effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

extracted_values <- raster::extract(gal_effort_raster, gal_system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T, df = T)

data <- gal_system %>% 
  mutate(fishing_hours = extracted_values$fishing_hours,
         radiance = extracted_values$radiance)

data %>% 
  st_drop_geometry() %>% 
  gather(source, value, -c(id, area, fraction_as_reserve, where)) %>% 
  filter(!id == "Ecuadorian EEZ") %>% 
  ggplot(aes(x = id, y = value, fill = where)) +
  geom_col(color = "black") +
  facet_wrap(~source, scales = "free", ncol = 1) +
  labs(x = "Polygon", y = "Total effort radiance") +
  scale_fill_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Two measres of effort") +
  labs(subtitle = "Galapagos MPA contains ~2% of the fishing effort and 30% of radiance \nin the system (using HUD 95)")

























