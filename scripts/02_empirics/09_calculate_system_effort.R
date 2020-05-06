############################################################################
#####                   02_calculate_system_effort                  #####
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
system <- 
  st_read(here("data", "mpa_systems.gpkg"),
          stringsAsFactors = F) %>% 
  filter(!id == "EEZ")

sys_bbox <- st_bbox(system)

# Effort data
effort_data <- 
  readRDS(here("data", "gridded_effort_by_gear_and_year_dist_to_mpa.rds")) %>% 
  filter(!(year <= 2017 & wdpaid == 555629385))

# Night lights data
night_data <- 
  readRDS(here("data", "gridded_night_lights_by_year_dist_to_mpa.rds")) %>% 
  filter(!(year <= 2017 & wdpaid == 555629385))

# Filter to keep only data within the bounding box
effort <- 
  effort_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(fishing_days = mean(fishing_hours, na.rm = T) / 24)

lights <- night_data %>% 
  filter(between(lon, sys_bbox[1] - 1, sys_bbox[3] + 1),
         between(lat, sys_bbox[2] - 1, sys_bbox[4] + 1)) %>% 
  group_by(lon, lat) %>%
  summarize(radiance = mean(sum_rad, na.rm = T))

effort <- effort %>% 
  full_join(lights, by = c("lon", "lat"))

effort_plot <- effort %>% 
  drop_na(fishing_days) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = fishing_days)) +
  geom_sf(data = system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Fishing days (2016 - 2019)")

lights_plot <- effort %>% 
  drop_na(radiance) %>% 
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = radiance)) +
  geom_sf(data = system, aes(color = id), fill = "transparent") +
  scale_fill_viridis_c() +
  scale_color_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Radiance (2016 - 2019)")

plot_grid(effort_plot, lights_plot, ncol = 1)

effort_raster <- 
  rasterFromXYZ(effort, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

extracted_values <- raster::extract(effort_raster, system,
                                    method = "simple",
                                    fun = sum,
                                    na.rm = T,
                                    df = T)

data <- system %>% 
  mutate(fishing_days = extracted_values$fishing_days,
         radiance = extracted_values$radiance) %>% 
  group_by(wdpaid) %>% 
  mutate(tot_fishing = sum(fishing_days),
         tot_rad = sum(radiance)) %>% 
  ungroup() %>% 
  mutate(fishing_days = fishing_days / tot_fishing,
         radiance = radiance / tot_rad) %>% 
  select(-contains("tot"))

data %>% 
  mutate_if(is.numeric, log10) %>% 
  ggplot(aes(x = radiance, y = fishing_days, color = where)) +
  geom_point()


data %>% 
  st_drop_geometry() %>%
  gather(source, value, -c(wdpaid, id, iso3, area, fraction_as_reserve, where)) %>% 
  ggplot(aes(x = id, y = value, fill = where)) +
  geom_col(color = "black") +
  facet_wrap(iso3~source, scales = "free_y", ncol = 2) +
  labs(x = "Polygon", y = "Total effort radiance") +
  scale_fill_brewer(palette = "Set1") +
  plot_theme() +
  ggtitle("Two measres of effort")






















